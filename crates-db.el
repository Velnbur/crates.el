;;; crates-db.el --- Local SQLite copy of the crates.io index  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kyrylo Baibula

;; Author: Kyrylo Baibula <kyrylobaybula@gmail.com>
;; URL: https://github.com/Velnbur/crates.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Builds and queries a small SQLite database holding every crate and
;; version published on crates.io.
;;
;; The database is built from the official daily dump
;; <https://static.crates.io/db-dump.tar.gz> by external programs, run
;; asynchronously: `curl' streams the archive into `tar', which keeps
;; only the three CSV files needed, and the `sqlite3' shell imports
;; them and boils them down to two narrow tables.  The result replaces
;; `crates-db-file' atomically, so Emacs keeps answering queries from
;; the previous copy until the new one is complete.
;;
;; Emacs itself only reads the database, through the built-in
;; `sqlite-*' functions (Emacs 29+).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function sqlite-open "sqlite.c")
(declare-function sqlite-close "sqlite.c")
(declare-function sqlite-select "sqlite.c")
(declare-function sqlitep "sqlite.c")
(declare-function sqlite-available-p "sqlite.c")

(defgroup crates nil
  "Show crates.io versions in Cargo manifests."
  :group 'tools
  :prefix "crates-")

(defcustom crates-db-file (locate-user-emacs-file "crates/crates.sqlite")
  "Where the local crates.io database is stored."
  :type 'file)

(defcustom crates-db-dump-url "https://static.crates.io/db-dump.tar.gz"
  "URL of the crates.io database dump.
Anything `curl' understands works, including file:// URLs."
  :type 'string)

(defcustom crates-db-max-age 7
  "Days after which the local database counts as stale.
When `crates-db-auto-update' is non-nil, a stale or missing database
is rebuilt in the background.  nil means it never goes stale."
  :type '(choice (const :tag "Never" nil) (number :tag "Days")))

(defcustom crates-db-auto-update nil
  "Non-nil means rebuild a stale or missing database automatically.
A rebuild downloads the whole crates.io dump (about 2 GB) and needs a
few GB of free space in the directory named by the variable
`temporary-file-directory'."
  :type 'boolean)

(defcustom crates-db-curl-program "curl"
  "The curl executable."
  :type 'string)

(defcustom crates-db-tar-program "tar"
  "The tar executable.  Both GNU tar and bsdtar work."
  :type 'string)

(defcustom crates-db-sqlite-program "sqlite3"
  "The sqlite3 command-line shell, version 3.32 or newer."
  :type 'string)

(defvar crates-db-after-update-hook nil
  "Hook run after the database has been replaced by a new build.")

(defconst crates-db-schema-version "1"
  "Version of the database layout, stored in its `meta' table.")

(defconst crates-db--log-buffer "*crates-db-update*"
  "Name of the buffer that logs database updates.")

(defvar crates-db--connection nil
  "Open handle on `crates-db-file', or nil.")

(defvar crates-db--update nil
  "Plist describing the running update, or nil when none is running.")

;;;; Reading

(defun crates-db--open ()
  "Return a handle on the database, or nil if it does not exist yet."
  (unless (and crates-db--connection (sqlitep crates-db--connection))
    (setq crates-db--connection
          (and (sqlite-available-p)
               (file-exists-p crates-db-file)
               (sqlite-open crates-db-file))))
  crates-db--connection)

(defun crates-db-close ()
  "Close the database handle, if open."
  (when (and crates-db--connection (sqlitep crates-db--connection))
    (sqlite-close crates-db--connection))
  (setq crates-db--connection nil))

(defun crates-db-available-p ()
  "Non-nil if the database exists and can be queried."
  (and (crates-db--open) t))

(defun crates-db-crate (name)
  "Return (ID NAME DEFAULT-VERSION) for the crate called NAME, or nil.
Names match case-insensitively, as on crates.io; an exact match wins."
  (when-let* ((db (crates-db--open)))
    (car (sqlite-select
          db "SELECT id, name, default_version FROM crates
              WHERE name = ?1 COLLATE NOCASE ORDER BY name = ?1 DESC LIMIT 1"
          (list name)))))

(defun crates-db-versions (crate-id)
  "Return (NUM YANKED CREATED-AT) for every version of CRATE-ID.
YANKED is t or nil.  The order is unspecified."
  (when-let* ((db (crates-db--open)))
    (mapcar (lambda (row) (list (nth 0 row) (eql (nth 1 row) 1) (nth 2 row)))
            (sqlite-select
             db "SELECT num, yanked, created_at FROM versions WHERE crate_id = ?"
             (list crate-id)))))

(defun crates-db-crate-names (prefix &optional limit)
  "Return up to LIMIT (default 200) crate names starting with PREFIX."
  (when-let* ((db (crates-db--open)))
    (mapcar #'car
            (sqlite-select
             db "SELECT name FROM crates WHERE name LIKE ?1 ESCAPE '\\'
                 ORDER BY length(name), name LIMIT ?2"
             (list (concat (replace-regexp-in-string "[%_\\]" "\\\\\\&" prefix) "%")
                   (or limit 200))))))

(defun crates-db-meta (key)
  "Return the value stored under KEY in the database's `meta' table."
  (when-let* ((db (crates-db--open)))
    (caar (sqlite-select db "SELECT value FROM meta WHERE key = ?" (list key)))))

(defun crates-db-age ()
  "Return the age of the database in days, or nil if there is none."
  (when (file-exists-p crates-db-file)
    (/ (float-time (time-subtract
                    nil (file-attribute-modification-time
                         (file-attributes crates-db-file))))
       86400.0)))

(defun crates-db-stale-p ()
  "Non-nil if the database is missing or older than `crates-db-max-age'."
  (let ((age (crates-db-age)))
    (or (null age)
        (and crates-db-max-age (> age crates-db-max-age)))))

;;;; Building

(defun crates-db--log (format-string &rest args)
  "Append FORMAT-STRING formatted with ARGS to the update log."
  (with-current-buffer (get-buffer-create crates-db--log-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (format-time-string "[%T] ") (apply #'format format-string args) "\n"))))

(defun crates-db--gnu-tar-p ()
  "Non-nil if `crates-db-tar-program' is GNU tar."
  (ignore-errors
    (string-match-p "GNU tar" (car (process-lines crates-db-tar-program "--version")))))

(defun crates-db--fetch-command (dir)
  "Return the shell command that downloads the dump and unpacks it into DIR."
  (format "%s -fsSL --retry 3 %s | %s -xzf - -C %s %s"
          (shell-quote-argument crates-db-curl-program)
          (shell-quote-argument crates-db-dump-url)
          (shell-quote-argument crates-db-tar-program)
          (shell-quote-argument dir)
          (mapconcat #'shell-quote-argument
                     (append (and (crates-db--gnu-tar-p) '("--wildcards"))
                             '("*/metadata.json" "*/data/crates.csv"
                               "*/data/versions.csv" "*/data/default_versions.csv"))
                     " ")))

(defun crates-db--sql-string (string)
  "Quote STRING as an SQL literal."
  (concat "'" (string-replace "'" "''" string) "'"))

(defun crates-db--build-script (data-dir timestamp)
  "Return the sqlite3 script importing the CSV files in DATA-DIR.
TIMESTAMP is the dump's creation time, recorded in the `meta' table."
  (let ((csv (lambda (name)
               (crates-db--sql-string (expand-file-name name data-dir)))))
    (concat
     "PRAGMA journal_mode = OFF;\n"
     "PRAGMA synchronous = OFF;\n"
     ;; Versions first: the crates table looks up default versions in it.
     ".import --csv " (funcall csv "versions.csv") " s_versions\n"
     "CREATE TABLE versions(id INTEGER PRIMARY KEY, crate_id INTEGER NOT NULL,
  num TEXT NOT NULL, yanked INTEGER NOT NULL, created_at TEXT);
INSERT INTO versions SELECT CAST(id AS INTEGER), CAST(crate_id AS INTEGER), num,
  lower(yanked) IN ('t', 'true', '1'), created_at FROM s_versions;
DROP TABLE s_versions;
CREATE INDEX versions_crate ON versions(crate_id);\n"
     ".import --csv " (funcall csv "default_versions.csv") " s_default\n"
     "CREATE TABLE default_versions(crate_id INTEGER PRIMARY KEY, version_id INTEGER);
INSERT INTO default_versions SELECT CAST(crate_id AS INTEGER), CAST(version_id AS INTEGER)
  FROM s_default;
DROP TABLE s_default;\n"
     ".import --csv " (funcall csv "crates.csv") " s_crates\n"
     "CREATE TABLE crates(id INTEGER PRIMARY KEY, name TEXT NOT NULL, default_version TEXT);
INSERT INTO crates SELECT CAST(c.id AS INTEGER), c.name, v.num FROM s_crates c
  LEFT JOIN default_versions d ON d.crate_id = CAST(c.id AS INTEGER)
  LEFT JOIN versions v ON v.id = d.version_id;
DROP TABLE s_crates;
DROP TABLE default_versions;
CREATE INDEX crates_name ON crates(name COLLATE NOCASE);
CREATE TABLE meta(key TEXT PRIMARY KEY, value TEXT);\n"
     (format "INSERT INTO meta VALUES ('schema', %s), ('dump_timestamp', %s), ('built_at', %s);\n"
             (crates-db--sql-string crates-db-schema-version)
             (crates-db--sql-string (or timestamp ""))
             (crates-db--sql-string (format-time-string "%FT%TZ" nil t)))
     "VACUUM;\n")))

(defun crates-db--finish (ok message)
  "End the running update, which succeeded if OK; log MESSAGE."
  (let* ((update crates-db--update)
         (tmp (plist-get update :tmp))
         (callback (plist-get update :callback)))
    (setq crates-db--update nil)
    (when (and tmp (file-directory-p tmp))
      (delete-directory tmp t))
    (crates-db--log "%s" message)
    (if ok
        (progn (message "crates: %s" message)
               (run-hooks 'crates-db-after-update-hook))
      (message "crates: database update failed: %s (see %s)" message crates-db--log-buffer))
    (when callback (funcall callback ok))))

(defun crates-db--run (name command stdin next)
  "Run shell COMMAND as step NAME of the update, then call NEXT.
STDIN, if non-nil, is sent to the process as its input."
  (crates-db--log "%s: %s" name command)
  (let ((proc (make-process
               :name (concat "crates-db-" name)
               :command (list shell-file-name shell-command-switch command)
               :buffer (get-buffer-create crates-db--log-buffer)
               :connection-type 'pipe
               :noquery t
               :sentinel
               (lambda (proc _event)
                 (unless (process-live-p proc)
                   (if (and (eq (process-status proc) 'exit)
                            (zerop (process-exit-status proc)))
                       (condition-case err
                           (funcall next)
                         (error (crates-db--finish nil (error-message-string err))))
                     (crates-db--finish
                      nil (format "%s exited with status %s"
                                  name (process-exit-status proc)))))))))
    (setq crates-db--update (plist-put crates-db--update :process proc))
    (when stdin
      (process-send-string proc stdin)
      (process-send-eof proc))))

(defun crates-db--import ()
  "Second update step: import the unpacked dump into a new database."
  (let* ((tmp (plist-get crates-db--update :tmp))
         (meta (car (directory-files-recursively tmp "\\`metadata\\.json\\'")))
         (data (and meta (expand-file-name "data" (file-name-directory meta))))
         (new (expand-file-name "new.sqlite" tmp)))
    (unless (and data (file-exists-p (expand-file-name "crates.csv" data)))
      (error "The dump did not contain the expected files"))
    (let ((timestamp (with-temp-buffer
                       (insert-file-contents meta)
                       (gethash "timestamp" (json-parse-buffer)))))
      (crates-db--run
       "import"
       (format "%s -bail %s" (shell-quote-argument crates-db-sqlite-program)
               (shell-quote-argument new))
       (crates-db--build-script data timestamp)
       (lambda () (crates-db--install new timestamp))))))

(defun crates-db--install (new timestamp)
  "Last update step: move NEW over `crates-db-file'.
TIMESTAMP is the dump's creation time."
  (crates-db-close)
  (make-directory (file-name-directory (expand-file-name crates-db-file)) t)
  (rename-file new crates-db-file t)
  (crates-db--finish t (format "database updated (dump of %s)" timestamp)))

;;;###autoload
(defun crates-db-update (&optional callback)
  "Rebuild the local crates.io database from the latest dump.
Runs in the background.  CALLBACK, if non-nil, is called with t on
success and nil on failure."
  (interactive)
  (if crates-db--update
      (progn (message "crates: a database update is already running")
             (when callback (funcall callback nil)))
    (dolist (program (list crates-db-curl-program crates-db-tar-program
                           crates-db-sqlite-program))
      (unless (executable-find program)
        (user-error "crates: `%s' is required to update the database" program)))
    (let ((tmp (make-temp-file "crates-db-" t)))
      (setq crates-db--update (list :tmp tmp :callback callback))
      (message "crates: updating database from %s..." crates-db-dump-url)
      (condition-case err
          (crates-db--run "fetch" (crates-db--fetch-command tmp) nil #'crates-db--import)
        (error (crates-db--finish nil (error-message-string err)))))))

(defun crates-db-update-running-p ()
  "Non-nil while a database update is in progress."
  (and crates-db--update t))

(defun crates-db-cancel-update ()
  "Stop the running database update."
  (interactive)
  (when-let* ((proc (plist-get crates-db--update :process)))
    (set-process-sentinel proc #'ignore)
    (delete-process proc))
  (when crates-db--update
    (crates-db--finish nil "cancelled")))

(defun crates-db-maybe-update ()
  "Start `crates-db-update' if the database is stale and that is wanted."
  (when (and crates-db-auto-update
             (not crates-db--update)
             (crates-db-stale-p))
    (crates-db-update)))

(provide 'crates-db)
;;; crates-db.el ends here
