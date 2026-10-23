;;; crates-test-helpers.el --- Shared helpers for crates.el tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Builds a database from the fixture dump in test/fixtures/db-dump with
;; the real update pipeline (curl on a file:// URL, tar, sqlite3).

;;; Code:

(require 'ert)
(require 'crates-db)

(defconst crates-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the tests.")

(defvar crates-test--db nil
  "Database built from the fixture dump, shared by the tests that need one.")

(defun crates-test-require (condition reason)
  "Skip the current test with REASON unless CONDITION is non-nil.
When the environment variable CRATES_TEST_STRICT is set, as it is in
CI, fail instead of skipping."
  (unless condition
    (if (getenv "CRATES_TEST_STRICT")
        (ert-fail (list "Missing test requirement" reason))
      (ert-skip reason))))

(defun crates-test-skip-unless-tools ()
  "Skip the current test unless the update pipeline can run."
  (crates-test-require (and (sqlite-available-p)
                            (executable-find crates-db-curl-program)
                            (executable-find crates-db-tar-program)
                            (executable-find crates-db-sqlite-program))
                       "sqlite support, curl, tar or sqlite3 is missing"))

(defun crates-test-skip-unless-grammar ()
  "Skip the current test unless the `toml' grammar is available."
  (crates-test-require (and (treesit-available-p) (treesit-language-available-p 'toml))
                       "the toml tree-sitter grammar is missing"))

(defun crates-test-build-db (&optional file)
  "Build the fixture dump into FILE (a fresh temporary file by default).
Wait for the asynchronous update and return its result."
  (let* ((work (make-temp-file "crates-test-" t))
         (tarball (expand-file-name "db-dump.tar.gz" work))
         (crates-db-file (or file (expand-file-name "crates.sqlite" work)))
         (crates-db-dump-url (concat "file://" tarball))
         (done 'pending))
    (unless (zerop (call-process crates-db-tar-program nil nil nil
                                 "-czf" tarball "-C"
                                 (expand-file-name "fixtures/db-dump" crates-test--dir)
                                 "."))
      (error "Could not create the fixture tarball"))
    (crates-db-close)
    (crates-db-update (lambda (ok) (setq done ok)))
    (with-timeout (60 (crates-db-cancel-update) (error "Database build timed out"))
      (while (eq done 'pending)
        (accept-process-output nil 0.05)))
    (crates-db-close)
    (list done crates-db-file)))

(defmacro crates-test-with-db (&rest body)
  "Run BODY with `crates-db-file' bound to the fixture database."
  (declare (indent 0))
  `(progn
     (crates-test-skip-unless-tools)
     (unless crates-test--db
       (pcase-let ((`(,ok ,file) (crates-test-build-db)))
         (unless ok (error "Building the fixture database failed"))
         (setq crates-test--db file)))
     (let ((crates-db-file crates-test--db))
       (crates-db-close)
       (unwind-protect (progn ,@body)
         (crates-db-close)))))

(provide 'crates-test-helpers)
;;; crates-test-helpers.el ends here
