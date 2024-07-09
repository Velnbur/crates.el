;;; crates-db.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Velnbur
;;
;; Author: Velnbur <kyrylobaybula@gmail.com>
;; Maintainer: Velnbur <kyrylobaybula@gmail.com>
;; Created: липня 08, 2024
;; Modified: липня 08, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/velnbur/crates-db
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Implements migrating from crates.io postgres dump to sqlite3 db and
;;  provides some queries to work with it.
;;
;; Requires `sqlite3' to be installed on the system for the migrations.
;;
;;; Code:

(require 'async)
(require 'xdg)
(require 'doom)
(require 'crates-download)

(defconst crates-db-path
  (concat
   (file-name-as-directory (xdg-state-home))
   "crates-io.sqlite3")
  "Variable to specify the base path for crates db.")

(defconst crates-db-table-csv-dump-alist
  '((crates   . "data/crates.csv")
    (versions . "data/versions.csv"))
  "Alist of tables and their corresponding csv dumps (path to them).")

;;; ---------------------------
;;; Migrate from dump functions
;;; ---------------------------

;;;###autoload
(defun crates-db-setup ()
  "Setup the crates db."
  (interactive)
  (async-start #'crates-db-setup-sync
               (lambda (result)
                 (message "Crates db setup done."))))

;;;###autoload
(defun crates-db-setup-sync ()
  (interactive)
  (crates-db--migrate-from-dump crates-download-dest-dir))

(defun crates-db--migrate-from-dump (dump-path &optional db-path)
  "Migrate required `.csv' fiels from `DUMP-PATH' onto the db at
   `DB-PATH'."
  (let* ((db-path (or db-path crates-db-path))
         (expanded-db-path (expand-file-name db-path))
         (sqlite-process (crates-db--start-sqlite-process expanded-db-path)))
    (make-empty-file dump-path t)
    (message "Migrating from %s to %s" dump-path expanded-db-path)
    (dolist (table-csv-dump crates-db-table-csv-dump-alist)
      (crates-db--migrate-table-from-dump sqlite-process
                                          (car table-csv-dump)
                                          (concat dump-path "/" (cdr table-csv-dump))))
    (process-send-string sqlite-process ".quit\n")))

(defun crates-db--start-sqlite-process (db-path)
  "Start a sqlite3 process with the db at `DB-PATH' ready for CSV
   import and return it."
  (let* ((expanded-db-path (expand-file-name db-path))
         (sqlite-process (start-process "crates.el-sqlite3" "*crates-sqlite*" "sqlite3" expanded-db-path)))
    (process-send-string sqlite-process ".mode csv\n")
    sqlite-process))

(defun crates-db--migrate-table-from-dump (sqlite-process table csv-dump-path)
  "Migrate `TABLE' from `CSV-DUMP-PATH' using opened `SQLITE-PROCESS'."
  (message "Migrating %s from %s" table csv-dump-path)
  (process-send-string sqlite-process
                       (format ".import %s %s\n" csv-dump-path table)))

;;; --------------------------
;;; DB queries
;;; --------------------------

(defun crates-db--ensure-db-con (con)
  "if `CON' is nil, open a new connection to the crates db and
   return it."
  (or con (sqlite-open crates-db-path)))

(defun crates-db--crate-id-from-name (crate-name &optional con)
  "Get the id of a crate with `CRATE-NAME'."
  (let* ((con (crates-db--ensure-db-con con)))
    (car (sqlite-select con "SELECT id FROM crates WHERE name = ? LIMIT 1" crate-name))))

(defun crates-db--propose-crates (prefix limit &optional con)
  "Propose crates with `PREFIX' and limit the result to `LIMIT'."
  (let ((con    (crates-db--ensure-db-con con))
        (prefix (concat prefix "%")))
    (mapcar #'car (sqlite-select con "SELECT name FROM crates WHERE name LIKE ? LIMIT ?" (list prefix limit)))))

(defun crates-db--available-versions-for-crate (crate-name limit &optional con version-str)
  "Select features for a crate with `CRATE-NAME' where versions
   string starts with `VERSION-STR'."
  (let* ((con      (crates-db--ensure-db-con con))
         (crate-id (crates-db--crate-id-from-name crate-name con)))
    (sqlite-select con "SELECT num FROM versions WHERE crate_id = ? LIMIT ?" (list crate-id limit))))

(provide 'crates-db)
;;; crates-db.el ends here
