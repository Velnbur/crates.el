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
  "Migrate required `.csv' fiels from `DUMP-PATH' onto the db at `DB-PATH'."
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
  "Start a sqlite3 process with the db at `DB-PATH' ready for CSV import and return it."
  (let* ((expanded-db-path (expand-file-name db-path))
         (sqlite-process (start-process "crates.el-sqlite3" "*crates-sqlite*" "sqlite3" expanded-db-path)))
    (process-send-string sqlite-process ".mode csv\n")
    sqlite-process))

(defun crates-db--migrate-table-from-dump (sqlite-process table csv-dump-path)
  "Migrate `TABLE' from `CSV-DUMP-PATH' using opened `SQLITE-PROCESS'."
  (message "Migrating %s from %s" table csv-dump-path)
  ;; Send command for importing csv dump into the raw table
  (process-send-string sqlite-process (format ".import %s %s_raw\n" csv-dump-path table)))

;;; --------------------------
;;; DB queries
;;; --------------------------

(defvar crates-db--con nil
  "Connection to the crates db.")

(defun crates-db--init-tables (&optional con)
  "Initialize tables in the db."
  (let ((con (crates-db--get-con)))
    (with-sqlite-transaction
     con
     (sqlite-execute con "
        CREATE TABLE IF NOT EXISTS crates (
                id INTEGER PRIMARY KEY,
                name TEXT UNIQUE
        )")
     (sqlite-execute con "
        CREATE TABLE IF NOT EXISTS versions (
                id INTEGER PRIMARY KEY,
                crate_id INTEGER,
                num TEXT,
                features JSONB,
                FOREIGN KEY(crate_id) REFERENCES crates(id)
        )")
     (sqlite-execute con "INSERT OR IGNORE INTO crates (name, id) SELECT name, id FROM crates_raw")
     (sqlite-execute con "INSERT OR IGNORE INTO crates (name, id) SELECT name, id FROM versions_raw"))))

(defun crates-db--get-con (&optional con)
  "Ensure that the connection to the db is open and return it.
If `CON' is sqlite connection return it."
  (cond
   ((sqlitep con) con)
   ((not crates-db--con) (setq crates-db--con (sqlite-open crates-db-path)))
   (t crates-db--con)))

(defun crates-db--crate-id-from-name (crate-name &optional con)
  "Get the id of a crate with `CRATE-NAME'.
Optionally using `CON' sqlite connection."
  (let* ((con (crates-db--get-con con)))
    (car (sqlite-select con "SELECT id FROM crates WHERE name = ? LIMIT 1" crate-name))))

(defun crates-db--propose-crates (prefix limit &optional con)
  "Propose crates with `PREFIX' and limit the result to `LIMIT'.
Optionally using `CON' sqlite connection."
  (let ((con    (crates-db--get-con con))
        (prefix (concat prefix "%")))
    (mapcar #'car (sqlite-select con "SELECT name FROM crates WHERE name LIKE ? LIMIT ?" (list prefix limit)))))

(defun crates-db--available-versions-for-crate (crate-name limit &optional con)
  "Return list of versions of a crate `CRATE-NAME' limitting to `LIMIT' items.
Optionally using `CON' sqlite connection."
  (let* ((con      (crates-db--get-con con))
         (crate-id (crates-db--crate-id-from-name crate-name con)))
    (sqlite-select con "SELECT num FROM versions WHERE crate_id = ? LIMIT ?" (list crate-id limit))))

(provide 'crates-db)
;;; crates-db.el ends here
