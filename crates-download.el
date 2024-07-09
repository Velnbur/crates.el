;;; crates-download.el --- Description  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Velnbur
;;
;; Author: Velnbur <kyrylobaybula@gmail.com>
;; Maintainer: Velnbur <kyrylobaybula@gmail.com>
;; Created: липня 08, 2024
;; Modified: липня 08, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/velnbur/crates-download
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Download package
;;
;;; Code:

(require 'async)
(require 'files)
(require 'xdg)

(defvar crates-download--url "https://static.crates.io/db-dump.tar.gz"
  "URL to download crates.io database dump.")

(defvar crates-download-tmp-dir
  (temporary-file-directory)
  "Directory to store temporary files.")

(defvar crates-download-tmp-archive-path
  (concat crates-download-tmp-dir "db-dump.tar.gz")
  "Directory to temporary store archive with database dump.")

(defvar crates-download-tmp-dump-dir
  (concat crates-download-tmp-dir "db-dump")
  "Directory to temporary store decompressed database dump.")

(defvar crates-download-dest-dir
  (concat (file-name-as-directory (xdg-state-home)) "/crates-download/crates-io")
  "Directory to store database dump.")

;; TODO: actually use this
(defconst crates-download--user-agent
  (format "Emacs %s" emacs-version)
  "User agent for HTTP requests.")

;;;###autoload
(defun crates-download-db ()
  "Download crates.io database dump."
  (interactive)
  (async-start
   (lambda () (crates-download-db-sync))
   (lambda (file-path)
     (message "Downloaded crates.io sql dump to %s" file-path))))

;;;###autoload
(defun crates-download-db-sync ()
  "Download crates.io database dump sync."
  (interactive)
  (let* ((archive (crates-download--download-sql-dump))
         (dir (crates-download--tar-gz-decompress archive)))
    (crates-download--move-result dir crates-download-dest-dir)
    crates-download-dest-dir))

(defun crates-download--download-sql-dump (&optional dest)
  "Download sql dump from crates.io into `DEST' and return path to it."
  (let ((file-path (or dest crates-download-tmp-archive-path)))
    (unless (file-exists-p file-path)
      (url-copy-file crates-download--url file-path t))
    file-path))

(defun crates-download--tar-gz-decompress (file &optional dest)
  "Decompress tar gz `FILE' archive into `DEST'.
Return path to the directory with decompressed archive."
  (let ((file (expand-file-name file))
        (dir (or dest crates-download-tmp-dump-dir)))
    ;; TODO: maybe use `call-process' or `start-process' instead?
    (unless (file-exists-p dir)
      (make-directory dir t))
    (unless (shell-command
             (format "tar -xzf %s -C %s" file (expand-file-name dir))
             "*crates-download*"
             "*crates-download-error*")
      (error "Failed to decompress archive"))
    dir))

(defun crates-download--move-result (dir dest)
  "Prepare `DIR' by removing inner directory with date name and move it to `DEST'."
  ;; match is added here to skip . and .. directories
  (let ((inner-dir (car (directory-files dir t "^[0-9]"))))
    (rename-file inner-dir dest t)))

(provide 'crates-download)
;;; crates-download.el ends here
