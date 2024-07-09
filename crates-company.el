;;; crates-company.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Velnbur
;;
;; Author: Velnbur <kyrylobaybula@gmail.com>
;; Maintainer: Velnbur <kyrylobaybula@gmail.com>
;; Created: липня 09, 2024
;; Modified: липня 09, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/velnbur/crates-company
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides crates completion using company-mode
;;
;;; Code:

(require 'cl-lib)
(require 'company)
(require 'crates-db)

(defvar crates-company-max-candidates 30
  "Maximum number of candidates to include.")

(defun crates-company-backend (command &optional arg &rest ignored)
  "Company backend for crates."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'crates-company-backend))
    (prefix (and (member 'crates-mode local-minor-modes)
                 (company-grab-symbol)))
    (candidates
     (all-completions
      arg
      (crates-db--propose-crates arg crates-company-max-candidates)))))

;;;###autoload
(defun crates-company-setup ()
  "Add crates backend to company-mode."
  (interactive)
  (add-to-list 'company-backends 'crates-company-backend))

(defvar crates-company--completion-cache nil
  "Cache for the last completion.")

(provide 'crates-company)
;;; crates-company.el ends here
