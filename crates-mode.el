;;; crates-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Velnbur
;;
;; Author: Velnbur <kyrylobaybula@gmail.com>
;; Maintainer: Velnbur <kyrylobaybula@gmail.com>
;; Created: липня 09, 2024
;; Modified: липня 09, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/velnbur/crates-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides a minor mode for completion inside of the `Cargo.toml' files.
;;
;;; Code:

(require 'crates-company)

(define-minor-mode crates-mode
  "A minor mode for completion inside of the `Cargo.toml' files."
  :init-value nil
  :lighter " Crates"
  :after-hook (lambda ()
                (crates-company-setup)))

(provide 'crates-mode)
;;; crates-mode.el ends here
