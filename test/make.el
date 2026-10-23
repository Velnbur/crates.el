;;; make.el --- Batch helpers for the Makefile  -*- lexical-binding: t; -*-

;;; Commentary:

;; Entry points used by the Makefile, so that CI and local runs do the
;; same thing:
;;
;;   crates-make-deps      install package-lint and the toml grammar
;;   crates-make-checkdoc  run checkdoc, failing on any warning
;;   crates-make-lint      run package-lint
;;
;; Everything is installed under .cache/ in the repository.

;;; Code:

(require 'package)
(require 'treesit)

(defconst crates-make-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory of the repository.")

(defconst crates-make-sources
  '("crates.el" "crates-db.el" "crates-semver.el" "crates-toml.el")
  "Files that make up the package.")

(defvar crates-make-toml-grammar
  ;; The grammar `toml-ts-mode' was written against.  Its generated
  ;; parser uses an old ABI, which every Emacs >= 29 can load.
  '(toml "https://github.com/ikatyang/tree-sitter-toml" "v0.5.1")
  "Source of the `toml' grammar, as in `treesit-language-source-alist'.")

(defun crates-make--cache (name)
  "Return directory NAME under the repository's .cache."
  (expand-file-name name (expand-file-name ".cache" crates-make-root)))

(defun crates-make--package-init ()
  "Use the repository-local package directory."
  (setq package-user-dir (crates-make--cache "elpa")
        package-archives '(("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

(defun crates-make-deps ()
  "Install the development dependencies."
  (crates-make--package-init)
  (unless (package-installed-p 'package-lint)
    (package-refresh-contents)
    (package-install 'package-lint))
  ;; The Makefile puts GRAMMAR_DIR first on `treesit-extra-load-path'.
  (let ((dir (or (car treesit-extra-load-path) (crates-make--cache "tree-sitter"))))
    (unless (treesit-language-available-p 'toml)
      (let ((treesit-language-source-alist (list crates-make-toml-grammar)))
        (treesit-install-language-grammar 'toml dir)))
    (unless (treesit-language-available-p 'toml)
      (error "Could not install the toml grammar"))))

(defun crates-make-checkdoc ()
  "Run checkdoc on the package and exit non-zero on any warning."
  (require 'checkdoc)
  (let ((failed nil))
    ;; In batch mode checkdoc reports through `display-warning'.
    (advice-add 'display-warning :before (lambda (&rest _) (setq failed t)))
    (dolist (file crates-make-sources)
      (checkdoc-file (expand-file-name file crates-make-root)))
    (kill-emacs (if failed 1 0))))

(defun crates-make-lint ()
  "Run package-lint on the package and exit."
  (crates-make--package-init)
  (require 'package-lint)
  (setq package-lint-main-file "crates.el")
  (setq command-line-args-left
        (mapcar (lambda (f) (expand-file-name f crates-make-root)) crates-make-sources))
  (package-lint-batch-and-exit))

(provide 'make)
;;; make.el ends here
