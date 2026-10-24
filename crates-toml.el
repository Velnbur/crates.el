;;; crates-toml.el --- Find dependencies in Cargo.toml with tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kyrylo Baibula

;; Author: Kyrylo Baibula <kyrylobaybula@gmail.com>
;; URL: https://github.com/Velnbur/crates.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Walks the `toml' tree-sitter tree of a Cargo manifest and returns
;; every dependency declaration, whichever of TOML's spellings it
;; uses: plain strings, inline tables, dotted keys or sub-tables, in
;; `[dependencies]', `[dev-dependencies]', `[build-dependencies]',
;; `[workspace.dependencies]' and `[target.CFG.*dependencies]'.
;;
;; Every value is first flattened to its full key path, so that
;; `[dependencies.serde] version = "1"', `serde.version = "1"' and
;; `serde = { version = "1" }' all become ("dependencies" "serde"
;; "version") and are handled by one code path.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'treesit)

(declare-function treesit-node-children "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-parser-root-node "treesit.c")
(declare-function treesit-parser-list "treesit.c")
(declare-function treesit-parser-language "treesit.c")

(cl-defstruct (crates-toml-dep
               (:constructor crates-toml--make-dep)
               (:copier nil))
  "A dependency declared in a Cargo manifest."
  (name nil :documentation "The key the dependency is declared under.")
  (table nil :documentation "The table it is declared in, as a list of key segments.")
  (package nil :documentation "The `package' field, when the dependency is renamed.")
  (req nil :documentation "The version requirement string, or nil.")
  (req-beg nil :documentation "Buffer position where the requirement text starts.")
  (req-end nil :documentation "Buffer position where the requirement text ends.")
  (key-beg nil :documentation "Buffer position of the dependency's name.")
  (string nil :documentation "(BEG . END) of the string value, when declared as NAME = \"REQ\".")
  (workspace nil :documentation "Non-nil for `workspace = true' dependencies.")
  (source nil :documentation "Non-nil when it has a `path' or `git' source.")
  (registry nil :documentation "The `registry' field, if any."))

(defun crates-toml-dep-crate (dep)
  "Return the crates.io name that DEP refers to."
  (or (crates-toml-dep-package dep) (crates-toml-dep-name dep)))

(defconst crates-toml--dep-kinds '("dependencies" "dev-dependencies" "build-dependencies")
  "Table names that hold dependencies.")

(defun crates-toml--key-path (node)
  "Return the key segments that key NODE spells.
Each element is (NAME . SEGMENT-NODE)."
  (pcase (treesit-node-type node)
    ("bare_key" (list (cons (treesit-node-text node t) node)))
    ("quoted_key" (list (cons (crates-toml--unquote (treesit-node-text node t)) node)))
    ("dotted_key" (mapcan #'crates-toml--key-path (treesit-node-children node t)))))

(defun crates-toml--quote-length (text)
  "Return the length of the opening quote of string literal TEXT."
  (if (or (string-prefix-p "\"\"\"" text) (string-prefix-p "'''" text)) 3 1))

(defun crates-toml--unquote (text)
  "Strip the quotes from string literal TEXT.
Escape sequences are left alone: neither crate names nor versions
use them."
  (let ((q (crates-toml--quote-length text)))
    (if (>= (length text) (* 2 q))
        (substring text q (- (length text) q))
      text)))

(defun crates-toml--key-p (node)
  "Non-nil if NODE is a key node."
  (member (treesit-node-type node) '("bare_key" "quoted_key" "dotted_key")))

(defun crates-toml--pairs (node prefix)
  "Return (PATH . VALUE-NODE) for every leaf under NODE, prefixed by PREFIX.
NODE is a `table', `inline_table' or `document' node.  PATH is a
list of (NAME . SEGMENT-NODE) as returned by `crates-toml--key-path'."
  (let (result)
    (dolist (child (treesit-node-children node t))
      (when (equal (treesit-node-type child) "pair")
        (let* ((parts (treesit-node-children child t))
               (key (car parts))
               (value (car (last parts)))
               (path (append prefix (crates-toml--key-path key))))
          (if (equal (treesit-node-type value) "inline_table")
              (setq result (nconc result (crates-toml--pairs value path)))
            (setq result (nconc result (list (cons path value))))))))
    result))

(defun crates-toml--leaves (root)
  "Return (PATH . VALUE-NODE) for every key/value in the document ROOT."
  (let ((result (crates-toml--pairs root nil)))
    (dolist (child (treesit-node-children root t))
      (when (equal (treesit-node-type child) "table")
        (let ((key (cl-find-if #'crates-toml--key-p (treesit-node-children child t))))
          (when key
            (setq result
                  (nconc result
                         (crates-toml--pairs child (crates-toml--key-path key))))))))
    result))

(defun crates-toml--deps-prefix-length (path)
  "Return how many segments of PATH name a dependencies table, or nil."
  (let ((names (mapcar #'car path)))
    (cond
     ((member (nth 0 names) crates-toml--dep-kinds) 1)
     ((equal (seq-take names 2) '("workspace" "dependencies")) 2)
     ((and (equal (nth 0 names) "target") (member (nth 2 names) crates-toml--dep-kinds)) 3))))

(defun crates-toml--string-node-p (node)
  "Non-nil if NODE is a string value."
  (equal (treesit-node-type node) "string"))

(defun crates-toml-parser ()
  "Return the current buffer's `toml' parser, or nil."
  ;; Emacs 29's `treesit-parser-list' cannot filter by language.
  (and (treesit-available-p)
       (seq-find (lambda (p) (eq (treesit-parser-language p) 'toml))
                 (treesit-parser-list))))

(defun crates-toml-dependencies (&optional root)
  "Return the dependencies declared under ROOT, in buffer order.
ROOT defaults to the root node of the current buffer's `toml' parser.
Each element is a `crates-toml-dep'."
  (let ((root (or root (treesit-parser-root-node (crates-toml-parser))))
        (deps nil))                     ; alist (TABLE-PATH NAME) -> dep
    (dolist (leaf (crates-toml--leaves root))
      (let* ((path (car leaf))
             (value (cdr leaf))
             (n (crates-toml--deps-prefix-length path)))
        (when (and n (> (length path) n))
          (let* ((name-cell (nth n path))
                 (id (cons (mapcar #'car (seq-take path n)) (car name-cell)))
                 (field (car (nth (1+ n) path)))
                 (dep (or (cdr (assoc id deps))
                          (let ((d (crates-toml--make-dep
                                    :name (car name-cell)
                                    :table (car id)
                                    :key-beg (treesit-node-start (cdr name-cell)))))
                            (push (cons id d) deps)
                            d)))
                 (text (and (crates-toml--string-node-p value)
                            (treesit-node-text value t))))
            (when (= (length path) (+ n (if field 2 1)))
              (pcase field
                ((or 'nil "version")
                 (when text
                   (let ((q (crates-toml--quote-length text)))
                     (setf (crates-toml-dep-req dep) (crates-toml--unquote text)
                           (crates-toml-dep-req-beg dep) (+ (treesit-node-start value) q)
                           (crates-toml-dep-req-end dep) (- (treesit-node-end value) q))
                     (unless field
                       (setf (crates-toml-dep-string dep)
                             (cons (treesit-node-start value) (treesit-node-end value)))))))
                ("package" (when text
                             (setf (crates-toml-dep-package dep) (crates-toml--unquote text))))
                ("registry" (when text
                              (setf (crates-toml-dep-registry dep) (crates-toml--unquote text))))
                ("workspace" (setf (crates-toml-dep-workspace dep)
                                   (equal (treesit-node-text value t) "true")))
                ((or "path" "git") (setf (crates-toml-dep-source dep) t))))))))
    (sort (mapcar #'cdr deps)
          (lambda (a b) (< (crates-toml-dep-key-beg a) (crates-toml-dep-key-beg b))))))

(defun crates-toml-registry-dep-p (dep)
  "Non-nil if DEP is resolved from crates.io and has a requirement."
  (and (crates-toml-dep-req dep)
       (not (crates-toml-dep-workspace dep))
       (or (null (crates-toml-dep-registry dep))
           (equal (crates-toml-dep-registry dep) "crates-io"))))

(provide 'crates-toml)
;;; crates-toml.el ends here
