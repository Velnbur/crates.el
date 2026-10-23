;;; crates-semver.el --- Cargo-flavoured semver for crates.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kyrylo Baibula

;; Author: Kyrylo Baibula <kyrylobaybula@gmail.com>
;; URL: https://github.com/Velnbur/crates.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Parsing and matching of semantic versions and Cargo version
;; requirements.  The matching rules follow the `semver' Rust crate
;; that Cargo itself uses, including its treatment of pre-releases.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(cl-defstruct (crates-semver-version
               (:constructor crates-semver--make-version)
               (:copier nil))
  "A parsed semantic version."
  major minor patch
  (pre nil :documentation "List of pre-release identifiers (numbers or strings)."))

(cl-defstruct (crates-semver-comparator
               (:constructor crates-semver--make-comparator)
               (:copier nil))
  "One comparator of a version requirement.
OP is one of the symbols `exact', `greater', `greater-eq', `less',
`less-eq', `tilde', `caret' or `wildcard'.  MINOR and PATCH are nil
when omitted or given as a wildcard."
  op major minor patch pre)

(defconst crates-semver--ident-re "[0-9A-Za-z-]+\\(?:\\.[0-9A-Za-z-]+\\)*"
  "Regexp matching dot-separated pre-release or build identifiers.")

(defconst crates-semver--version-re
  (concat "\\`[ \t]*v?\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"
          "\\(?:-\\(" crates-semver--ident-re "\\)\\)?"
          "\\(?:\\+" crates-semver--ident-re "\\)?[ \t]*\\'")
  "Regexp matching a full semantic version.")

(defconst crates-semver--comparator-re
  (concat "\\`[ \t]*\\(\\^\\|~\\|>=\\|<=\\|>\\|<\\|=\\)?[ \t]*v?"
          "\\([0-9]+\\|[*xX]\\)"
          "\\(?:\\.\\([0-9]+\\|[*xX]\\)"
          "\\(?:\\.\\([0-9]+\\|[*xX]\\)"
          "\\(?:-\\(" crates-semver--ident-re "\\)\\)?\\)?\\)?"
          "\\(?:\\+" crates-semver--ident-re "\\)?[ \t]*\\'")
  "Regexp matching one comparator of a version requirement.")

(defun crates-semver--parse-pre (string)
  "Split pre-release STRING into a list of identifiers."
  (when string
    (mapcar (lambda (id)
              (if (string-match-p "\\`[0-9]+\\'" id) (string-to-number id) id))
            (split-string string "\\."))))

(defun crates-semver-parse (string)
  "Parse STRING as a semantic version, or return nil if it is not one."
  (when (and (stringp string)
             (string-match crates-semver--version-re string))
    (crates-semver--make-version
     :major (string-to-number (match-string 1 string))
     :minor (string-to-number (match-string 2 string))
     :patch (string-to-number (match-string 3 string))
     :pre (crates-semver--parse-pre (match-string 4 string)))))

(defun crates-semver--compare-pre (a b)
  "Compare pre-release identifier lists A and B.
Return a negative, zero or positive integer.  An empty list (a
release) sorts after any pre-release."
  (cond
   ((and (null a) (null b)) 0)
   ((null a) 1)
   ((null b) -1)
   (t
    (catch 'done
      (while (or a b)
        (let ((x (pop a)) (y (pop b)))
          (cond
           ((null x) (throw 'done -1))
           ((null y) (throw 'done 1))
           ((and (numberp x) (numberp y))
            (unless (= x y) (throw 'done (if (< x y) -1 1))))
           ((numberp x) (throw 'done -1))
           ((numberp y) (throw 'done 1))
           ((not (string= x y))
            (throw 'done (if (string< x y) -1 1))))))
      0))))

(defun crates-semver-compare (a b)
  "Compare versions A and B, returning a negative, zero or positive integer."
  (let ((d (or (cl-find-if-not #'zerop
                               (list (- (crates-semver-version-major a)
                                        (crates-semver-version-major b))
                                     (- (crates-semver-version-minor a)
                                        (crates-semver-version-minor b))
                                     (- (crates-semver-version-patch a)
                                        (crates-semver-version-patch b))))
               0)))
    (if (zerop d)
        (crates-semver--compare-pre (crates-semver-version-pre a)
                                    (crates-semver-version-pre b))
      d)))

(defun crates-semver< (a b)
  "Return non-nil if version A is lower than version B."
  (< (crates-semver-compare a b) 0))

(defun crates-semver--number (string)
  "Return STRING as a number, or nil when it is absent or a wildcard."
  (when (and string (string-match-p "\\`[0-9]+\\'" string))
    (string-to-number string)))

(defun crates-semver--parse-comparator (string)
  "Parse a single comparator STRING, or return nil."
  (when (string-match crates-semver--comparator-re string)
    (let* ((op (match-string 1 string))
           (major (crates-semver--number (match-string 2 string)))
           (minor-text (match-string 3 string))
           (minor (crates-semver--number minor-text))
           (patch-text (match-string 4 string))
           (patch (crates-semver--number patch-text))
           (pre (match-string 5 string)))
      (cond
       ;; `*' (optionally `*.*'...) matches every release.
       ((null major) (and (null op) (null minor) (null patch) (null pre) 'any))
       ;; Nothing numeric may follow a wildcard, nor a pre-release.
       ((or (and minor-text (null minor) patch)
            (and pre (null patch)))
        nil)
       (t
        (crates-semver--make-comparator
         :op (cond
              ((and (null op) (or (and minor-text (null minor))
                                  (and patch-text (null patch))))
               'wildcard)
              ((member op '(nil "^")) 'caret)
              ((equal op "~") 'tilde)
              ((equal op "=") 'exact)
              ((equal op ">") 'greater)
              ((equal op ">=") 'greater-eq)
              ((equal op "<") 'less)
              ((equal op "<=") 'less-eq))
         :major major
         :minor minor
         :patch (and minor patch)
         :pre (crates-semver--parse-pre pre)))))))

(defun crates-semver-parse-req (string)
  "Parse Cargo requirement STRING into a list of comparators.
Return the symbol `any' for requirements that match every release,
and nil when STRING cannot be parsed."
  (when (stringp string)
    (let ((parts (mapcar #'crates-semver--parse-comparator
                         (split-string string "," nil "[ \t]+"))))
      (cond
       ((memq nil parts) nil)
       ((cl-every (lambda (p) (eq p 'any)) parts) 'any)
       (t (delq 'any parts))))))

(defun crates-semver--match-exact (c v)
  "Non-nil if version V matches comparator C exactly."
  (and (= (crates-semver-version-major v) (crates-semver-comparator-major c))
       (or (null (crates-semver-comparator-minor c))
           (= (crates-semver-version-minor v) (crates-semver-comparator-minor c)))
       (or (null (crates-semver-comparator-patch c))
           (= (crates-semver-version-patch v) (crates-semver-comparator-patch c)))
       (zerop (crates-semver--compare-pre (crates-semver-version-pre v)
                                          (crates-semver-comparator-pre c)))))

(defun crates-semver--match-order (c v sign)
  "Non-nil if V is strictly greater (SIGN 1) or lower (SIGN -1) than C."
  (let ((pairs (list (cons (crates-semver-version-major v) (crates-semver-comparator-major c))
                     (cons (crates-semver-version-minor v) (crates-semver-comparator-minor c))
                     (cons (crates-semver-version-patch v) (crates-semver-comparator-patch c)))))
    (catch 'done
      (dolist (p pairs)
        (cond
         ((null (cdr p)) (throw 'done nil))
         ((/= (car p) (cdr p))
          (throw 'done (> (* sign (- (car p) (cdr p))) 0)))))
      (> (* sign (crates-semver--compare-pre (crates-semver-version-pre v)
                                             (crates-semver-comparator-pre c)))
         0))))

(defun crates-semver--pre>= (c v)
  "Non-nil if the pre-release of V is at least that of C."
  (>= (crates-semver--compare-pre (crates-semver-version-pre v)
                                  (crates-semver-comparator-pre c))
      0))

(defun crates-semver--match-tilde (c v)
  "Non-nil if V matches tilde comparator C."
  (let ((minor (crates-semver-comparator-minor c))
        (patch (crates-semver-comparator-patch c)))
    (and (= (crates-semver-version-major v) (crates-semver-comparator-major c))
         (or (null minor) (= (crates-semver-version-minor v) minor))
         (if (and patch (/= (crates-semver-version-patch v) patch))
             (> (crates-semver-version-patch v) patch)
           (crates-semver--pre>= c v)))))

(defun crates-semver--match-caret (c v)
  "Non-nil if V matches caret comparator C."
  (let ((major (crates-semver-comparator-major c))
        (minor (crates-semver-comparator-minor c))
        (patch (crates-semver-comparator-patch c))
        (vminor (crates-semver-version-minor v))
        (vpatch (crates-semver-version-patch v)))
    (cond
     ((/= (crates-semver-version-major v) major) nil)
     ((null minor) t)
     ((null patch) (if (> major 0) (>= vminor minor) (= vminor minor)))
     ((> major 0)
      (cond ((/= vminor minor) (> vminor minor))
            ((/= vpatch patch) (> vpatch patch))
            (t (crates-semver--pre>= c v))))
     ((> minor 0)
      (cond ((/= vminor minor) nil)
            ((/= vpatch patch) (> vpatch patch))
            (t (crates-semver--pre>= c v))))
     ((or (/= vminor minor) (/= vpatch patch)) nil)
     (t (crates-semver--pre>= c v)))))

(defun crates-semver--match-comparator (c v)
  "Non-nil if version V matches comparator C."
  (pcase (crates-semver-comparator-op c)
    ((or 'exact 'wildcard) (crates-semver--match-exact c v))
    ('greater (crates-semver--match-order c v 1))
    ('greater-eq (or (crates-semver--match-exact c v) (crates-semver--match-order c v 1)))
    ('less (crates-semver--match-order c v -1))
    ('less-eq (or (crates-semver--match-exact c v) (crates-semver--match-order c v -1)))
    ('tilde (crates-semver--match-tilde c v))
    ('caret (crates-semver--match-caret c v))))

(defun crates-semver--pre-allowed-p (req v)
  "Non-nil if pre-release V is opted into by a comparator of REQ.
That is the case when a comparator names a pre-release of the same
major.minor.patch."
  (cl-some (lambda (c)
             (and (crates-semver-comparator-pre c)
                  (eql (crates-semver-comparator-major c) (crates-semver-version-major v))
                  (eql (crates-semver-comparator-minor c) (crates-semver-version-minor v))
                  (eql (crates-semver-comparator-patch c) (crates-semver-version-patch v))))
           req))

(defun crates-semver-satisfies-p (req version)
  "Non-nil if VERSION satisfies requirement REQ.
REQ is the result of `crates-semver-parse-req' or a string; VERSION a
parsed version or a string."
  (let ((req (if (stringp req) (crates-semver-parse-req req) req))
        (v (if (stringp version) (crates-semver-parse version) version)))
    (and req v
         (if (eq req 'any)
             (null (crates-semver-version-pre v))
           (and (cl-every (lambda (c) (crates-semver--match-comparator c v)) req)
                (or (null (crates-semver-version-pre v))
                    (crates-semver--pre-allowed-p req v)))))))

(defun crates-semver-simple-req-p (string)
  "If STRING is a bare, caret or tilde requirement, return its parts.
The result is (PREFIX . COMPONENTS) where PREFIX is the operator text
as written (possibly empty) and COMPONENTS the number of version
components given.  Return nil for anything more complex."
  (when (and (stringp string)
             (string-match "\\`\\([ \t]*\\(?:\\^\\|~\\)?[ \t]*\\)\\([0-9]+\\(?:\\.[0-9]+\\)\\{0,2\\}\\)[ \t]*\\'"
                           string))
    (cons (match-string 1 string)
          (length (split-string (match-string 2 string) "\\.")))))

(defun crates-semver-truncate (version components)
  "Return VERSION (a string) cut down to its first COMPONENTS parts.
A pre-release or build suffix is kept only when all three parts are."
  (if (>= components 3)
      version
    (string-join (seq-take (split-string (car (split-string version "[-+]")) "\\.")
                           components)
                 ".")))

(provide 'crates-semver)
;;; crates-semver.el ends here
