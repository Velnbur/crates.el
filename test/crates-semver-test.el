;;; crates-semver-test.el --- Tests for crates-semver  -*- lexical-binding: t; -*-

;;; Commentary:

;; Requirement cases mirror the examples in the Cargo book
;; ("Specifying Dependencies") and the `semver' crate's test suite.

;;; Code:

(require 'ert)
(require 'crates-semver)

(defmacro crates-semver-test--matrix (req &rest cases)
  "Assert REQ against CASES of the form (VERSION EXPECTED)."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (c)
                 `(should (eq (and (crates-semver-satisfies-p ,req ,(car c)) t)
                              ,(cadr c))))
               cases)))

(ert-deftest crates-semver-test-parse ()
  (let ((v (crates-semver-parse "1.2.3-alpha.1+build.5")))
    (should (equal (list (crates-semver-version-major v)
                         (crates-semver-version-minor v)
                         (crates-semver-version-patch v)
                         (crates-semver-version-pre v))
                   '(1 2 3 ("alpha" 1)))))
  (should-not (crates-semver-parse "1.2"))
  (should-not (crates-semver-parse "abc")))

(ert-deftest crates-semver-test-ordering ()
  (let ((sorted '("1.0.0-alpha" "1.0.0-alpha.1" "1.0.0-alpha.beta" "1.0.0-beta"
                  "1.0.0-beta.2" "1.0.0-beta.11" "1.0.0-rc.1" "1.0.0" "1.0.1"
                  "1.1.0" "2.0.0")))
    (should (equal (sort (reverse (copy-sequence sorted))
                         (lambda (a b) (crates-semver< (crates-semver-parse a)
                                                       (crates-semver-parse b))))
                   sorted))))

(ert-deftest crates-semver-test-caret ()
  (crates-semver-test--matrix "1.2.3"
    ("1.2.3" t) ("1.9.0" t) ("1.2.2" nil) ("2.0.0" nil) ("1.3.0-alpha" nil))
  (crates-semver-test--matrix "^1.2"
    ("1.2.0" t) ("1.99.0" t) ("1.1.9" nil) ("2.0.0" nil))
  (crates-semver-test--matrix "1"
    ("1.0.0" t) ("1.99.99" t) ("0.9.0" nil) ("2.0.0" nil))
  (crates-semver-test--matrix "0.2.3"
    ("0.2.3" t) ("0.2.9" t) ("0.3.0" nil) ("0.2.2" nil))
  (crates-semver-test--matrix "0.2"
    ("0.2.0" t) ("0.2.9" t) ("0.3.0" nil))
  (crates-semver-test--matrix "0.0.3"
    ("0.0.3" t) ("0.0.4" nil))
  (crates-semver-test--matrix "0.0"
    ("0.0.9" t) ("0.1.0" nil))
  (crates-semver-test--matrix "0"
    ("0.9.9" t) ("1.0.0" nil)))

(ert-deftest crates-semver-test-tilde ()
  (crates-semver-test--matrix "~1.2.3"
    ("1.2.3" t) ("1.2.9" t) ("1.3.0" nil))
  (crates-semver-test--matrix "~1.2"
    ("1.2.0" t) ("1.2.9" t) ("1.3.0" nil))
  (crates-semver-test--matrix "~1"
    ("1.0.0" t) ("1.9.0" t) ("2.0.0" nil)))

(ert-deftest crates-semver-test-wildcard ()
  (crates-semver-test--matrix "*"
    ("0.0.1" t) ("99.0.0" t) ("1.0.0-alpha" nil))
  (crates-semver-test--matrix "1.*"
    ("1.0.0" t) ("1.9.9" t) ("2.0.0" nil))
  (crates-semver-test--matrix "1.2.*"
    ("1.2.0" t) ("1.2.9" t) ("1.3.0" nil)))

(ert-deftest crates-semver-test-comparison ()
  (crates-semver-test--matrix ">= 1.2.0, < 1.5"
    ("1.2.0" t) ("1.4.9" t) ("1.5.0" nil) ("1.1.0" nil))
  (crates-semver-test--matrix "=1.2.3"
    ("1.2.3" t) ("1.2.4" nil))
  (crates-semver-test--matrix ">1.2"
    ("1.3.0" t) ("1.2.9" nil))
  (crates-semver-test--matrix "<=1.2"
    ("1.2.9" t) ("1.3.0" nil)))

(ert-deftest crates-semver-test-prerelease ()
  (crates-semver-test--matrix "1.0.0-beta.2"
    ("1.0.0-beta.2" t) ("1.0.0-beta.10" t) ("1.0.0-rc.1" t) ("1.0.0" t)
    ("1.0.0-beta.1" nil) ("1.0.1-beta.3" nil) ("1.2.0" t))
  (crates-semver-test--matrix ">=1.0.0-alpha, <2"
    ("1.0.0-beta" t) ("1.1.0-beta" nil)))

(ert-deftest crates-semver-test-invalid ()
  (should-not (crates-semver-parse-req "foo"))
  (should-not (crates-semver-parse-req "1.*.3"))
  (should-not (crates-semver-parse-req ">=*"))
  (should-not (crates-semver-parse-req "1.2-beta")))

(ert-deftest crates-semver-test-simple-req ()
  (should (equal (crates-semver-simple-req-p "1.2") '("" . 2)))
  (should (equal (crates-semver-simple-req-p "^0.3.1") '("^" . 3)))
  (should (equal (crates-semver-simple-req-p "~ 1") '("~ " . 1)))
  (should-not (crates-semver-simple-req-p ">=1, <2"))
  (should-not (crates-semver-simple-req-p "=1.2.3")))

(ert-deftest crates-semver-test-truncate ()
  (should (equal (crates-semver-truncate "2.0.1" 1) "2"))
  (should (equal (crates-semver-truncate "2.0.1-rc.1" 2) "2.0"))
  (should (equal (crates-semver-truncate "2.0.1-rc.1" 3) "2.0.1-rc.1")))

(provide 'crates-semver-test)
;;; crates-semver-test.el ends here
