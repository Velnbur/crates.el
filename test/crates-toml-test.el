;;; crates-toml-test.el --- Tests for crates-toml  -*- lexical-binding: t; -*-

;;; Commentary:

;; Needs the `toml' tree-sitter grammar; skipped when it is missing.

;;; Code:

(require 'ert)
(require 'crates-toml)
(require 'crates-test-helpers)

(defconst crates-toml-test--fixture
  (expand-file-name "fixtures/Cargo.toml"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path of the fixture manifest.")

(defmacro crates-toml-test--with-fixture (&rest body)
  "Run BODY in a buffer holding the fixture manifest, with a toml parser."
  (declare (indent 0))
  `(progn
     (crates-test-skip-unless-grammar)
     (with-temp-buffer
       (insert-file-contents crates-toml-test--fixture)
       (treesit-parser-create 'toml)
       ,@body)))

(defun crates-toml-test--summary (dep)
  "Return the fields of DEP worth asserting on."
  (list (crates-toml-dep-name dep)
        (crates-toml-dep-crate dep)
        (crates-toml-dep-req dep)
        (and (crates-toml-dep-workspace dep) t)
        (and (crates-toml-dep-source dep) t)
        (crates-toml-dep-registry dep)))

(ert-deftest crates-toml-test-dependencies ()
  (crates-toml-test--with-fixture
    (should (equal (mapcar #'crates-toml-test--summary (crates-toml-dependencies))
                   '(("serde" "serde" "1.0" nil nil nil)
                     ("anyhow" "anyhow" "1" nil nil nil)
                     ("tokio" "tokio" "1.2" nil nil nil)
                     ("quoted" "quoted" "0.3" nil nil nil)
                     ("renamed" "real-name" "~2.1" nil nil nil)
                     ("local" "local" nil nil t nil)
                     ("inherited" "inherited" nil t nil nil)
                     ("private" "private" "1" nil nil "my-registry")
                     ("subtable" "subtable" "=0.1.2" nil nil nil)
                     ("proptest" "proptest" ">=1, <2" nil nil nil)
                     ("libc" "libc" "0.2" nil nil nil)
                     ("regex" "regex" "1.10" nil nil nil))))))

(ert-deftest crates-toml-test-positions ()
  (crates-toml-test--with-fixture
    (dolist (dep (crates-toml-dependencies))
      (when (crates-toml-dep-req dep)
        (should (equal (buffer-substring-no-properties
                        (crates-toml-dep-req-beg dep) (crates-toml-dep-req-end dep))
                       (crates-toml-dep-req dep))))
      (should (save-excursion
                (goto-char (crates-toml-dep-key-beg dep))
                (looking-at-p (regexp-quote (if (equal (crates-toml-dep-name dep) "quoted")
                                                "\"quoted\""
                                              (crates-toml-dep-name dep)))))))))

(ert-deftest crates-toml-test-registry-filter ()
  (crates-toml-test--with-fixture
    (should (equal (mapcar #'crates-toml-dep-name
                           (seq-filter #'crates-toml-registry-dep-p (crates-toml-dependencies)))
                   '("serde" "anyhow" "tokio" "quoted" "renamed" "subtable"
                     "proptest" "libc" "regex")))))

(provide 'crates-toml-test)
;;; crates-toml-test.el ends here
