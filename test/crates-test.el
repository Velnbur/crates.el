;;; crates-test.el --- Tests for crates-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; End-to-end tests: the fixture manifest annotated from the fixture
;; database.  Need the `toml' grammar and the update pipeline's tools.

;;; Code:

(require 'ert)
(require 'crates)
(require 'crates-test-helpers)

(defmacro crates-test--with-manifest (&rest body)
  "Run BODY in `crates-mode' visiting a copy of the fixture manifest."
  (declare (indent 0))
  `(progn
     (crates-test-skip-unless-grammar)
     (crates-test-with-db
       (clrhash crates--cache)
       (with-temp-buffer
         (insert-file-contents (expand-file-name "fixtures/Cargo.toml" crates-test--dir))
         (let ((crates-db-auto-update nil)
               (inhibit-message t))
           (crates-mode))
         ,@body))))

(defun crates-test--annotations ()
  "Return (LINE-TEXT . ANNOTATION) for every annotation in the buffer."
  (mapcar (lambda (ov)
            (save-excursion
              (goto-char (overlay-start ov))
              (cons (string-trim (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position)))
                    (string-trim (substring-no-properties (overlay-get ov 'after-string))))))
          (sort (seq-filter (lambda (ov) (overlay-get ov 'crates))
                            (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(ert-deftest crates-test-annotations ()
  (crates-test--with-manifest
    (should (equal (crates-test--annotations)
                   '(("serde = { version = \"1.0\", features = [\"derive\"] }" . "✓ 1.0.219")
                     ("anyhow = \"1\"" . "✓ 1.0.98")
                     ("tokio.version = \"1.2\"" . "↑ 1.45.0")
                     ("\"quoted\" = '0.3'" . "✗ not found on crates.io")
                     ("renamed = { package = \"real-name\", version = \"~2.1\" }" . "→ 3.0.0")
                     ("version = \"=0.1.2\"" . "✗ not found on crates.io")
                     ("proptest = \">=1, <2\"" . "✗ not found on crates.io")
                     ("libc = \"0.2\"" . "✓ 0.2.170")
                     ("regex = \"1.10\"" . "✗ not found on crates.io"))))))

(ert-deftest crates-test-status ()
  (crates-test-with-db
    (clrhash crates--cache)
    (cl-flet ((status (name req)
                (car (crates-status (crates-toml--make-dep :name name :req req)))))
      (should (eq (status "serde" "=1.0.220") 'yanked))
      (should (eq (status "serde" "2") 'no-match))
      (should (eq (status "serde" "not a req") 'invalid))
      (should (eq (status "serde" ">=1.0.0") 'up-to-date))
      (should (eq (status "tokio" "2.0.0-alpha.1") 'up-to-date))
      (should (eq (status "prerelease-only" "0.1.0-rc.1") 'up-to-date)))))

(ert-deftest crates-test-bump-all ()
  (crates-test--with-manifest
    (crates-bump-all)
    (should (string-search "tokio.version = \"1.45\"" (buffer-string)))
    (should (string-search "version = \"~3.0\" }" (buffer-string)))
    (should (string-search "serde = { version = \"1.0\"" (buffer-string)))
    (should (string-search "proptest = \">=1, <2\"" (buffer-string)))))

(ert-deftest crates-test-bump-all-outdated-only ()
  (crates-test--with-manifest
    (crates-bump-all t)
    (should (string-search "tokio.version = \"1.2\"" (buffer-string)))
    (should (string-search "version = \"~3.0\" }" (buffer-string)))))

(ert-deftest crates-test-bump-at-point ()
  (crates-test--with-manifest
    (goto-char (point-min))
    (search-forward "tokio.version")
    (let ((inhibit-message t)) (crates-bump-at-point))
    (should (string-search "tokio.version = \"1.45\"" (buffer-string)))
    (search-forward "local")
    (should-error (crates-bump-at-point) :type 'user-error)))

(ert-deftest crates-test-completion ()
  (crates-test--with-manifest
    (goto-char (point-min))
    (search-forward "real-name\", version = \"~")
    (pcase-let ((`(,beg ,end ,table . ,_) (crates-completion-at-point)))
      (should (equal (buffer-substring-no-properties beg end) "2.1"))
      (should (equal (all-completions "" table) '("3.0.0" "2.1.4"))))
    (goto-char (point-min))
    (search-forward "serde = { version = \"1.")
    (pcase-let ((`(,_ ,_ ,table . ,_) (crates-completion-at-point)))
      ;; 1.0.220 is yanked.
      (should (equal (all-completions "" table) '("1.0.219" "1.0.0"))))))

(ert-deftest crates-test-bumped-req ()
  (should (equal (crates--bumped-req "1.2" "1.45.0") "1.45"))
  (should (equal (crates--bumped-req "^0.3.1" "0.4.2") "^0.4.2"))
  (should (equal (crates--bumped-req "~ 1" "2.0.0") "~ 2"))
  (should (equal (crates--bumped-req ">=1, <2" "2.0.0") "2.0.0")))

(provide 'crates-test)
;;; crates-test.el ends here
