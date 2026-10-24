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

(ert-deftest crates-test-expand-at-point ()
  (crates-test--with-manifest
    (goto-char (point-min))
    (search-forward "anyhow")
    (crates-expand-at-point)
    (should (equal (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                   "anyhow = { version = \"1\" }"))
    (should (looking-at-p " }$"))
    (search-forward "\"quoted\"")
    (crates-expand-at-point)
    (should (string-search "\"quoted\" = { version = '0.3' }" (buffer-string)))
    (search-forward "renamed")
    (should-error (crates-expand-at-point) :type 'user-error)
    (goto-char (point-min))
    (search-forward "tokio.version")
    (should-error (crates-expand-at-point) :type 'user-error)
    (search-forward "local")
    (should-error (crates-expand-at-point) :type 'user-error)))

(ert-deftest crates-test-completion ()
  (crates-test--with-manifest
    (goto-char (point-min))
    (search-forward "real-name\", version = \"~")
    (pcase-let ((`(,beg ,end ,table . ,_) (crates-version-completion-at-point)))
      (should (equal (buffer-substring-no-properties beg end) "2.1"))
      (should (equal (all-completions "" table) '("3.0.0" "2.1.4"))))
    (goto-char (point-min))
    (search-forward "serde = { version = \"1.")
    (pcase-let ((`(,_ ,_ ,table . ,_) (crates-version-completion-at-point)))
      ;; 1.0.220 is yanked.
      (should (equal (all-completions "" table) '("1.0.219" "1.0.0"))))))

(defmacro crates-test--with-text (text &rest body)
  "Run BODY in `crates-mode' on TEXT, with point at the `|' in it."
  (declare (indent 1))
  `(progn
     (crates-test-skip-unless-grammar)
     (crates-test-with-db
       (clrhash crates--cache)
       (with-temp-buffer
         (insert ,text)
         (let ((crates-db-auto-update nil)
               (inhibit-message t))
           (crates-mode))
         (goto-char (point-min))
         (search-forward "|")
         (delete-char -1)
         ,@body))))

(defun crates-test--names ()
  "Return the crate names `crates-name-completion-at-point' offers at point."
  (pcase-let ((`(,beg ,end ,table . ,_) (crates-name-completion-at-point)))
    (all-completions (buffer-substring-no-properties beg end) table)))

(ert-deftest crates-test-name-completion-contexts ()
  (crates-test--with-text "[dependencies]\nse|\n"
    (should (equal (crates-test--names) '("serde"))))
  (crates-test--with-text "[target.'cfg(unix)'.dev-dependencies]\nli|b = \"0.2\"\n"
    (should (equal (crates-test--names) '("libc"))))
  (crates-test--with-text "[workspace.dependencies]\nto|\n"
    (should (equal (crates-test--names) '("tokio"))))
  (crates-test--with-text "[dependencies.re|]\n"
    (should (equal (crates-test--names) '("real-name"))))
  ;; Not offered outside dependency tables, in values, or in comments.
  (crates-test--with-text "[package]\nna|\n"
    (should-not (crates-name-completion-at-point)))
  (crates-test--with-text "[dependencies]\nserde = { features = [\"se|\"] }\n"
    (should-not (crates-name-completion-at-point)))
  (crates-test--with-text "[dependencies]\n# se|\n"
    (should-not (crates-name-completion-at-point))))

(ert-deftest crates-test-name-completion-properties ()
  (crates-test--with-text "[dependencies]\nse|\n"
    (let* ((capf (crates-name-completion-at-point))
           (props (nthcdr 3 capf))
           (table (nth 2 capf)))
      (all-completions "se" table)
      (should (equal (funcall (plist-get props :annotation-function) "serde")
                     "  1.0.219   A serialization framework"))
      (should (equal (funcall (plist-get props :company-docsig) "serde")
                     "A serialization framework"))
      (should (eq (funcall (plist-get props :company-kind) "serde") 'module))
      (with-current-buffer (funcall (plist-get props :company-doc-buffer) "serde")
        (should (string-search "Downloads: 900,000,000" (buffer-string)))
        (should (string-search "https://crates.io/crates/serde" (buffer-string))))
      ;; Completing on an otherwise empty line also writes the version.
      (delete-region (nth 0 capf) (nth 1 capf))
      (insert "serde")
      (funcall (plist-get props :exit-function) "serde" 'finished)
      (should (equal (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position))
                     "serde = \"1.0.219\"")))))

(ert-deftest crates-test-name-completion-keeps-existing-value ()
  (crates-test--with-text "[dependencies]\nse| = \"1\"\n"
    (let* ((capf (crates-name-completion-at-point))
           (props (nthcdr 3 capf)))
      (all-completions "se" (nth 2 capf))
      (goto-char (nth 1 capf))
      (funcall (plist-get props :exit-function) "serde" 'finished)
      (should (equal (buffer-string) "[dependencies]\nse = \"1\"\n")))))

(ert-deftest crates-test-group-digits ()
  (should (equal (crates--group-digits 0) "0"))
  (should (equal (crates--group-digits 1234) "1,234"))
  (should (equal (crates--group-digits 1234567) "1,234,567")))

(ert-deftest crates-test-bumped-req ()
  (should (equal (crates--bumped-req "1.2" "1.45.0") "1.45"))
  (should (equal (crates--bumped-req "^0.3.1" "0.4.2") "^0.4.2"))
  (should (equal (crates--bumped-req "~ 1" "2.0.0") "~ 2"))
  (should (equal (crates--bumped-req ">=1, <2" "2.0.0") "2.0.0")))

(provide 'crates-test)
;;; crates-test.el ends here
