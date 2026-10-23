;;; crates-db-test.el --- Tests for crates-db  -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises the full update pipeline against the fixture dump, then
;; the queries on the database it produces.

;;; Code:

(require 'ert)
(require 'crates-db)
(require 'crates-test-helpers)

(ert-deftest crates-db-test-build ()
  (crates-test-with-db
    (should (equal (crates-db-meta "schema") crates-db-schema-version))
    (should (equal (crates-db-meta "dump_timestamp") "2026-01-02T03:04:05.000000000Z"))
    (should (< (crates-db-age) 1))
    (should-not (let ((crates-db-max-age 7)) (crates-db-stale-p)))))

(ert-deftest crates-db-test-crate ()
  (crates-test-with-db
    (should (equal (crates-db-crate "serde") '(1 "serde" "1.0.219")))
    (should (equal (crates-db-crate "Serde") '(1 "serde" "1.0.219")))
    (should (equal (crates-db-crate "prerelease-only") '(6 "prerelease-only" "0.1.0-rc.1")))
    (should-not (crates-db-crate "does-not-exist"))))

(ert-deftest crates-db-test-versions ()
  (crates-test-with-db
    (should (equal (sort (crates-db-versions 1) (lambda (a b) (string< (car a) (car b))))
                   '(("1.0.0" nil "2017-01-01 00:00:00+00")
                     ("1.0.219" nil "2025-01-01 00:00:00+00")
                     ("1.0.220" t "2025-06-01 00:00:00+00"))))))

(ert-deftest crates-db-test-search ()
  (crates-test-with-db
    (should (equal (crates-db-search "se")
                   '(("serde" "A serialization framework" 900000000 "1.0.219"))))
    ;; Most downloaded first; LIMIT applies.
    (should (equal (mapcar #'car (crates-db-search ""))
                   '("serde" "libc" "tokio" "anyhow" "real-name" "prerelease-only")))
    (should (equal (mapcar #'car (crates-db-search "" 2)) '("serde" "libc")))
    ;; Case-insensitive, `-' and `_' interchangeable.
    (should (equal (mapcar #'car (crates-db-search "REAL_n")) '("real-name")))
    ;; LIKE's `%' is taken literally.
    (should-not (crates-db-search "%"))))

(ert-deftest crates-db-test-schema ()
  (crates-test-with-db
    (should (crates-db-schema-current-p))
    (let ((crates-db-schema-version "0"))
      (should-not (crates-db-schema-current-p))
      (should (crates-db-stale-p)))))

(ert-deftest crates-db-test-failed-update-keeps-db ()
  (crates-test-skip-unless-tools)
  (let* ((dir (make-temp-file "crates-test-" t))
         (crates-db-file (expand-file-name "crates.sqlite" dir))
         (crates-db-dump-url (concat "file://" dir "/missing.tar.gz"))
         (done 'pending))
    (write-region "previous" nil crates-db-file)
    (crates-db-update (lambda (ok) (setq done ok)))
    (with-timeout (30 (error "Timed out"))
      (while (eq done 'pending) (accept-process-output nil 0.05)))
    (should-not done)
    (should-not (crates-db-update-running-p))
    (should (equal (with-temp-buffer (insert-file-contents crates-db-file) (buffer-string))
                   "previous"))))

(provide 'crates-db-test)
;;; crates-db-test.el ends here
