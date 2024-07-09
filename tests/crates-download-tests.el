;; -*- lexical-binding: t -*-

(ert-deftest test-crates-download-db ()
  (setq crates-download-tmp-dir (make-temp-file "crates-download-tmp" t))
  (setq crates-download-dest-dir (make-temp-file "crates-download-result" t))
  (crates-download--db-sync)

  ;; downloaded archive should exist
  (should (file-exist-p crates-download-tmp-archive-path))
  ;; extracted files should exist
  (should (file-exist-p crates-download-tmp-dump-dir))
  ;; moved at destination dir with /data in it should exist
  (should (file-exist-p (concat crates-download-dest-dir "/data"))))
