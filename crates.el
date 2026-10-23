;;; crates.el --- Show newer crates.io versions in Cargo.toml  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kyrylo Baibula

;; Author: Kyrylo Baibula <kyrylobaybula@gmail.com>
;; Maintainer: Kyrylo Baibula <kyrylobaybula@gmail.com>
;; URL: https://github.com/Velnbur/crates.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, languages, rust
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `crates-mode' is a major mode for Cargo manifests, derived
;; from `toml-ts-mode'.  Next to every dependency it shows the latest
;; version published on crates.io and whether the requirement allows
;; it.  It also completes version strings and bumps requirements.
;;
;; Versions are looked up offline, in a SQLite database built from the
;; daily crates.io dump with `crates-db-update'.  Building it needs
;; `curl', `tar' and `sqlite3'; see the README for details.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'crates-db)
(require 'crates-semver)
(require 'crates-toml)
(require 'toml-ts-mode)
(require 'url-util)

;;;; Customization

(defface crates-up-to-date
  '((t :inherit shadow))
  "Face for dependencies whose requirement names the latest version."
  :group 'crates)

(defface crates-compatible-update
  '((t :inherit success))
  "Face for dependencies whose requirement allows a newer version than it names."
  :group 'crates)

(defface crates-outdated
  '((t :inherit warning))
  "Face for dependencies whose requirement excludes the latest version."
  :group 'crates)

(defface crates-error
  '((t :inherit error))
  "Face for dependencies that do not resolve to a usable version."
  :group 'crates)

(defcustom crates-indicators
  '((up-to-date . "✓") (compatible . "↑") (outdated . "→") (error . "✗"))
  "Symbols put in front of the version shown next to a dependency."
  :type '(alist :key-type symbol :value-type string)
  :group 'crates)

(defcustom crates-annotate-delay 0.3
  "Seconds of idle time after an edit before annotations are refreshed."
  :type 'number
  :group 'crates)

;;;; Looking up crates

(defvar crates--cache (make-hash-table :test #'equal)
  "Crate name -> info plist, or `missing'.  Cleared when the database changes.")

(defun crates--info (name)
  "Return a plist describing crate NAME, or nil when it is unknown.
Keys are :name, :default and :versions, the latter a list of
\(VERSION NUM YANKED CREATED-AT) sorted newest first, VERSION being a
parsed `crates-semver-version'."
  (let ((cached (gethash name crates--cache)))
    (if cached
        (and (not (eq cached 'missing)) cached)
      (let* ((row (crates-db-crate name))
             (info (and row
                        (list :name (nth 1 row)
                              :default (nth 2 row)
                              :versions
                              (sort (cl-loop for (num yanked created) in (crates-db-versions (nth 0 row))
                                             for v = (crates-semver-parse num)
                                             when v collect (list v num yanked created))
                                    (lambda (a b) (crates-semver< (car b) (car a))))))))
        (puthash name (or info 'missing) crates--cache)
        info))))

(defun crates--latest (info)
  "Return the latest version string of the crate described by INFO.
That is crates.io's default version: the newest non-yanked stable
release, else the newest non-yanked version."
  (or (plist-get info :default)
      (nth 1 (cl-find-if-not #'caddr (plist-get info :versions)))))

(defun crates--newest-prerelease (info)
  "Return the newest non-yanked pre-release of INFO if it beats the latest."
  (let ((latest (crates-semver-parse (crates--latest info)))
        (pre (cl-find-if (lambda (e) (and (not (nth 2 e))
                                          (crates-semver-version-pre (car e))))
                         (plist-get info :versions))))
    (when (and pre (or (null latest) (crates-semver< latest (car pre))))
      (nth 1 pre))))

(defun crates--req-version (req)
  "Return the version text of simple requirement REQ, without operator."
  (let ((simple (crates-semver-simple-req-p req)))
    (and simple (string-trim (substring req (length (car simple)))))))

(defun crates--bumped-req (req latest)
  "Return requirement REQ rewritten to allow LATEST, in the same style.
A bare, caret or tilde requirement keeps its operator and precision;
anything else becomes LATEST."
  (let ((simple (crates-semver-simple-req-p req)))
    (if simple
        (concat (car simple) (crates-semver-truncate latest (cdr simple)))
      latest)))

(defun crates-status (dep)
  "Return (STATUS LATEST MESSAGE) for the dependency DEP.
STATUS is one of `up-to-date', `compatible', `outdated', `unknown',
`invalid', `yanked' and `no-match'."
  (let* ((info (crates--info (crates-toml-dep-crate dep)))
         (req-text (crates-toml-dep-req dep))
         (req (crates-semver-parse-req req-text))
         (latest (and info (crates--latest info)))
         (latest-v (and latest (crates-semver-parse latest))))
    (cond
     ((null info) (list 'unknown nil "not found on crates.io"))
     ((null req) (list 'invalid latest "invalid version requirement"))
     ((null latest-v) (list 'no-match nil "every version is yanked"))
     (t
      (let* ((versions (plist-get info :versions))
             (matching (cl-find-if (lambda (e) (and (not (nth 2 e))
                                                    (crates-semver-satisfies-p req (car e))))
                                   versions)))
        (cond
         ((null matching)
          (if (cl-find-if (lambda (e) (crates-semver-satisfies-p req (car e))) versions)
              (list 'yanked latest "every matching version is yanked")
            (list 'no-match latest "no published version matches")))
         ((crates-semver< (car matching) latest-v)
          (list 'outdated latest (format "%s excludes %s" req-text latest)))
         ((equal (crates--bumped-req req-text latest) req-text)
          (list 'up-to-date latest "up to date"))
         ((crates-semver-simple-req-p req-text)
          (list 'compatible latest (format "%s allows %s" req-text latest)))
         (t (list 'up-to-date latest "up to date"))))))))

;;;; Annotations

(defun crates--indicator (status)
  "Return the indicator string and face for STATUS as a cons."
  (pcase status
    ('up-to-date (cons (alist-get 'up-to-date crates-indicators) 'crates-up-to-date))
    ('compatible (cons (alist-get 'compatible crates-indicators) 'crates-compatible-update))
    ('outdated (cons (alist-get 'outdated crates-indicators) 'crates-outdated))
    (_ (cons (alist-get 'error crates-indicators) 'crates-error))))

(defun crates--help-echo (dep status latest message info)
  "Return the tooltip for DEP with STATUS, LATEST, MESSAGE and INFO."
  (string-join
   (delq nil
         (list (format "%s: %s" (crates-toml-dep-crate dep) message)
               (and latest (format "Latest: %s" latest))
               (and info (crates--newest-prerelease info)
                    (format "Newest pre-release: %s" (crates--newest-prerelease info)))
               (and (memq status '(compatible outdated))
                    (substitute-command-keys "\\[crates-bump-at-point] to bump"))
               (and (crates-db-meta "dump_timestamp")
                    (format "Data from the crates.io dump of %s"
                            (crates-db-meta "dump_timestamp")))))
   "\n"))

(defun crates--annotation (dep)
  "Return the text displayed after DEP, or nil."
  (pcase-let* ((`(,status ,latest ,message) (crates-status dep))
               (`(,indicator . ,face) (crates--indicator status))
               (label (if (memq status '(up-to-date compatible outdated))
                          latest
                        message)))
    (propertize (concat "  " indicator " " label)
                'face face
                'help-echo (crates--help-echo dep status latest message
                                              (crates--info (crates-toml-dep-crate dep))))))

(defun crates-clear ()
  "Remove the version annotations from the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'crates t))

(defun crates-refresh ()
  "Re-check every dependency in the current buffer."
  (interactive)
  (crates-clear)
  (when (and (crates-db-available-p) (crates-toml-parser))
    (save-excursion
      (dolist (dep (crates-toml-dependencies))
        (when (crates-toml-registry-dep-p dep)
          (goto-char (crates-toml-dep-req-end dep))
          (let ((ov (make-overlay (line-end-position) (line-end-position))))
            (overlay-put ov 'crates t)
            (overlay-put ov 'after-string (crates--annotation dep))))))))

(defvar-local crates--timer nil
  "Idle timer that will refresh the annotations of this buffer.")

(defun crates--after-change (&rest _)
  "Schedule a refresh of the annotations after an edit."
  (when crates--timer (cancel-timer crates--timer))
  (setq crates--timer
        (run-with-idle-timer crates-annotate-delay nil #'crates--refresh-buffer
                             (current-buffer))))

(defun crates--refresh-buffer (buffer)
  "Refresh the annotations in BUFFER if it is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq crates--timer nil)
      (crates-refresh))))

(defun crates--refresh-all ()
  "Forget cached crate data and refresh every Cargo manifest buffer."
  (clrhash crates--cache)
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'crates-mode)
      (crates--refresh-buffer buffer))))

(add-hook 'crates-db-after-update-hook #'crates--refresh-all)

(defun crates--mode-line ()
  "Return the database status for the mode line."
  (cond
   ((crates-db-update-running-p) " [updating]")
   ((not (file-exists-p crates-db-file)) " [no db]")
   ((crates-db-stale-p) (format " [db %dd]" (crates-db-age)))))

;;;; Dependency at point

(defun crates--dep-at-point ()
  "Return the dependency whose declaration spans point, or nil."
  (let ((pos (point)))
    (cl-find-if (lambda (dep)
                  (and (crates-toml-dep-req dep)
                       (<= (save-excursion (goto-char (crates-toml-dep-key-beg dep))
                                           (line-beginning-position))
                           pos
                           (save-excursion (goto-char (crates-toml-dep-req-end dep))
                                           (line-end-position)))))
                (crates-toml-dependencies))))

(defun crates--bump (dep)
  "Rewrite the requirement of DEP to allow the latest version.
Return non-nil if the buffer changed."
  (let* ((info (crates--info (crates-toml-dep-crate dep)))
         (latest (and info (crates--latest info)))
         (old (crates-toml-dep-req dep))
         (new (and latest (crates--bumped-req old latest))))
    (when (and new (not (equal new old)))
      (save-excursion
        (goto-char (crates-toml-dep-req-beg dep))
        (delete-region (crates-toml-dep-req-beg dep) (crates-toml-dep-req-end dep))
        (insert new))
      t)))

(defun crates-bump-at-point ()
  "Update the requirement of the dependency at point to the latest version."
  (interactive)
  (let ((dep (or (crates--dep-at-point) (user-error "No dependency at point"))))
    (unless (crates-toml-registry-dep-p dep)
      (user-error "%s does not come from crates.io" (crates-toml-dep-name dep)))
    (unless (crates--info (crates-toml-dep-crate dep))
      (user-error "%s is not in the crates.io database" (crates-toml-dep-crate dep)))
    (if (crates--bump dep)
        (message "Bumped %s" (crates-toml-dep-name dep))
      (message "%s is already up to date" (crates-toml-dep-name dep)))))

(defun crates-bump-all (&optional outdated-only)
  "Update every simple requirement in the buffer to the latest version.
Requirements that are not a bare, caret or tilde version are left
alone.  With prefix argument OUTDATED-ONLY, only touch those that
exclude the latest version."
  (interactive "P")
  (let ((count 0))
    ;; Back to front, so earlier positions stay valid.
    (dolist (dep (reverse (crates-toml-dependencies)))
      (when (and (crates-toml-registry-dep-p dep)
                 (crates-semver-simple-req-p (crates-toml-dep-req dep))
                 (or (not outdated-only)
                     (eq (car (crates-status dep)) 'outdated))
                 (crates--bump dep))
        (cl-incf count)))
    (message "Bumped %d dependenc%s" count (if (= count 1) "y" "ies"))))

(defun crates-browse ()
  "Open the crates.io page of the dependency at point."
  (interactive)
  (let ((dep (or (crates--dep-at-point) (user-error "No dependency at point"))))
    (browse-url (concat "https://crates.io/crates/"
                        (url-hexify-string (crates-toml-dep-crate dep))))))

;;;; Completion

(defconst crates--operator-re "\\`[ \t]*\\(?:\\^\\|~\\|>=\\|<=\\|>\\|<\\|=\\)?[ \t]*"
  "Regexp matching the operator in front of a requirement's version.")

(defun crates-completion-at-point ()
  "Complete the version in the requirement string at point.
Candidates are the crate's non-yanked versions, newest first."
  (when-let* ((dep (cl-find-if (lambda (dep)
                                 (and (crates-toml-dep-req dep)
                                      (<= (crates-toml-dep-req-beg dep) (point)
                                          (crates-toml-dep-req-end dep))))
                               (crates-toml-dependencies)))
              (info (crates--info (crates-toml-dep-crate dep))))
    (let* ((beg (crates-toml-dep-req-beg dep))
           (text (buffer-substring-no-properties beg (point)))
           (start (if (and (string-match crates--operator-re text)
                           (not (string-search "," text)))
                      (+ beg (match-end 0))
                    (point)))
           (entries (cl-remove-if #'caddr (plist-get info :versions)))
           (dates (mapcar (lambda (e) (cons (nth 1 e) (nth 3 e))) entries))
           (candidates (mapcar #'cadr entries)))
      (list start (crates-toml-dep-req-end dep)
            (lambda (string pred action)
              (if (eq action 'metadata)
                  '(metadata (display-sort-function . identity)
                             (cycle-sort-function . identity))
                (complete-with-action action candidates string pred)))
            :exclusive 'no
            :annotation-function
            (lambda (candidate)
              (when-let* ((date (cdr (assoc candidate dates))))
                (concat "  " (substring date 0 (min 10 (length date))))))))))

;;;; Mode

(defvar-keymap crates-mode-map
  :doc "Keymap for `crates-mode'."
  "C-c C-u" #'crates-bump-at-point
  "C-c C-a" #'crates-bump-all
  "C-c C-o" #'crates-browse
  "C-c C-r" #'crates-refresh)

(defun crates--maybe-hint ()
  "Explain how to get a database when there is none."
  (unless (or (file-exists-p crates-db-file) (crates-db-update-running-p))
    (message (substitute-command-keys
              "crates: no crates.io database yet; build one with \\[crates-db-update]"))))

;;;###autoload
(define-derived-mode crates-mode toml-ts-mode "Cargo"
  "Major mode for Cargo manifests, showing crates.io versions.
Each dependency is annotated with the latest version on crates.io,
looked up in the local database (see `crates-db-update').

\\{crates-mode-map}"
  (setq-local mode-line-process '(:eval (crates--mode-line)))
  (add-hook 'after-change-functions #'crates--after-change nil t)
  (add-hook 'completion-at-point-functions #'crates-completion-at-point nil t)
  (crates-db-maybe-update)
  (crates--maybe-hint)
  (crates-refresh))

;;;###autoload
(add-to-list 'auto-mode-alist '("/Cargo\\.toml\\'" . crates-mode))

(provide 'crates)
;;; crates.el ends here
