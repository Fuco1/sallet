;;; sallet-autobookmarks.el --- Autobookmarks sallet -*- lexical-binding: t -*-

;; Copyright (C) 2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 10th October 2015
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)

(require 'sallet-source)
(require 'sallet-state)
(require 'sallet-filters)
(require 'sallet-faces)

(defun sallet-filter-autobookmark-path-substring (candidates indices pattern)
  "Keep autobookmark CANDIDATES substring-matching PATTERN against file path."
  (let ((quoted-pattern (regexp-quote pattern)))
    (--keep (sallet-predicate-path-regexp (cadr (sallet-aref candidates it)) it quoted-pattern) indices)))

(defun sallet-filter-autobookmark-path-flx (candidates indices pattern)
  "Keep autobookmark CANDIDATES flx-matching PATTERN against file path."
  (--keep (sallet-predicate-path-flx (cadr (sallet-aref candidates it)) it pattern) indices))

(defun sallet-filter-autobookmark-mode-flx (candidates indices pattern)
  "Keep autobookmark CANDIDATES flx-matching PATTERN against the major-mode they would open in."
  (--keep (sallet-predicate-buffer-major-mode
           (-let* (((_ _ . (&alist 'abm-auto-major-mode mm)) (sallet-aref candidates it))) mm)
           it pattern) indices))

(defun sallet-autobookmarks-matcher (candidates state)
  "Match autobookmark CANDIDATES using special rules.

First, the prompt is split on whitespace.  This creates a list of
patterns.

A pattern starting with / flx-matches against the path to the
file bookmark represents.

A pattern starting with // substring-matches against the path to the
file bookmark represents.

A pattern starting with * flx-matches against the major mode the
bookmark would open in.  This is guessed using `auto-mode-alist'.

Any other non-prefixed pattern is matched using the following rules:

- If the pattern is first of this type at the prompt, it is
  flx-matched against the bookmark name.
- All the following patterns are substring matched against the
  bookmark name."
  (let* ((prompt (sallet-state-get-prompt state))
         (indices (sallet-make-candidate-indices candidates)))
    (sallet-compose-filters-by-pattern
     '(("\\`//\\(.*\\)" 1 sallet-filter-autobookmark-path-substring)
       ("\\`/\\(.*\\)" 1 sallet-filter-autobookmark-path-flx)
       ("\\`\\*\\(.*\\)" 1 sallet-filter-autobookmark-mode-flx)
       (t sallet-filter-flx-then-substring))
     candidates
     indices
     prompt)))

;; TODO: improve
(defun sallet-autobookmarks-renderer (candidate _state user-data)
  "Render an `autobookmarks-mode' CANDIDATE."
  (-let* (((name path . data) candidate)
          ((&alist 'visits visits) data))
    (format "%-55s%5s  %s"
            (sallet-compose-fontifiers
             ;; TODO: create a "fontify flx after regexp" function to
             ;; simplify this common pattern
             (propertize name 'face 'sallet-recentf-buffer-name) user-data
             '(sallet-fontify-regexp-matches . :regexp-matches)
             '(sallet-fontify-flx-matches . :flx-matches))
            (propertize (if visits (int-to-string visits) "0") 'face 'sallet-buffer-size)
            (abbreviate-file-name
             (sallet-compose-fontifiers
              (propertize path 'face 'sallet-recentf-file-path) user-data
              '(sallet-fontify-regexp-matches . :regexp-matches-path)
              '(sallet-fontify-flx-matches . :flx-matches-path))))))


(defvar sallet-autobookmarks--name-to-major-mode-cache (make-hash-table :test 'equal)
  "Name-to-major-mode cache.")

(defun sallet-autobookmarks--name-to-major-mode (name)
  "Return `major-mode' in which file with NAME would open."
  (-if-let (mm (gethash name sallet-autobookmarks--name-to-major-mode-cache)) mm
    (puthash name
             (cond
              ((string-match-p "/\\'" name)
               "dired-mode")
              (t (catch 'match
                   (--each auto-mode-alist
                     (when (string-match-p (car it) name)
                       (throw 'match (symbol-name
                                      (if (listp (cdr it))
                                          (-last-item (cdr it))
                                        (cdr it)))))))))
             sallet-autobookmarks--name-to-major-mode-cache)))

(sallet-defsource autobookmarks nil
  "Files saved with `autobookmarks-mode'."
  (candidates (lambda ()
                (require 'autobookmarks)
                (-keep
                 (lambda (bookmark)
                   (-when-let (name
                               (cond
                                ((assoc 'defaults (cdr bookmark))
                                 (cadr (assoc 'defaults (cdr bookmark))))
                                ((assoc 'filename (cdr bookmark))
                                 (f-filename
                                  (cdr (assoc 'filename (cdr bookmark)))))))
                     (when (string-match-p "/\\'" (car bookmark))
                       (setq name (concat name "/")))
                     (-snoc (cons name bookmark)
                            (cons
                             'abm-auto-major-mode
                             (sallet-autobookmarks--name-to-major-mode name)))))
                 (-sort (-lambda ((_ . (&alist 'time a))
                                  (_ . (&alist 'time b)))
                          (time-less-p b a))
                        (abm-recent-buffers)))))
  (matcher sallet-autobookmarks-matcher)
  (renderer sallet-autobookmarks-renderer)
  (action (-lambda (_source (_ . x)) (abm-restore-killed-buffer x)))
  (header "Autobookmarks"))

(provide 'sallet-autobookmarks)
;;; sallet-autobookmarks.el ends here
