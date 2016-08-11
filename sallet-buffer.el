;;; sallet-buffer.el --- Sallet for picking buffers -*- lexical-binding: t -*-

;; Copyright (C) 2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 14th September 2015
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
(require 's)

(require 'ibuffer)
(require 'imenu)

(require 'sallet-faces)
(require 'sallet-filters)
(require 'sallet-state)
(require 'sallet-source)

;; TODO: create a customize group just for sources
(defcustom sallet-buffer-sources '(sallet-source-buffer)
  "Sources for `sallet-buffer'.

Since `sallet' does not make any artificial distinctions between
sources, you can put any source here.  However, keeping it
thematic and related to buffers is probably a good idea."
  :group 'sallet
  :type '(repeat symbol))


;; Buffer predicates and filters

(defun sallet-predicate-buffer-imenu (candidate index pattern)
  "Check if buffer has an `imenu' item flx-matching pattern.

CANDIDATE is a buffer or buffer name.

INDEX is its index and associated meta data.

PATTERN is a string flx-matched against imenu items.

Returns updated INDEX with optional added metadata or nil if this
candidate should not pass the filter."
  (when (with-current-buffer candidate
          (let ((imenu-alist-flat
                 ;; TODO: cache the flattened
                 ;; alists so we don't have to
                 ;; recomute on every inserted letter.
                 (-flatten (--tree-map (if (stringp it) nil (car it))
                                       ;; TODO: make sure the alist is initialized
                                       imenu--index-alist))))
            ;; TODO: add list of matching imenu items as metadata so
            ;; we can render that somehow in the list?
            (--any? (flx-score it pattern) imenu-alist-flat)))
    index))

(defun sallet-filter-buffer-imenu (candidates indices pattern)
  "Keep buffer CANDIDATES at INDICES flx-matching PATTERN against an imenu item."
  (--keep (sallet-predicate-buffer-imenu (sallet-candidate-aref candidates it) it pattern) indices))

(defun sallet-filter-buffer-major-mode (candidates indices pattern)
  "Keep buffer CANDIDATES at INDICES flx-matching PATTERN against current `major-mode'."
  (--keep (sallet-predicate-buffer-major-mode
           (with-current-buffer (sallet-candidate-aref candidates it) (symbol-name major-mode))
           it pattern) indices))

(defun sallet-predicate-buffer-fulltext (candidate index pattern)
  "Check if buffer's `buffer-string' regexp-matches pattern.

CANDIDATE is a buffer or buffer name.

INDEX is its index and associated meta data.

PATTERN is a regexp matched against `buffer-string'.

Returns updated INDEX with optional added metadata or nil if this
candidate should not pass the filter."
  (when (with-current-buffer candidate
          (save-excursion
            (goto-char (point-min))
            (re-search-forward pattern nil t)))
    index))

(defun sallet-filter-buffer-fulltext (candidates indices pattern)
  "Keep buffer CANDIDATES at INDICES regexp-matching PATTERN against `buffer-string'."
  (--keep (sallet-predicate-buffer-fulltext (sallet-candidate-aref candidates it) it pattern) indices))

(defun sallet-filter-buffer-default-directory-flx (candidates indices pattern)
  "Keep buffer CANDIDATES at INDICES flx-matching PATTERN against `default-directory'."
  (--keep (sallet-predicate-path-flx
           (with-current-buffer (sallet-candidate-aref candidates it) default-directory)
           it pattern) indices))

(defun sallet-filter-buffer-default-directory-substring (candidates indices pattern)
  "Keep buffer CANDIDATES at INDICES substring-matching PATTERN against `default-directory'."
  (let ((quoted-pattern (regexp-quote pattern)))
    ;; TODO: replace with specialized file substring matcher
    (--keep (sallet-predicate-path-regexp
             (with-current-buffer (sallet-candidate-aref candidates it) default-directory)
             it quoted-pattern) indices)))



(defun sallet-buffer-fontify-buffer-name (candidate)
  "Fontify buffer CANDIDATE's name."
  (with-current-buffer candidate
    (let ((face (cond
                 ((and (buffer-file-name)
                       (buffer-modified-p))
                  'sallet-buffer-modified)
                 ((eq major-mode (quote dired-mode)) 'sallet-buffer-directory)
                 ((memq major-mode ibuffer-help-buffer-modes) 'sallet-buffer-help)
                 ((string-match-p "^*" (buffer-name)) 'sallet-buffer-special)
                 ((and buffer-file-name
                       (string-match-p ibuffer-compressed-file-name-regexp buffer-file-name))
                  'sallet-buffer-compressed)
                 (buffer-read-only 'sallet-buffer-read-only)
                 (t 'sallet-buffer-ordinary))))
      (propertize (buffer-name) 'face face))))

(defun sallet-buffer-renderer (candidate _ user-data)
  "Render a buffer CANDIDATE."
  (with-current-buffer candidate
    ;; TODO: make the column widths configurable
    (format "%-50s%10s  %20s  %s"
            (truncate-string-to-width
             (sallet-compose-fontifiers
              candidate user-data
              'sallet-buffer-fontify-buffer-name
              '(sallet-fontify-regexp-matches . :regexp-matches)
              '(sallet-fontify-flx-matches . :flx-matches))
             50 nil nil t)
            (propertize (file-size-human-readable (buffer-size)) 'face 'sallet-buffer-size)
            (truncate-string-to-width
             (s-chop-suffix "-mode"
                            (sallet-fontify-flx-matches
                             (plist-get user-data :flx-matches-mm)
                             (symbol-name major-mode)))
             20 nil nil t)
            (format (propertize
                     (concat "(" (or (and (buffer-file-name) (concat "in %s"))
                                     (-when-let (process (get-buffer-process (current-buffer)))
                                       (concat (process-name process)
                                               " run in %s"))
                                     "%s")
                             ")")
                     'face
                     'sallet-buffer-default-directory)
                    (sallet-compose-fontifiers
                     default-directory user-data
                     '(sallet-fontify-regexp-matches . :regexp-matches-path)
                     '(sallet-fontify-flx-matches . :flx-matches-path))))))

(defun sallet-buffer-matcher (candidates state)
  "Match a buffer candidate using special rules.

CANDIDATES are buffer names.

First, the prompt is split on whitespace.  This creates a list of
patterns.

A pattern starting with * is flx-matched against the `major-mode'.

A pattern starting with @ is flx-matched against the
`imenu--index-alist' entries.  These are usually names of
classes, functions, variables defined in the file.

A pattern starting with # does a full-text regexp search inside
the buffer.

A pattern starting with / flx-matches against the default directory.

Any other non-prefixed pattern is matched using the following rules:

- If the pattern is first of this type at the prompt, it is
  flx-matched against the buffer name.
- All the following patterns are substring matched against the
  buffer name."
  (let* ((prompt (sallet-state-get-prompt state))
         (indices (sallet-make-candidate-indices candidates)))
    ;; TODO: add . prefix to match on file extension
    ;; TODO: add gtags filter?
    (sallet-compose-filters-by-pattern
     '(("\\`\\*\\(.*\\)" 1 sallet-filter-buffer-major-mode)
       ("\\`@\\(.*\\)" 1 sallet-filter-buffer-imenu)
       ("\\`#\\(.*\\)" 1 sallet-filter-buffer-fulltext)
       ("\\`//\\(.*\\)" 1 sallet-filter-buffer-default-directory-substring)
       ("\\`/\\(.*\\)" 1 sallet-filter-buffer-default-directory-flx)
       (t sallet-filter-flx-then-substring))
     candidates
     indices
     prompt)))

(defun sallet-buffer-candidates ()
  (let ((buffers
         ;; TODO: preprocess candidates to include
         ;; major-mode and directory so we don't have to
         ;; query it multiple times (in filtering and
         ;; rendering)
         (--keep (let ((name (buffer-name it)))
                   ;; TODO: add a variable where users
                   ;; can write regexps to exclude
                   ;; buffers
                   (unless (string-match-p "^ " name) name))
                 (buffer-list))))
    (if (< 1 (length buffers))
        ;; swap the current buffer with the last
        ;; recently visited other buffer, so we default
        ;; to toggling
        (-cons* (cadr buffers) (car buffers) (cddr buffers))
      buffers)))

;; TODO: sorting is now done the same way as `buffer-list' returns the
;; results, in LRU order.  We should also try to add some weight to
;; the flx score.  One possibility is to add +100, and decreasing, to
;; the flx-score for more recent buffers.
(sallet-defsource buffer nil
  "Buffer source."
  (candidates sallet-buffer-candidates)
  (matcher sallet-buffer-matcher)
  (action (lambda (_source c) (switch-to-buffer c)))
  (header "Buffers")
  (renderer sallet-buffer-renderer))

;; TODO: remove duplication with `buffer' source
(sallet-defsource similar-buffer (buffer)
  "Buffers with the same name but in a different file hierarchy."
  (candidates (lambda ()
                (let* ((current-name (cond
                                      ((and (featurep 'uniquify)
                                            (uniquify-buffer-base-name)))
                                      ((buffer-name))))
                       (buffers
                        (--keep (let ((name (cond
                                             ((and (featurep 'uniquify)
                                                   (with-current-buffer it
                                                     (uniquify-buffer-base-name))))
                                             ((buffer-name it)))))
                                  (when (string= name current-name) (buffer-name it)))
                                (buffer-list))))
                  (when (< 1 (length buffers))
                    (-cons* (cadr buffers) (car buffers) (cddr buffers))))))
  (header "Similar buffers"))

(provide 'sallet-buffer)
;;; sallet-buffer.el ends here
