;;; sallet-imenu.el --- Imenu sallet -*- lexical-binding: t -*-

;; Copyright (C) 2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 10th October 2015
;; Package-requires: ((dash "2.10.0"))
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

(require 'imenu)

(require 'sallet-filters)
(require 'sallet-faces)
(require 'sallet-source)


(defun sallet--imenu-flatten (alist)
  "Flatten an imenu ALIST."
  (--mapcat (if (imenu--subalist-p it)
                (-map (-lambda ((name pos . tags)) (-cons* name pos (car it) tags)) (sallet--imenu-flatten (cdr it)))
              (list (list (car it) (cdr it))))
            alist))

(defun sallet-filter-imenu-tags-flx (candidates indices pattern)
  "Keep buffer CANDIDATES at INDICES flx-matching PATTERN against imenu tags."
  (--keep (sallet-predicate-path-flx
           (mapconcat 'identity (cddr (sallet-aref candidates it)) ", ")
           it pattern) indices))

(defun sallet-imenu-renderer (candidate _state user-data)
  "Render an imenu CANDIDATE."
  (-let* (((x _ . tags) candidate)
          (face (cond ((member "Variables" tags)
                       'font-lock-variable-name-face)
                      ((member "Types" tags)
                       'font-lock-type-face)
                      (t 'font-lock-function-name-face))))
    (format "%-80s%s"
            (sallet-compose-fontifiers
             (propertize x 'face face) user-data
             '(sallet-fontify-regexp-matches . :regexp-matches)
             '(sallet-fontify-flx-matches . :flx-matches))
            (sallet-compose-fontifiers
             (mapconcat 'identity tags ", ") user-data
             '(sallet-fontify-regexp-matches . :regexp-matches-path)
             '(sallet-fontify-flx-matches . :flx-matches-path)))))

(defun sallet-imenu-candidates ()
  "Compute imenu candidates."
  ;; We need to clean the index for `imenu--make-index-alist' to
  ;; refresh.
  (setq imenu--index-alist nil)
  (let ((initial (symbol-name (symbol-at-point)))
        (cands (--map (if (cddr it) it (-snoc it ""))
                      (--remove
                       (or (not (integer-or-marker-p (cadr it)))
                           (< (cadr it) 0))
                       (sallet--imenu-flatten (imenu--make-index-alist))))))
    (if initial
        (-if-let (initial (--first (equal (car it) initial) cands))
            (cons initial (--remove (equal initial it) cands))
          cands)
      cands)))

(sallet-defsource imenu nil
  "Imenu."
  (candidates sallet-imenu-candidates)
  (matcher (sallet-make-matcher
            (lambda (c i p)
              ;; TODO: maybe just search in name and tags by default
              ;; but prioritize matches in the name first
              (sallet-compose-filters-by-pattern
               '(("\\`/\\(.*\\)" 1 sallet-filter-imenu-tags-flx)
                 (t sallet-filter-flx-then-substring)) c i p))))
  (sorter sallet-sorter-flx)
  (renderer sallet-imenu-renderer)
  (action (-lambda (_source (_ pos))
            (cond
             ((eq major-mode 'org-mode)
              (goto-char pos)
              (org-show-context)
              (org-show-entry))
             (t (goto-char pos)))))
  (header "Imenu"))

(provide 'sallet-imenu)
;;; sallet-imenu.el ends here
