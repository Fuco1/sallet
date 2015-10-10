;;; sallet-recentf.el --- Recentf sallet -*- lexical-binding: t -*-

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
(require 'sallet-faces)

(defface sallet-recentf-buffer-name
  '((t (:inherit font-lock-builtin-face)))
  "Face used to fontify recentf buffer name."
  :group 'sallet-faces)

(defface sallet-recentf-file-path
  '((t (:inherit sallet-buffer-default-directory)))
  "Face used to fontify recentf file path."
  :group 'sallet-faces)

;; TODO: faces should come as optional parameters, this should be called "bookmark cons" renderer
(defun sallet-recentf-renderer (candidate _ user-data)
  "Render a recentf candidate."
  (-let (((name . file) candidate))
    (format "%-50s%s"
            (sallet-fontify-flx-matches
             (plist-get user-data :flx-matches)
             (propertize name 'face 'sallet-recentf-buffer-name))
            (propertize (abbreviate-file-name file) 'face 'sallet-recentf-file-path))))

(sallet-defsource recentf nil
  "Files saved on `recentf-list'."
  (candidates (lambda ()
                (unless recentf-mode (recentf-mode 1))
                (--map
                 (let ((name (file-name-nondirectory it)))
                   (cons name it))
                 recentf-list)))
  ;; TODO: add matching on path with /
  (matcher sallet-matcher-flx)
  (renderer sallet-recentf-renderer)
  (action (-lambda ((_ . file)) (find-file file)))
  (header "Recently opened files"))

(sallet-defsource recentf-closed-only (recentf)
  "Files saved on `recentf-list', but without those whose buffer is already opened."
  (candidates (lambda ()
                (unless recentf-mode (recentf-mode 1))
                (--keep
                 (let ((name (file-name-nondirectory it)))
                   (unless (get-file-buffer it)
                     (cons name it)))
                 recentf-list))))

(provide 'sallet-recentf)
;;; sallet-recentf.el ends here
