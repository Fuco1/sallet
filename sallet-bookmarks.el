;;; sallet-bookmarks.el --- Bookmarks sallet -*- lexical-binding: t -*-

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
(require 'pcase)

(require 'sallet-source)
(require 'sallet-filters)

(require 'sallet-recentf)


(defun sallet--bookmarks-handler-to-desc (handler)
  (pcase handler
    (`elfeed-search-bookmark-handler "Elfeed search")
    (`Info-bookmark-jump "Info")
    (`bmkp-jump-dired "Dired")
    (`eshell-bookmark--restore "Eshell")
    (_ "File")))

(defun sallet--bookmarks-get-face (type)
  (pcase type
    ("Elfeed search" 'sallet-buffer-special)
    ("Info" 'sallet-buffer-help)
    ("Dired" 'sallet-buffer-directory)
    ("File" 'sallet-buffer-ordinary)))

(defun sallet-bookmarks-candidates ()
  (bookmark-maybe-load-default-file)
  (-map
   (lambda (b)
     (list
      (car b)
      (sallet--bookmarks-handler-to-desc (cdr (assq 'handler (cdr b))))
      (or (cdr (assq 'filename (cdr b))) "")))
   bookmark-alist))

(defun sallet-bookmarks-renderer (data _ user-data)
  (-let (((name type file) data))
    (format "%-45s%15s  %s"
            (sallet-fontify-flx-matches
             (plist-get user-data :flx-matches)
             (propertize name 'face
                         (sallet--bookmarks-get-face type)))
            type
            (propertize file 'face 'sallet-buffer-default-directory))))

(sallet-defsource bookmarks nil
  (candidates sallet-bookmarks-candidates)
  (matcher sallet-matcher-flx)
  (renderer sallet-bookmarks-renderer)
  (action (-lambda (_ (name))
            (bookmark-jump name 'switch-to-buffer)))
  (header "Bookmarks"))

;; TODO: this depends on bookmark+ (`bmkp-file-alist-only',
;; `bmkp-jump-1'), should probably be moved to a different file.
(sallet-defsource bookmarks-file-only nil
  "Bookmarks source, files only."
  (candidates (lambda () (--map
                          (cons
                           (substring-no-properties (car it))
                           (cdr (assoc 'filename (cdr it))))
                          (bmkp-file-alist-only))))
  ;; TODO: enable matching on paths with /
  (matcher sallet-matcher-flx)
  ;; TODO: extract into generic "flx fontify string candidate"
  ;; renderer
  (renderer sallet-recentf-renderer ;; TODO: directory bookmarks should have different color
            ;; (lambda (c _ user-data)
            ;;   (sallet-fontify-flx-matches
            ;;    (plist-get user-data :flx-matches)
            ;;    (car c)))
            )
  (action (-lambda (_source (name))
            ;; TODO: doesn't seem to work
            (bmkp-jump-1 name 'switch-to-buffer nil)))
  (header "Bookmarked files"))

(sallet-defsource bookmarks-file-only-closed-only (bookmarks-file-only)
  "Bookmarks source, files only, closed files only."
  (candidates (lambda () (--keep
                          (let ((path (cdr (assoc 'filename (cdr it)))))
                            (unless (get-file-buffer path)
                              (cons (substring-no-properties (car it)) path)))
                          (bmkp-file-alist-only)))))

(provide 'sallet-bookmarks)
;;; sallet-bookmarks.el ends here
