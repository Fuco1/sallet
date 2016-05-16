;;; sallet-registers.el --- Register sallet -*- lexical-binding: t -*-

;; Copyright (C) 2016 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 16th May 2016
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

(require 'sallet-source)
(require 'sallet-faces)

(sallet-defsource register-point nil
  "Sallet for navigating to point registers.

Point registers (set by \\[point-to-register]) serve as transient bookmarks.

Each is identified by the register, which is a number or a letter."
  (candidates (lambda ()
                (--keep
                 (let* ((marker (cdr it))
                        (buffer (marker-buffer marker)))
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (save-excursion
                         (goto-char marker)
                         (let ((line-number (line-number-at-pos)))
                           (list (s-trim (thing-at-point 'line)) line-number (buffer-name buffer) it))))))
                 (--filter (markerp (cdr it)) register-alist))))
  (matcher sallet-matcher-flx-then-substring)
  (renderer (-lambda ((line line-number buffer-name) _ user-data)
              (format "% 5d:%-30s%-80s"
                      line-number
                      buffer-name
                      (sallet-compose-fontifiers
                       line user-data
                       '(sallet-fontify-regexp-matches . :regexp-matches)
                       '(sallet-fontify-flx-matches . :flx-matches)))))
  (action (-lambda (_source (_ _ _ (register)))
            (message "%s" register)
            (jump-to-register register)))
  (header "Point registers"))

(provide 'sallet-registers)
;;; sallet-registers.el ends here
