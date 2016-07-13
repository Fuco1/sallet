;;; sallet-ag.el --- Sallet for ag -*- lexical-binding: t -*-

;; Copyright (C) 2016 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 13th July 2016
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

(require 's)

(require 'sallet-source)

;; TODO: add a decorator to "start process in a directory"
;; TODO: see `sallet-grep-make-process-creator'.
(defun sallet-ag-make-process-creator (root)
  "Return a process creator for gtags-files sallet.

ROOT is the directory from where we launch ag(1)."
  (lambda (prompt)
    (with-temp-buffer
      (cd root)
      (start-process
       "ag" nil "ag"
       "--nocolor" "--literal" "--line-number" "--smart-case"
       "--nogroup" "--column" prompt))))

(defun sallet-ag-files-make-process-creator (root)
  "Return a process creator for ag-files sallet.

ROOT is the directory from where we launch ag(1)."
  (lambda (prompt)
    (with-temp-buffer
      (cd root)
      (start-process
       "ag" nil "ag" "--nocolor" "--literal"
       "--smart-case" "-g" prompt))))

(defun sallet-ag-processor (input)
  (-let (((file line column content) (s-split-up-to ":" input 4)))
    (list content file line column)))

;; TODO: match only on content, add / matcher for path.  We should
;; acomplish this by generating better candidates, not just lines
;; (identity)
(sallet-defsource ag (asyncio)
  "Grep."
  (generator
   (lambda (source state)
     (funcall
      (sallet-make-generator-linewise-asyncio
       (sallet-process-creator-first-token-only
        (sallet-ag-make-process-creator (oref source search-root)))
       'sallet-ag-processor)
      source state)))
  (search-root)
  (init 'sallet--set-search-root)
  (renderer (-lambda ((content file line column) _ user-data)
              (format "%s:%s:%s:%s"
                      file line column
                      (sallet-fontify-regexp-matches
                       (plist-get user-data :regexp-matches)
                       content))))
  (action (-lambda (source (_ file line column))
            (find-file (concat (oref source search-root) file))
            (widen)
            (goto-char (point-min))
            (forward-line (1- (string-to-number line)))
            (forward-char (1- (string-to-number column))))))

;; TODO: add "search-root" source/parent
(sallet-defsource ag-files (asyncio)
  "Grep."
  (generator
   ;; TODO: this is the exact same as ag except for the creator and
   ;; processor.  Add a common wrapper?
   (lambda (source state)
     (funcall
      (sallet-make-generator-linewise-asyncio
       (sallet-process-creator-first-token-only
        (sallet-ag-files-make-process-creator (oref source search-root)))
       'identity)
      source state)))
  (search-root)
  (init 'sallet--set-search-root)
  (renderer (lambda (candidate _ user-data)
              (sallet-fontify-regexp-matches
               (plist-get user-data :regexp-matches)
               candidate)))
  ;; TODO: finish the action
  (action (lambda (_source c) (find-file c))))

(provide 'sallet-ag)
;;; sallet-ag.el ends here
