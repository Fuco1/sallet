;;; sallet-core.el --- Core shared functions for sallet -*- lexical-binding: t -*-

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
(require 's)

(defun sallet-vector-logical-length (vector)
  "Return logical length of VECTOR.

Logical length is the number of non-nil elements from start."
  (let ((i 0))
    (catch 'end
      (mapc (lambda (x) (if x (setq i (1+ i)) (throw 'end i))) vector)
      (length vector))))

(defun sallet-make-candidate-indices (candidates)
  "Create the indices list for CANDIDATES.

This is a list from 0 to (1- logical-length-of-candidates).  This
list is used in the filtering pipeline and at the end the
remaining indices point to the candidates structure and designate
valid candidates."
  (number-sequence 0 (1- (sallet-vector-logical-length candidates))))

(defun sallet-car-maybe (cons-or-thing)
  "Return `car' of CONS-OR-THING if it is a cons or itself otherwise."
  (if (consp cons-or-thing) (car cons-or-thing) cons-or-thing))

(defun sallet-aref (candidates index)
  "Return element of CANDIDATES at INDEX.

If INDEX is a number behaves just like `aref'.

If INDEX is a cons, take its `car' and then behaves like `aref'."
  (if (numberp index)
      (aref candidates index)
    (aref candidates (car index))))

(defun sallet-candidate-aref (candidates index)
  "Return candidate from CANDIDATES at INDEX.

CANDIDATES is a vector of candidates.  If the element at index is
a list, return its `car', otherwise return the element without change.

INDEX is a number, index into the CANDIDATES array.  If the index
is a list, take its `car'."
  (sallet-car-maybe (aref candidates (sallet-car-maybe index))))

(defun sallet-plist-update (plist property data update-function)
  "Take PLIST and append DATA to PROPERTY.

The value at PROPERTY is a list.

UPDATE-FUNCTION is used to compute the new value inserted into
the plist.  It takes two arguments, DATA and old value of
PROPERTY."
  (let ((old-data (plist-get plist property)))
    (plist-put plist property (funcall update-function data old-data))))

(defun sallet-update-index (index &rest properties)
  "Update INDEX with PROPERTIES.

PROPERTIES is a list of properties (PROPERTY NEW-VALUE UPDATE-FUNCTION).

PROPERTY is the key under which the value is stored.

NEW-VALUE is the value to combine with the old value.

UPDATE-FUNCTION is used to compute the new value inserted into
the plist.  It takes two arguments, NEW-VALUE and old value of
PROPERTY.

If UPDATE-FUNCTION is omitted the old value is replaced with NEW-VALUE."
  (cons
   (sallet-car-maybe index)
   (--reduce-from (let* ((property (car it))
                         (new-value (cadr it))
                         (update-function (or (nth 2 it) (lambda (x _) x))))
                    (sallet-plist-update acc property new-value update-function))
                  (cdr-safe index)
                  properties)))

(defun sallet--xdg-can-open-p (file)
  "Return non-nil if FILE can be opened with xdg-open(1)."
  (let* ((mime-type (with-temp-buffer
                      (call-process
                       "xdg-mime" nil (current-buffer) nil
                       "query" "filetype"
                       (expand-file-name file))
                      (buffer-string)))
         (desktop-file (with-temp-buffer
                         (call-process
                          "xdg-mime" nil (current-buffer) nil
                          "query" "default" (s-trim mime-type))
                         (buffer-string))))
    (not (equal "" (s-trim desktop-file)))))

(defun sallet--find-file-in-emacs-p (file)
  "Return non-nil if Emacs should try to open FILE."
  (let ((mime-type (with-temp-buffer
                     (call-process
                      "file" nil (current-buffer)
                      nil "-b" "--mime-type" file)
                     (buffer-string))))
    (string-match-p "^\\(inode\\|text\\)/" mime-type)))

(provide 'sallet-core)
;;; sallet-core.el ends here
