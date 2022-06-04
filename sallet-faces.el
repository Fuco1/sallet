;;; sallet-faces.el --- Faces for sallet -*- lexical-binding: t -*-

;; Copyright (C) 2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 10th October 2015
;; Keywords: faces

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


;;; Faces

(defgroup sallet-faces nil
  "Sallet faces."
  :group 'sallet)

(defface sallet-source-header
  '((t (:inherit highlight :extend t)))
  "Face used to fontify source header."
  :group 'sallet-faces)

(defface sallet-buffer-ordinary
  '((t (:inherit font-lock-type-face)))
  "Face used to fontify ordinary buffers."
  :group 'sallet-faces)

(defface sallet-buffer-modified
  '((t (:inherit font-lock-warning-face)))
  "Face used to fontify modified (unsaved) buffers."
  :group 'sallet-faces)

(defface sallet-buffer-compressed
  '((t (:inherit font-lock-doc-face)))
  "Face used to fontify buffers representing compressed files.

Compressed files are those matching
`ibuffer-compressed-file-name-regexp'."
  :group 'sallet-faces)

(defface sallet-buffer-read-only
  '((t (:inherit font-lock-constant-face)))
  "Face used to fontify read-only buffers."
  :group 'sallet-faces)

(defface sallet-buffer-special
  '((t (:inherit font-lock-keyword-face)))
  "Face used to fontify special buffers.

Special buffers are those prefixed by *."
  :group 'sallet-faces)

(defface sallet-buffer-help
  '((t (:inherit font-lock-comment-face)))
  "Face used to fontify help buffers.

Help buffers are those whose major mode matches
`ibuffer-help-buffer-modes'."
  :group 'sallet-faces)

(defface sallet-buffer-directory
  '((t (:inherit font-lock-function-name-face)))
  "Face used to fontify directory buffers.

Directory buffers are those whose major mode is `dired-mode'."
  :group 'sallet-faces)

(defface sallet-buffer-size
  '((t (:foreground "RosyBrown")))
  "Face used to fontify buffer size."
  :group 'sallet-faces)

(defface sallet-buffer-default-directory
  '((t (:foreground "Sienna3")))
  "Face used to fontify buffer's default directory or process."
  :group 'sallet-faces)

(defface sallet-regexp-match
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face used to fontify regexp matches."
  :group 'sallet-faces)

(defface sallet-substring-match
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face used to fontify substring matches."
  :group 'sallet-faces)

(defface sallet-flx-match
  '((t (:inherit font-lock-variable-name-face :weight bold
        :underline (:color foreground-color :style line))))
  "Face used to fontify flx matches."
  :group 'sallet-faces)


;;; Fontification helpers

(defun sallet--fontify-regions (regions string face)
  "Highlight REGIONS of STRING using FACE.

REGIONS is a list of conses (BEG . END) where each cons delimits the region.

STRING is the string we want to fontify."
  (let ((new-string (copy-sequence string)))
    (-each regions
      (-lambda ((beg . end))
        (add-text-properties beg end (list 'face face) new-string)))
    new-string))

(defun sallet-fontify-regexp-matches (matches string)
  "Highlight regexp MATCHES in STRING.

MATCHES is a list of conses (BEG . END) where each cons delimits
the matched region.

STRING is the string we want to fontify."
  (sallet--fontify-regions matches string 'sallet-regexp-match))

(defun sallet-fontify-substring-matches (matches string)
  "Highlight substring MATCHES in STRING.

MATCHES is a list of conses (BEG . END) where each cons delimits
the matched region.

STRING is the string we want to fontify."
  (sallet--fontify-regions matches string 'sallet-substring-match))

(defun sallet-fontify-flx-matches (matches string)
  "Highlight flx MATCHES in STRING.

MATCHES is a list of indices where flx matched a letter to the input pattern.

STRING is the string we want to fontify."
  (let ((new-string (copy-sequence string)))
    (--each matches
      (add-text-properties it (1+ it) (list 'face 'sallet-flx-match) new-string))
    new-string))

(defun sallet-compose-fontifiers (string user-data &rest fontifiers)
  "Fontify STRING using information from USER-DATA by applying FONTIFIERS.

FONTIFIERS is an list of (FONTIFIER . ATTRIBUTE) or FONTIFIER.
Fontifiers are applied in sequence.

ATTRIBUTE is key into the USER-DATA.

FONTIFIER is a function of one or two arguments.  If it has
associated ATTRIBUTE, its value in USER-DATA is passed as first
argument, the string to be fontified as second.  Otherwise just
the string is passed to the function."
  (--reduce-from (let ((user-value (when (consp it) (plist-get user-data (cdr it))))
                       (fn (if (consp it) (car it) it)))
                   (if (consp it)
                       (funcall fn user-value acc)
                     (funcall fn acc)))
                 string fontifiers))

(provide 'sallet-faces)
;;; sallet-faces.el ends here
