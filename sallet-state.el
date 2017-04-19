;;; sallet-state.el --- Sallet state -*- lexical-binding: t -*-

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

;; Sallet state keeps track of all the necessary information for a
;; sallet session.  In theory we could run multiple sessions at the
;; same time, each contained within its state object.

;;; Code:

(require 'dash)
(require 'cl-lib)

(require 'sallet-core)
(require 'sallet-source)

;; TODO: make this into eieio object?
;; TODO: add documentation for the futures thing (and write/figure
;; out how the async works)
(defvar sallet-state nil
  "Current state.

SOURCES is a list of initialized sources.

CURRENT-BUFFER is the buffer from which sallet was executed.

PROMPT is the current prompt.

SELECTED-CANDIDATE is the currently selected candidate.

FUTURES is a plist mapping source id to the `async' future that
computes it.")

(defun sallet-state-get-sources (state)
  (cdr (assoc 'sources state)))
(defun sallet-state-get-current-buffer (state)
  (cdr (assoc 'current-buffer state)))
(defun sallet-state-get-prompt (state)
  (cdr (assoc 'prompt state)))
(defun sallet-state-get-selected-candidate (state)
  (cdr (assoc 'selected-candidate state)))
(defun sallet-state-get-candidate-buffer (state)
  (cdr (assoc 'candidate-buffer state)))
(defun sallet-state-get-futures (state)
  (cdr (assoc 'futures state)))

(defun sallet-state-set-sources (state sources)
  (setf (cdr (assoc 'sources state)) sources))
(defun sallet-state-set-prompt (state prompt)
  (setf (cdr (assoc 'prompt state)) prompt))
(defun sallet-state-set-selected-candidate (state selected-candidate)
  (setf (cdr (assoc 'selected-candidate state)) selected-candidate))
(defun sallet-state-set-futures (state futures)
  (setf (cdr (assoc 'futures state)) futures))

(defun sallet-state-incf-selected-candidate (state)
  (cl-incf (cdr (assoc 'selected-candidate state))))
(defun sallet-state-decf-selected-candidate (state)
  (cl-decf (cdr (assoc 'selected-candidate state))))

(defun sallet-state-get-number-of-all-candidates (state)
  "Return the number of all candidates in this STATE."
  (-sum (--map (length (sallet-source-get-processed-candidates it))
               (sallet-state-get-sources state))))

(defun sallet--goto-candidate (state)
  "Move point to current candidate in STATE."
  (-when-let (pos (text-property-any
                   (point-min) (point-max)
                   'sallet-candidate-index
                   (sallet-state-get-selected-candidate state)))
    (goto-char pos)))

(defun sallet-state-get-selected-source (state)
  "Return the currently selected source and candidate.

STATE is sallet state."
  (-when-let (sources
              (--filter (< 0 (length (sallet-source-get-processed-candidates it)))
                        (sallet-state-get-sources state)))
    (let* ((offset (sallet-state-get-selected-candidate state))
           (re (car sources))
           (total 0)
           (total-old total))
      (--each-while sources (<= total offset)
        (setq total-old total)
        (setq total (+ total (length (sallet-source-get-processed-candidates it))))
        (setq re it))
      (cons re (sallet-source-get-candidate
                re
                (sallet-car-maybe
                 (let ((proc (sallet-source-get-processed-candidates re)))
                   (nth (- offset total-old) proc))))))))

(defun sallet-init-state (sources candidate-buffer)
  "Initialize state with SOURCES in CANDIDATE-BUFFER."
  (let ((state (list (cons 'sources (-keep 'sallet-init-source sources))
                     (cons 'current-buffer (current-buffer))
                     (cons 'prompt "")
                     (cons 'selected-candidate 0)
                     (cons 'futures nil)
                     (cons 'candidate-buffer candidate-buffer))))
    (setq sallet-state state)
    state))

(provide 'sallet-state)
;;; sallet-state.el ends here
