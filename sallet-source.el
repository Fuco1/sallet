;;; sallet-source.el --- Abstract source definition -*- lexical-binding: t -*-

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

(require 'eieio)

(require 'sallet-core)

;; TODO: add docs
(defmacro sallet-defsource (name parents &optional docstring &rest body)
  (declare (doc-string 3)
           (debug (&define name (&rest arg) [&optional stringp] def-body))
           (indent defun))
  (unless (stringp docstring) (setq body (cons docstring body)))
  `(defclass ,(intern (concat "sallet-source-" (symbol-name name)))
     ,(--map (intern (concat "sallet-source-" (symbol-name it)))
             (if (and (not (memq name '(-default default)))
                      (not parents))
                 (list 'default) parents))
     ,(-map (lambda (arg)
              (let (re)
                (push (list (car arg) :initform (cadr arg)) re)
                (when (plist-member arg :documentation)
                  (push (list :documentation (plist-get arg :documentation)) re))
                (apply '-concat (nreverse re))))
            body)
     ,@(when (stringp docstring) (list :documentation docstring))))

(font-lock-add-keywords 'emacs-lisp-mode `(("(\\(sallet-defsource\\)\\>[[:blank:]]+\\(.*?\\)[[:blank:]]"
                                            (1 font-lock-keyword-face)
                                            (2 font-lock-type-face))))

(sallet-defsource -default ()
  "The absolute parent of the source hierarchy.

This is an internal class.

This source defines internal variables used to hold state during
the picking process."
  (processed-candidates nil))

;; TODO: add a simple interface to create an interactive transient
;; "source" with just a static list of candidates (i.e. support for
;; anonymous sources)
;; TODO: remake all the functions to take source as first
;; argument... this allows us to save state inside the source as with
;; normal objects.
(sallet-defsource default (-default)
  "Default source.

Every user or package-defined source must inherit from this
source.  If the user does not specify any source to inherit from,
this is added automatically!

Sets default matcher `sallet-matcher-default', identity renderer
and identity action."
  (matcher sallet-matcher-default
           :documentation "write what a matcher is. function matching and ranking/sorting candidates")
  (sorter (lambda (x _) x)
          :documentation "Sorter.")
  (init ignore
        :documentation
    "Arbitrary initialization function.

The function is run in the context of the buffer where sallet was
invoked and takes one argument, the current source.")
  (renderer (lambda (candidate state user-data) candidate)
            :documentation "write what a renderer is.")
  ;; action: TODO: (cons action-name action-function)
  ;; TODO: add support for actions which do not kill the session
  (action (lambda (_source candidate) candidate)
          :documentation
    "Default action on the candidate when sallet session is finished.

An action is a function of two arguments, the current source and
the picked candidate.")
  ;; A function generating candidates, a list or vector of candidates.
  ;; Candidates can be either strings or any lists with first element
  ;; being used for matching (usually a string, but can be anything as
  ;; long as the matcher and renderer and action know how to handle
  ;; it).
  (candidates nil)
  ;; function generating candidates, takes source and state, returns a
  ;; vector The difference with `candidates' is that `candidates' run
  ;; only when we first enter the session while `generator' takes
  ;; input interactively.
  (generator nil)
  ;; If t, this source is asynchronous and processing takes place in a
  ;; different emacs instance.  The source must be written with this in mind.
  (async nil)
  ;; Header.  Is either a string or a function.  In case of a string,
  ;; it is rendered in sallet-source-header face with a counter
  ;; showing number of filtered/all candidates.  If a function, it is
  ;; passed current source as argument and its output is used verbatim
  ;; as the header text (newline is added *automatically*)
  (header "Select a candidate"))

(sallet-defsource asyncio (default)
  "Default asyncio source."
  (process nil
           :documentation "Process generating candidates"))

;; TODO: replace all these setters and getters with the
;; accessor/setter property on the eieio object
(defun sallet-source-get-matcher (source)
  (oref source matcher))
(defun sallet-source-get-sorter (source)
  (oref source sorter))
(defun sallet-source-get-init (source)
  (oref source init))
(defun sallet-source-get-renderer (source)
  (oref source renderer))
(defun sallet-source-get-candidates (source)
  (oref source candidates))
(defun sallet-source-get-generator (source)
  (oref source generator))
(defun sallet-source-get-header (source)
  (oref source header))
(defun sallet-source-get-action (source)
  (oref source action))
(defun sallet-source-get-processed-candidates (source)
  (oref source processed-candidates))
(defun sallet-source-is-async (source)
  (oref source async))
(defun sallet-source-get-process (source)
  (when (slot-exists-p source 'process)
    (oref source process)))
(defun sallet-source-get-before-candidate-render-hook (source)
  (when (slot-exists-p source 'before-candidate-render-hook)
    (oref source before-candidate-render-hook)))
(defun sallet-source-get-before-render-hook (source)
  (when (slot-exists-p source 'before-render-hook)
    (oref source before-render-hook)))

(defun sallet-source-set-candidates (source candidates)
  (oset source candidates candidates))
(defun sallet-source-set-generator (source generator)
  (oset source generator generator))
(defun sallet-source-set-processed-candidates (source processed-candidates)
  (oset source processed-candidates processed-candidates))
(defun sallet-source-set-process (source process)
  (oset source process process))

(defun sallet-source-get-candidate (source n)
  (elt (sallet-source-get-candidates source) n))

(defun sallet-init-source (source)
  "Initiate the SOURCE."
  (-when-let (instance (funcall source (symbol-name source)))
    (funcall (sallet-source-get-init instance) instance)
    (let ((candidates (sallet-source-get-candidates instance)))
      (cond
       ((functionp candidates)
        (setq candidates (funcall candidates)))
       ((or (listp candidates)
            (vectorp candidates)
            (-when-let (sv (ignore-errors (symbol-value candidates)))
              (when (or (listp sv) (vectorp sv))
                (setq candidates sv)))))
       ((functionp (sallet-source-get-generator instance))
        (setq candidates nil))
       (t (error "Invalid source: no way to generate candidates")))
      (when (and candidates
                 (not (vectorp candidates)))
        (setq candidates (vconcat candidates)))
      (sallet-source-set-candidates instance candidates)
      ;; no filtering at start
      (sallet-source-set-processed-candidates instance (sallet-make-candidate-indices candidates)))
    (let ((generator (sallet-source-get-generator instance)))
      (unless (functionp generator)
        (sallet-source-set-generator instance (eval generator t))))
    instance))

(provide 'sallet-source)
;;; sallet-source.el ends here
