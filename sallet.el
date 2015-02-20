;;; sallet.el --- Select candidates in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 31st December 2014
;; Package-requires: ((dash "2.10.0") (flx "0.4"))
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
(require 'flx)
(require 'eieio)

(defgroup sallet ()
  "Select candidates in a buffer."
  :group 'convenience
  :prefix "sallet-")

(defun sallet-car-maybe (cons-or-thing)
  "Return `car' of CONS-OR-THING if it is a cons or itself otherwise."
  (if (consp cons-or-thing) (car cons-or-thing) cons-or-thing))

;; TODO: make this better
(defun sallet-matcher-default (candidates state)
  "Default matcher.

The input string is split on whitespace, then candidate must
match each constituent to pass the test.  Matches are not
reordered."
  (let* ((i 0)
         (re nil)
         (prompt (sallet-state-get-prompt state))
         (parts (split-string prompt)))
    (mapc
     (lambda (c)
       (let ((c-str (if (stringp c) c (car c))))
         (when (--all? (string-match-p (regexp-quote it) c-str) parts)
           (push i re)))
       (setq i (1+ i)))
     candidates)
    (nreverse re)))

;; TODO: make this better
(defun sallet-matcher-flx (candidates state)
  (let* ((i 0)
         (re nil)
         (prompt (sallet-state-get-prompt state))
         (parts (split-string prompt)))
    ;; first fuzzy score/filter by first input
    ;; TODO: add a function modifier that would transform any function into "first item matcher"
    (if (= (length prompt) 0)
        (number-sequence 0 (1- (length candidates)))
      (mapc
       (lambda (c)
         (let ((c-str (if (stringp c) c (car c))))
           (-when-let (score (flx-score c-str (car parts)))
             (push (cons i score) re)))
         (setq i (1+ i)))
       candidates)
      (nreverse re))))

(defun sallet-sorter-flx (candidates state)
  ;; sort by score
  (sort candidates (lambda (a b) (> (cadr a) (cadr b)))))

;; TODO: add docs
(defmacro sallet-defsource (name parents &optional docstring &rest body)
  (declare (docstring 3)
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
  (renderer (lambda (candidate state) candidate)
            :documentation "write what a renderer is.")
  ;; action: TODO: (cons action-name action-function)
  ;; TODO: add support for actions which do not kill the session
  (action identity)
  ;; A function generating candidates, a list or vector of candidates.
  ;; Candidates can be either strings or any lists with first element
  ;; being used for matching (usually a string, but can be anything as
  ;; long as the matcher and renderer and action know how to handle
  ;; it).
  (candidates nil)
  ;; function generating candidates, takes state, returns a vector
  ;; The difference with `candidates' is that `candidates' run only
  ;; when we first enter the session while `generator' takes input
  ;; interactively.
  (generator nil)
  ;; header
  (header "Select a candidate"))

;; TODO: add better renderer
;; TODO: add some version supporting filtering? is it even worth
;; having if we have narrowing built-in?
(sallet-defsource buffer nil
  "Buffer source."
  (candidates (lambda ()
                (--keep (let ((name (buffer-name it)))
                          (unless (string-match-p "^ " name) name))
                        (buffer-list))))
  (matcher sallet-matcher-flx)
  (action switch-to-buffer)
  (header "Buffers"))

(sallet-defsource recentf nil
  "Files saved on `recentf-list'."
  (candidates (lambda ()
                (unless recentf-mode (recentf-mode 1))
                (--keep
                 (let ((name (file-name-nondirectory it)))
                   (unless (get-file-buffer name)
                     (cons name it)))
                 recentf-list)))
  (matcher sallet-matcher-flx)
  (renderer (-lambda ((name) _) name))
  (action (-lambda ((_ . file)) (find-file file)))
  (header "Recently opened files"))

;; TODO: this depends on bookmark+ (`bmkp-file-alist-only',
;; `bmkp-jump-1'), should probably be moved to a different file.
(sallet-defsource bookmarks-file-only nil
  "Bookmarks source, files only."
  (candidates bmkp-file-alist-only)
  (matcher sallet-matcher-default)
  (renderer (lambda (c _) (car c)))
  (action (lambda (bookmark-name)
            ;; TODO: doesn't seem to work
            (bmkp-jump-1 (cons "" bookmark-name) 'switch-to-buffer nil)))
  (header "Bookmarks"))

;; TODO: write docstring
(defun sallet-occur-get-lines (buffer prompt &optional mode)
  "Mode: :normal, :fuzzy, :regexp (default)"
  (let ((pattern
         (concat "\\("
                 (cond
                  ((eq mode :normal)
                   (regexp-quote prompt))
                  ((eq mode :fuzzy)
                   (mapconcat 'identity
                              (mapcar 'char-to-string (string-to-list prompt))
                              ".*"))
                  (t prompt))
                 "\\)"))
        re)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (let* ((lb (line-beginning-position))
               (le (line-end-position))
               (line (save-excursion
                       (font-lock-fontify-region lb le)
                       (buffer-substring lb le))))
          (push (list line (point) (line-number-at-pos)) re)))
      (vconcat (nreverse re)))))

(sallet-defsource occur nil
  "Occur source."
  (candidates nil)
  (matcher nil)
  (renderer (-lambda ((line-string _ line-number) _)
              ;; TODO: add face to the number
              (format "%5d:%s" line-number line-string)))
  (generator '(let ((buffer (current-buffer)))
                (lambda (state)
                  (let ((prompt (sallet-state-get-prompt state)))
                    ;; TODO: move this into a separate setting... this
                    ;; is going to be quite common for "computing"
                    ;; sources
                    (when (>= (length prompt) 2)
                      (sallet-occur-get-lines buffer prompt :normal))))))
  (action (lambda (c)
            ;; TODO: why isn't it enough to use `goto-char'?  Probably
            ;; active window is badly re-set after candidate window is
            ;; disposed
            (set-window-point (selected-window) (cadr c)))))

(sallet-defsource occur-fuzzy (occur)
  "Fuzzy occur source."
  ;; matcher is used to rank & reorder best matches on top ...
  (matcher sallet-matcher-flx)
  (sorter sallet-sorter-flx)
  ;; ... while generator is the stupidest matcher possible
  (generator '(let ((buffer (current-buffer)))
                (lambda (state)
                  (let ((prompt (sallet-state-get-prompt state)))
                    (when (>= (length prompt) 2)
                      (sallet-occur-get-lines buffer prompt :fuzzy)))))))

(defun sallet-source-get-matcher (source)
  (oref source matcher))
(defun sallet-source-get-sorter (source)
  (oref source sorter))
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

(defun sallet-source-set-candidates (source candidates)
  (oset source candidates candidates))
(defun sallet-source-set-generator (source generator)
  (oset source generator generator))
(defun sallet-source-set-processed-candidates (source processed-candidates)
  (oset source processed-candidates processed-candidates))

(defun sallet-source-get-candidate (source n)
  (elt (sallet-source-get-candidates source) n))

(defun sallet-init-source (source)
  "Initiate the source."
  (-when-let (instance (funcall source (symbol-name source)))
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
      (sallet-source-set-processed-candidates instance (number-sequence 0 (1- (length candidates)))))
    (let ((generator (sallet-source-get-generator instance)))
      (unless (functionp generator)
        (sallet-source-set-generator instance (eval generator t))))
    instance))

;; TODO: make this into eieio object?
(defvar sallet-state nil
  "Current state.

SOURCES is a list of initialized sources.

CURRENT-BUFFER is the buffer from which sallet was executed.

PROMPT is the current prompt.

SELECTED-CANDIDATE is the currently selected candidate.")

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

(defun sallet-state-set-prompt (state prompt)
  (setf (cdr (assoc 'prompt state)) prompt))
(defun sallet-state-set-selected-candidate (state selected-candidate)
  (setf (cdr (assoc 'selected-candidate state)) selected-candidate))

(defun sallet-state-incf-selected-candidate (state)
  (incf (cdr (assoc 'selected-candidate state))))
(defun sallet-state-decf-selected-candidate (state)
  (decf (cdr (assoc 'selected-candidate state))))

(defun sallet-state-get-number-of-all-candidates (state)
  (-sum (--map (length (or (sallet-source-get-processed-candidates it)
                           (sallet-source-get-candidates it)))
               (sallet-state-get-sources state))))

;; TODO: make this function better, it's a mess
;; TODO: FIX: if some source has no displayed candidates at all, it is
;; skipped but index is not raised
(defun sallet-state-get-selected-source (state)
  (let* ((offset (sallet-state-get-selected-candidate state))
         (sources (sallet-state-get-sources state))
         (re (car sources))
         (total 0)
         (total-old total))
    (--each-while sources (< total offset)
      (setq total-old total)
      ;; TODO: abstract the `or' here... we just want to get some
      ;; candidates.  Search for it everywhere, it is used all over
      ;; the source
      (setq total (+ total (length (or (sallet-source-get-processed-candidates it)
                                       (sallet-source-get-candidates it)))))
      (setq re it))
    (cons re (sallet-source-get-candidate
              re
              (sallet-car-maybe
               (-if-let (proc (sallet-source-get-processed-candidates re))
                   (nth (- offset total-old) proc)
                 (- offset total-old)))))))

(defun sallet-init-state (sources candidate-buffer)
  (let ((state (list (cons 'sources (-keep 'sallet-init-source sources))
                     (cons 'current-buffer (current-buffer))
                     (cons 'prompt "")
                     (cons 'selected-candidate 0)
                     (cons 'candidate-buffer candidate-buffer))))
    (setq sallet-state state)
    state))

;; TODO propertize the interesting stuff, define faces
(defun sallet-render-source (state source offset)
  "Render.

OFFSET is the number of already rendered candidates before
this source.

Return number of rendered candidates."
  (with-current-buffer (sallet-state-get-candidate-buffer state)
    (let* ((selected (sallet-state-get-selected-candidate state))
           (prompt (sallet-state-get-prompt state))
           (selected-candidates (sallet-source-get-processed-candidates source))
           (renderer (sallet-source-get-renderer source))
           (coffset (- selected offset))
           (i 0))
      ;; TODO: abstract header rendering
      (when (and selected-candidates
                 (> (length selected-candidates) 0))
        (insert "=== " (sallet-source-get-header source) " ===\n"))
      (-each selected-candidates
        (lambda (n)
          ;; `n' can be a number or a list returned from the
          ;; matcher---the `car' of which is then the index, the rest
          ;; is arbitrary meta data ignored at this stage (it is
          ;; useful when at the sorter stage)
          (let* ((candidate (sallet-source-get-candidate source (sallet-car-maybe n))))
            ;; TODO: The >> marker should be handled with an
            ;; overlay. See the note in `sallet'.
            (insert (if (= coffset i) ">>" "  ")
                    (funcall renderer candidate state)
                    "\n"))
          (when (= coffset i)
            (set-window-point (get-buffer-window (sallet-state-get-candidate-buffer state)) (point)))
          (setq i (1+ i))))
      i)))

(defun sallet-render-state (state)
  "Render state."
  (with-current-buffer (sallet-state-get-candidate-buffer state)
    (erase-buffer)
    (let ((offset 0))
      (-each (sallet-state-get-sources state)
        (lambda (source)
          (setq offset (+ offset (sallet-render-source state source offset)))))
      (insert "\n\n"))))

;; Add user-facing documentation as docstring and developer
;; documentation in code.
(defun sallet (sources)
  (let* ((buffer (get-buffer-create "*Candidates*"))
         ;; make this lexically scoped
         (state (sallet-init-state sources buffer)))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (buffer-disable-undo)
      (setq cursor-type nil))
    (pop-to-buffer buffer)
    (sallet-render-state state)
    (condition-case var
        (minibuffer-with-setup-hook
            (lambda ()
              ;; TODO: figure out where to do which updates... this currently doesn't work
              ;; The problem is that this lags the minibuffer input
              ;; reading every time it changes and some recomputation
              ;; happens.  We want to be able to type in a word
              ;; without sallet recomputing the full candidate list
              ;; after every letter.  Helm solves this with timers,
              ;; which we will probably have to opt for too (aka poor
              ;; man's threads)
              (add-hook
               'post-command-hook
               (lambda ()
                 ;; TODO: add old-prompt to state
                 (let ((old-prompt (sallet-state-get-prompt state))
                       (new-prompt (buffer-substring-no-properties 5 (point-max))))
                   (unless (equal old-prompt new-prompt)
                     (sallet-state-set-selected-candidate state 0)
                     (sallet-state-set-prompt state new-prompt)
                     (-each (sallet-state-get-sources state)
                       (lambda (source)
                         (-when-let (generator (sallet-source-get-generator source))
                           (sallet-source-set-candidates source (funcall generator state)))
                         (let* ((candidates (sallet-source-get-candidates source)))
                           (-if-let (matcher (sallet-source-get-matcher source))
                               (let ((selected-candidates (funcall matcher candidates state)))
                                 (sallet-source-set-processed-candidates source selected-candidates))
                             (sallet-source-set-processed-candidates source (number-sequence 0 (1- (length candidates))))))
                         (let* ((candidates (sallet-source-get-processed-candidates source)))
                           (-when-let (sorter (sallet-source-get-sorter source))
                             (sallet-source-set-processed-candidates
                              source
                              (funcall sorter candidates state))))))))
                 ;; TODO: we shouldn't need to re-render if
                 ;; no change happened... currently this only
                 ;; handles scrolling (the >> indicator).
                 ;; That should be done with a sliding
                 ;; overlay instead.  Change in
                 ;; `sallet-render-state'.
                 (sallet-render-state state))
               nil t))
          ;; TODO: add support to pass maps
          ;; TODO propertize prompt
          (read-from-minibuffer ">>> " nil (let ((map (make-sparse-keymap)))
                                             (set-keymap-parent map minibuffer-local-map)
                                             (define-key map (kbd "C-n") 'sallet-candidate-up)
                                             (define-key map (kbd "C-p") 'sallet-candidate-down)
                                             map))
          (sallet-default-action))
      ;; TODO: do we want `kill-buffer-and-window?'
      (quit (kill-buffer-and-window))
      (error (kill-buffer-and-window)))))

;; TODO: figure out how to avoid the global state here: sallet-state
(defun sallet-candidate-up ()
  (interactive)
  (when (< (sallet-state-get-selected-candidate sallet-state)
           (1- (sallet-state-get-number-of-all-candidates sallet-state)))
    (sallet-state-incf-selected-candidate sallet-state)))

(defun sallet-candidate-down ()
  (interactive)
  (when (> (sallet-state-get-selected-candidate sallet-state) 0)
    (sallet-state-decf-selected-candidate sallet-state)))

(defun sallet-default-action ()
  (kill-buffer-and-window)
  (-when-let ((source . cand) (sallet-state-get-selected-source sallet-state))
    (funcall (sallet-source-get-action source) cand)))

;; TODO: create a customize group just for sources
(defcustom sallet-buffer-sources '(sallet-source-buffer)
  "Sources for `sallet-buffer'.

Since `sallet' does not make any artificial distinctions between
sources, you can put any source here.  However, keeping it
thematic and related to buffers is probably a good idea."
  :group 'sallet
  :type '(repeat symbol))

(defun sallet-buffer ()
  "Display buffer-like candidates.

Takes the list of used sources from `sallet-buffer-sources'."
  (interactive)
  (sallet sallet-buffer-sources))

(defun sallet-occur ()
  "Show all lines in current buffer matching the fuzzy pattern.

First, all lines matching the input pattern \"fuzzily\" are
collected.  They are then scored and ordered to bring the most
interesting lines at the top.  Therefore, the results are not in
the same order as they appear in the buffer.  If you want that,
use `salet-occur-nonfuzzy'.

The scoring algorithm is from the package `flx'.

If you want to customize the matching algorithm, you can extend
sallet source `sallet-source-occur-fuzzy'."
  (interactive)
  (sallet (list sallet-source-occur-fuzzy)))

(defun sallet-occur-nonfuzzy ()
  "Show all lines in current buffer matching pattern.

The lines are presented in the same order as they appear in the
file.  The lines are matched against each word in the input
separately.

See also `sallet-occur' for a fuzzy variant.  If you want to
customize the matching algorithm, you can extend sallet source
`sallet-source-occur'."
  (interactive)
  (sallet (list sallet-source-occur)))

;; TODO: write sallet for opening files

(provide 'sallet)
;;; sallet.el ends here
