;;; sallet.el --- Select candidates in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 31st December 2014
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
(require 'async)
(require 'flx)
(require 'ov)

(require 'eieio)
(require 'ibuffer)
(require 'imenu)

(require 'sallet-core)
(require 'sallet-source)
(require 'sallet-state)
(require 'sallet-filters)
(require 'sallet-faces)

(require 'sallet-buffer)
(require 'sallet-recentf)
(require 'sallet-imenu)
(require 'sallet-occur)
(require 'sallet-autobookmarks)
(require 'sallet-registers)
(require 'sallet-ag)

(defgroup sallet ()
  "Select candidates in a buffer."
  :group 'convenience
  :prefix "sallet-")

(sallet-defsource default-directory-files ()
  "List files in `default-directory.'"
  (candidates (-map 'f-filename (f-files default-directory)))
  (header "Default directory")
  (action (lambda (_ c)
            (find-file (concat default-directory "/" c)))))

;; TODO: add a user/source option to disable this
(defun sallet--smart-case (pattern &optional switch)
  "Decide if we should turn on smart-case matching for PATTERN.

If PATTERN contains upper-case letters, respect case, otherwise
ignore case.

SWITCH is the command switch which we should use to toggle this
behaviour, defaults to \"--ignore-case\".

Returns a list with car being the SWITCH."
  (let ((case-fold-search nil))
    (unless (string-match-p "[A-Z]" pattern)
      (list (or switch "--ignore-case")))))

;; TODO: move all the asyncio to a separate file
(defun sallet-process-args (args)
  "Construct a list of arguments to pass to `start-process'.

ARGS is a list of strings or lists.  If a string, it is copied to
the output list verbatim.  If a list, its elements are copied to
the output one by one, thus flattening the input by one level.
This also means that nil inputs are ignored."
  (let (re)
    (-each args
      (lambda (arg)
        (if (listp arg)
            (when arg (--each arg (push it re)))
          (push arg re))))
    (nreverse re)))

(defun sallet-start-process (program &rest args)
  "Run PROGRAM with ARGS.

ARGS are preprocessed using `sallet-process-args'."
  (declare (indent 1))
  (apply 'start-process
         program nil program
         (sallet-process-args args)))

(defun sallet-make-generator-linewise-asyncio (process-creator processor)
  "Make a linewise generator.

PROCESS-CREATOR is a function which when called returns a process
which produces the candidates.  It takes one argument, the
current prompt.

PROCESSOR is a function taking one line of output and producing a
candidate.

Return a generator."
  (lambda (source state)
    (let ((prompt (sallet-state-get-prompt state)))
      (-when-let (proc (funcall process-creator prompt))
        (sallet--kill-source-process source)
        (set-process-filter
         proc
         (sallet-process-filter-linewise-candidate-decorator
          processor source state))
        (set-process-sentinel
         proc
         (lambda (_process process-state)
           (when (equal process-state "finished\n")
             (sallet-update-candidates state source)
             ;; TODO: do we want to render here?
             (sallet-render-state state t))))
        (sit-for 0.01)
        proc))))

(defun sallet-process-run-in-directory (process-creator directory)
  "Run PROCESS-CREATOR in DIRECTORY."
  (lambda (prompt)
    (when directory
      (with-temp-buffer
        (cd directory)
        (funcall process-creator prompt)))))

(defun sallet-process-creator-first-token-only (process-creator)
  "Decorate PROCESS-CREATOR to only receive first input token.

PROCESS-CREATOR is responsible for creating a process which will
generate candidates for current session and is called each time
the prompt changes.

This decorator interprets the prompt and only passes the first
whitespace-separated token to the decorated PROCESS-CREATOR.
Further, if this token hasn't changed the process is not
restarted."
  (let ((old ""))
    (lambda (prompt)
      (let ((input (split-string prompt " ")))
        (when (or (not old)
                  (not (equal (car input) old)))
          (setq old (car input))
          (funcall process-creator (car input)))))))

(defun sallet-process-creator-min-prompt-length (process-creator &optional limit)
  "Decorate PROCESS-CREATOR to only run if prompt is longer than LIMIT.

Default limit is 3 characters."
  (lambda (prompt)
    (when (>= (length prompt) (or limit 3))
      (funcall process-creator prompt))))

;; TODO: add arguments such as path and other "session" data we need
;; to pass to grep
(defun sallet-grep-make-process-creator (file-name)
  "Return a process creator for grep sallet.

FILE-NAME is the file we are grepping."
  (lambda (prompt)
    (sallet-start-process "grep"
      "-n" (sallet--smart-case prompt) prompt file-name)))

(sallet-defsource grep (asyncio)
  "Grep."
  (generator
   (sallet-make-generator-linewise-asyncio
    (sallet-process-creator-min-prompt-length
     (sallet-process-creator-first-token-only
      (sallet-grep-make-process-creator (buffer-file-name))))
    'identity))
  (matcher (sallet-make-matcher
            (sallet-ignore-first-token-filter
             (sallet-make-tokenized-filter
              'sallet-filter-substring))))
  (renderer (lambda (c _ _) c))
  (action (lambda (_source c)
            (goto-char (point-min))
            (forward-line (1- (string-to-number (car (split-string c ":"))))))))

(defun sallet-grep ()
  "Run grep sallet."
  (interactive)
  (sallet (list sallet-source-grep)))

(defun sallet-gtags-files-make-process-creator ()
  "Return a process creator for gtags-files sallet."
  (lambda (prompt)
    (apply
     'start-process
     "global" nil "global" "-P"
     (-concat
      (sallet--smart-case prompt)
      ;; TODO: for this kind of flex matching we should
      ;; replace . with [^/] so that we search only in the
      ;; base name and not the directory tree.  Additionally,
      ;; / does flex matching on the path and non-prefixed
      ;; second and further strings substring-match the entire
      ;; path (if the first token starts with /, we use . in
      ;; the pattern to get full list over the entire project)
      ;; (list (mapconcat
      ;;        (lambda (x) (char-to-string x))
      ;;        (string-to-list prompt)
      ;;        ".*"))
      (list (concat ".*" prompt ".*"))))))

;; TODO: after some timeout, start generating candidates automatically
(sallet-defsource gtags-files (asyncio)
  "Grep."
  (generator
   (sallet-make-generator-linewise-asyncio
    (sallet-process-creator-min-prompt-length
     (sallet-process-creator-first-token-only
      (sallet-process-run-in-directory
       (sallet-gtags-files-make-process-creator)
       (locate-dominating-file default-directory "GTAGS"))))
    'identity))
  (project-root (locate-dominating-file default-directory "GTAGS"))
  (matcher sallet-matcher-default)
  ;; (matcher sallet-matcher-flx-then-substring)
  ;; TODO: add some sorter which does intelligent scoring for
  ;; substring matches
  ;; (sorter sallet-sorter-flx)
  (renderer (lambda (candidate _ user-data)
              (sallet-compose-fontifiers
               candidate user-data
               '(sallet-fontify-regexp-matches . :regexp-matches)
               '(sallet-fontify-flx-matches . :flx-matches))))
  (action (lambda (source c)
            (find-file (concat (oref source project-root) "/" c))))
  (header (lambda (source)
            (sallet--wrap-header-string
             (format "File in project %s" (oref source project-root))
             source))))

(defun sallet-gtags-files ()
  "Run gtags files sallet."
  (interactive)
  (sallet (list sallet-source-gtags-files)))

(defun sallet-tags-make-process-creator ()
  "Return a process creator for gtags tags sallet."
  (let ((old ""))
    (lambda (prompt)
      ;; TODO: Extract this "run on change of first token only" logic,
      ;; see `sallet-run-program-on-first'.
      (let ((input (split-string prompt " ")))
        (when (or (not old)
                  (not (equal (car input) old)))
          (setq old (car input))
          ;; TODO: write something to "start global in the root" or
          ;; figure out a way to print paths from root, not relative.
          (with-temp-buffer
            ;; TODO: this should come from outside?
            (cd (locate-dominating-file default-directory "GTAGS"))
            (apply
             'start-process
             "global" nil "global" "--result" "grep" "-T"
             (-concat
              (sallet--smart-case (car input))
              ;; TODO: extract this "match substring anywhere in the
              ;; string" logic
              (list (concat ".*" (car input) ".*")))
             ;; TODO: extract this "fuzzy regexp" generator logic
             ;; (mapconcat
             ;;  (lambda (x) (char-to-string x))
             ;;  (string-to-list (car input))
             ;;  ".*")
             )))))))

;; TODO: add a stack so we can pop back from where we came
(defun sallet-gtags-tags-action (source candidate)
  "Display tag CANDIDATE in its buffer."
  (-when-let (root (locate-dominating-file default-directory "GTAGS"))
    (save-match-data
      (let (file line)
        (string-match "^\\(.*?\\):\\(.*?\\):" candidate)
        ;; sigh...
        (setq file (match-string 1 candidate))
        (setq line (match-string 2 candidate))
        (find-file (concat root "/" file))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line)))
        (recenter-top-bottom)))))

(sallet-defsource gtags-tags (asyncio)
  "Run global(1) to generate tag candidates."
  (generator
   ;; TODO: We should be generating better candidates, not just lines
   ;; (identity)
   (sallet-make-generator-linewise-asyncio
    (sallet-tags-make-process-creator)
    'identity))
  (matcher
   ;; TODO: match only on content, add / matcher for path
   sallet-matcher-default)
  ;; TODO: better sorter
  ;; (sorter sallet-sorter-flx)
  (renderer
   ;; TODO: extract this renderer into a function, it is pretty common
   (lambda (candidate _ user-data)
     (sallet-compose-fontifiers
      candidate user-data
      '(sallet-fontify-regexp-matches . :regexp-matches)
      '(sallet-fontify-flx-matches . :flx-matches))))
  (action sallet-gtags-tags-action))

(defun sallet-gtags-tags ()
  "Run gtags tags sallet."
  (interactive)
  (sallet (list sallet-source-gtags-tags)))

;; TODO: add projectile support
(defun sallet--set-search-root (source)
  "Set search root for SOURCE.

The user is asked interactively for search root.

If the `default-directory' is inside a gtags project, the project
root is supplied as the default choice.

Otherwise the user is asked to pick the search root starting at
the `default-directory'."
  (unless (slot-exists-p source 'search-root)
    (error "Slot `search-root' does not exist"))
  (oset source search-root
        (read-directory-name
         "Project root: "
         (locate-dominating-file default-directory "GTAGS"))))

(defun sallet-ag ()
  "Run ag sallet."
  (interactive)
  (sallet (list sallet-source-ag)))

(defun sallet-ag-files ()
  "Run ag sallet."
  (interactive)
  (sallet (list sallet-source-ag-files)))

(defun sallet-locate-make-process-creator ()
  "Return a process creator for locate sallet."
  (lambda (prompt)
    (apply
     'start-process
     "locate" nil "locate"
     (-concat
      ;; TODO: write something that dispatches on pattern like
      ;; we have for filters
      (unless (eq (aref prompt 0) ?/)
        (list "--basename"))
      (sallet--smart-case prompt)
      (list
       (if (eq (aref prompt 0) ?/)
           (substring prompt 1)
         prompt))))))

(sallet-defsource locate (asyncio)
  "Run locate(1).

Candidates are opened with xdg-open(1) if possible or inside
emacs using `find-file' if no launcher is defined."
  (generator
   (sallet-make-generator-linewise-asyncio
    (sallet-process-creator-min-prompt-length
     (sallet-process-creator-first-token-only
      (sallet-locate-make-process-creator)))
    'identity))
  (matcher (lambda (candidates state)
             (let* ((prompt (sallet-state-get-prompt state))
                    (indices (sallet-make-candidate-indices candidates)))
               (sallet-compose-filters-by-pattern
                '(("\\`/\\(.*\\)" 1 sallet-filter-substring)
                  ("\\`\\.\\(.*\\)" 1 sallet-filter-file-extension)
                  ;; TODO: we should match first on the basename, then
                  ;; on the rest
                  (t sallet-filter-substring))
                candidates
                indices
                prompt))))
  (renderer (lambda (candidate _ user-data)
              (sallet-fontify-regexp-matches
               (plist-get user-data :regexp-matches)
               candidate)))
  (header "locate")
  (action (lambda (_source c)
            (if (sallet--xdg-can-open-p c)
                (call-process "xdg-open" nil 0 nil c)
              (find-file c)))))

(defun sallet-locate ()
  "Run locate sallet."
  (interactive)
  (sallet (list sallet-source-locate)))

(defun sallet--wrap-header-string (header-string source)
  "Wrap HEADER-STRING for SOURCE with meta information."
  (format
   " • %s [%d/%d]"
   header-string
   (length (sallet-source-get-processed-candidates source))
   (length (sallet-source-get-candidates source))))

(defun sallet--propertize-header (header-string)
  "Propertize HEADER-STRING with default sallet header face."
  (propertize header-string 'face 'sallet-source-header))

(defun sallet-render-header (source)
  "Render header for sallet SOURCE."
  (let ((processed-candidates (sallet-source-get-processed-candidates source)))
    (when (and processed-candidates
               (> (length processed-candidates) 0))
      (let ((header (sallet-source-get-header source)))
        (if (functionp header)
            (insert
             (let ((header-string (funcall header source)))
               (if (text-property-not-all
                    0 (length header-string) 'face nil header-string)
                   header-string
                 (sallet--propertize-header
                  (concat header-string "\n")))))
          (insert
           (sallet--propertize-header
            (concat (sallet--wrap-header-string header source) "\n"))))))))

;; TODO propertize the interesting stuff, define faces
(defun sallet-render-source (source state offset)
  "Render SOURCE in STATE.

OFFSET is the number of already rendered candidates before
this source.

Return number of rendered candidates."
  (with-current-buffer (sallet-state-get-candidate-buffer state)
    (let* ((processed-candidates (sallet-source-get-processed-candidates source))
           (renderer (sallet-source-get-renderer source))
           (before-render-hook
            (sallet-source-get-before-render-hook source))
           (before-candidate-render-hook
            (sallet-source-get-before-candidate-render-hook source))
           (i 0))
      (when (functionp before-render-hook)
        (funcall before-render-hook source state))
      (sallet-render-header source)
      (-each processed-candidates
        (lambda (n)
          ;; `n' can be a number or a list returned from the
          ;; matcher---the `car' of which is then the index, the rest
          ;; is arbitrary meta data ignored at this stage (it is
          ;; useful when at the sorter stage)
          (let* ((candidate (sallet-source-get-candidate source (sallet-car-maybe n))))
            (when (functionp before-candidate-render-hook)
              (funcall before-candidate-render-hook candidate state n))
            (insert (propertize "  " 'sallet-candidate-index (+ offset i))
                    ;; TODO: cache the already rendered lines also
                    ;; between sallet calls, there's quite a lot of
                    ;; chance it will come again, like with buffers or
                    ;; so
                    (funcall renderer candidate state (cdr-safe n))
                    "\n"))
          (setq i (1+ i))))
      i)))

(defun sallet-render-state (state render-sources)
  "Render state.

STATE is the current `sallet-state'.

RENDER-SOURCES indicates whether we need to render sources (in
case the prompt or candidates changed) or only update the
scrolling/position of selected/marked candidate."
  (when render-sources
    (with-current-buffer (sallet-state-get-candidate-buffer state)
      (erase-buffer)
      (let ((offset 0))
        (-each (sallet-state-get-sources state)
          (lambda (source)
            (setq offset (+ offset (sallet-render-source source state offset)))))
        (insert "\n\n"))))
  ;; Draw the >> pointer to the currently active candidate
  (with-current-buffer (sallet-state-get-candidate-buffer state)
    (-when-let (pos (text-property-any (point-min) (point-max) 'sallet-candidate-index (sallet-state-get-selected-candidate state)))
      (ov-clear 'sallet-selected-candidate-arrow)
      (goto-char pos)
      ;; TODO: add face, extend the overlay over the entire row? (then
      ;; we can highlight the rest with some overlay as well)
      (ov (point) (+ 2 (point)) 'display ">>" 'sallet-selected-candidate-arrow t)
      (set-window-point (get-buffer-window (sallet-state-get-candidate-buffer state)) pos))))

(defun sallet--kill-source-process (source)
  "Kill process associated with SOURCE, if any."
  (-when-let (old-proc (sallet-source-get-process source))
    (set-process-filter old-proc nil)
    (set-process-sentinel old-proc nil)
    (ignore-errors (kill-process old-proc))))

(defun sallet-cleanup-candidate-window (state)
  "Cleanup the sallet STATE."
  (--each (sallet-state-get-sources state)
    (sallet--kill-source-process it))
  (-when-let (buffer (get-buffer "*Sallet candidates*"))
    (kill-buffer buffer))
  (--each (buffer-list)
    (when (string-match-p "\\` \\*Minibuf-[0-9]+\\*\\'"(buffer-name it))
      (remove-hook 'post-command-hook 'sallet-minibuffer-post-command-hook t))))

(defvar sallet-minibuffer-post-command-hook nil
  "Closure used to update sallet window on minibuffer events.

The closure is stored in function slot.")

(defun sallet-minibuffer-setup (state)
  "Setup `post-command-hook' in minibuffer to update sallet STATE."
  ;; TODO: figure out where to do which updates... this currently
  ;; doesn't work The problem is that this lags the minibuffer input
  ;; reading every time it changes and some recomputation happens.  We
  ;; want to be able to type in a word without sallet recomputing the
  ;; full candidate list after every letter.  Helm solves this with
  ;; timers, which we will probably have to opt for too (aka poor
  ;; man's threads)
  (fset 'sallet-minibuffer-post-command-hook
        (lambda () (sallet-minibuffer-post-command state)))
  (add-hook 'post-command-hook 'sallet-minibuffer-post-command-hook nil t))

;; TODO: figure out how to do the buffer passing fast
(defun sallet-process-source-async (source state)
  "Process SOURCE in STATE asynchronously in separate Emacs."
  (let ((sallet-async-state (--remove
                             (memq (car it) '(sources
                                              futures
                                              current-buffer
                                              candidate-buffer)) state))
        (sallet-async-source source))
    (async-start
     `(lambda ()
        (push ,(file-name-directory (locate-library "dash")) load-path)
        (push ,(file-name-directory (locate-library "flx")) load-path)
        (push ,(file-name-directory (locate-library "shut-up")) load-path)
        (push ,(file-name-directory (locate-library "sallet")) load-path)
        (require 'shut-up)
        (require 'sallet)
        (with-temp-buffer
          (shut-up
            ;; TODO: this isn't always necessary, should be part of the
            ;; async source generator?
            (insert ,(with-current-buffer (sallet-state-get-current-buffer state)
                       (buffer-substring-no-properties (point-min) (point-max))))
            (setq sallet-async-state (read ,(format "%S" sallet-async-state)))
            (setq sallet-async-source (read ,(format "%S" sallet-async-source)))
            (sallet-process-source sallet-async-state sallet-async-source))
          (list :candidates (sallet-source-get-candidates sallet-async-source)
                :processed-candidates (sallet-source-get-processed-candidates sallet-async-source))))
     (lambda (result)
       (-when-let ((&plist :candidates candidates
                           :processed-candidates processed-candidates)
                   result)
         (sallet-source-set-candidates source candidates)
         (sallet-source-set-processed-candidates source processed-candidates)
         (sallet-render-state state t))))))

(defun sallet-update-candidates (state source)
  "Update candidates and processed-candidatess in STATE for SOURCE."
  (let* ((candidates (sallet-source-get-candidates source)))
    (-if-let (matcher (sallet-source-get-matcher source))
        (let ((processed-candidates (funcall matcher candidates state)))
          (sallet-source-set-processed-candidates source processed-candidates))
      (sallet-source-set-processed-candidates source (sallet-make-candidate-indices candidates))))
  (let* ((processed-candidates (sallet-source-get-processed-candidates source)))
    (-when-let (sorter (sallet-source-get-sorter source))
      (sallet-source-set-processed-candidates
       source
       (funcall sorter processed-candidates state)))))

(defun sallet-process-source (state source)
  "Update sallet STATE by processing SOURCE."
  (-when-let (generator (sallet-source-get-generator source))
    (let ((gen (funcall generator source state)))
      (cond
       ((processp gen)
        (sallet-source-set-process source gen))
       (gen (sallet-source-set-candidates source gen)))))
  (sallet-update-candidates state source))

(defun sallet-process-sources (state)
  "Process all sallet sources in STATE.

There are three principal types of sources: sync, async and
asyncio."
  ;; TODO: add old-prompt to state
  ;; TODO: add old-processed-candidates to state
  (-each (sallet-state-get-sources state)
    (lambda (source)
      ;; Here async means async package (computing in background
      ;; emacs).  Another meaning of async is async io (using output
      ;; of a process to construct candidates).  The latter is not yet
      ;; supported, but we should get it working.
      ;; TODO: add support for taking process output and constructing
      ;; list of candidates out of that.  That could be called
      ;; "process filtering source" ?
      (if (not (sallet-source-is-async source))
          (sallet-process-source state source)
        (let ((futures (sallet-state-get-futures state))
              (source-id (aref source 2)))
          (-when-let ((&plist source-id process) futures)
            (ignore-errors
              (let ((buffer (process-buffer process)))
                (kill-process process)
                (kill-buffer buffer))))
          (let ((proc (sallet-process-source-async source state)))
            (sallet-state-set-futures state (plist-put futures source-id proc))))))))

(defun sallet-minibuffer-post-command (state)
  "Function called in `post-command-hook' when sallet STATE is active.

This function is added to minibuffer's `post-command-hook' and
updates the candidate buffer."
  (let ((old-prompt (sallet-state-get-prompt state))
        (new-prompt (buffer-substring-no-properties 5 (point-max))))
    (unless (equal old-prompt new-prompt)
      (sallet-state-set-selected-candidate state 0)
      (sallet-state-set-prompt state new-prompt)
      (sallet-process-sources state))
    (sallet-render-state state (not (equal old-prompt new-prompt)))))

;; TODO: add user-facing documentation as docstring and developer
;; documentation in code.
;; TODO: add a way to preprocess the pattern before passing it to the
;; individual sources... this can help when we mix "incompatible"
;; sources together (where e.g. special prefixes mean different things
;; or are meaningless... so if we mix e.g. buffer and locate, we don't
;; want to pass leading / to locate but we want to pass it to buffer).
;; The filter is not done on the source level because the same prefix
;; (or lack of) can mean different thing to different sources.
;; TODO: add conditional evaluation of sources.  For example, first
;; run global -P, if nothing is found run ag -g, if nothing is found
;; run find . -name '*<pat>*', if nothing is found run locate...  This
;; way we don't run all the redundant "broader" searches if some
;; narrower search succeeds.  After some timeouts or a "recompute
;; signal" we can recompute all targets.
;; TODO: add some simple default implementation for "line candidates
;; from process" and "grep-like candidates from process"
(defun sallet (sources)
  "Run sallet SOURCES."
  (let* ((buffer (get-buffer-create "*Sallet candidates*"))
         ;; make this lexically scoped
         (state (sallet-init-state sources buffer)))
    ;; TODO: add better modeline, show number of sources/candidates etc...
    ;; TODO: add sallet-candidates-mode as major-mode
    (with-current-buffer buffer
      (kill-all-local-variables)
      ;; FIXME: hotfix against sql-workbench
      (setq-local font-lock-keywords nil)
      (setq truncate-lines t)
      (buffer-disable-undo)
      (setq cursor-type nil))
    ;; TODO: if we have actions which could use "current" buffer
    ;; during session (e.g. show context of this occur line), we
    ;; should show that buffer in a separate (existing?) window.  The
    ;; operations to restore the original state should go into
    ;; `sallet-cleanup-candidate-window'.  See also
    ;; `helm-always-two-windows'.
    (switch-to-buffer buffer)
    (sallet-render-state state t)
    (condition-case _var
        (minibuffer-with-setup-hook (lambda () (sallet-minibuffer-setup state))
          ;; TODO: add support to pass maps
          ;; TODO: propertize prompt
          (read-from-minibuffer
           ">>> " nil
           (let ((map (make-sparse-keymap)))
             (set-keymap-parent map minibuffer-local-map)
             (define-key map (kbd "C-n") 'sallet-candidate-up)
             (define-key map (kbd "C-p") 'sallet-candidate-down)
             (define-key map (kbd "C-o") 'sallet-candidate-next-source)
             (define-key map (kbd "C-v") 'sallet-scroll-up)
             (define-key map (kbd "M-v") 'sallet-scroll-down)
             map))
          (sallet-default-action))
      ;; TODO: do we want `kill-buffer-and-window?'
      (quit (sallet-cleanup-candidate-window state))
      (error (sallet-cleanup-candidate-window state)))))

;; TODO: figure out how to avoid the global state here: sallet-state
(defun sallet-candidate-up ()
  "Move up one candidate in the candidate buffer."
  (interactive)
  (when (< (sallet-state-get-selected-candidate sallet-state)
           (1- (sallet-state-get-number-of-all-candidates sallet-state)))
    (sallet-state-incf-selected-candidate sallet-state)))

(defun sallet-candidate-down ()
  "Move down one candidate in the candidate buffer."
  (interactive)
  (when (> (sallet-state-get-selected-candidate sallet-state) 0)
    (sallet-state-decf-selected-candidate sallet-state)))

(defun sallet-candidate-next-source ()
  "Set the current selected candidate to the first candidate of next source."
  (interactive)
  (let* ((current (sallet-state-get-selected-candidate sallet-state))
         ;; TODO: rewrite using text properties: mark the header with
         ;; some property when rendering, then find the next such
         ;; property after the point, go to the next line, extract the
         ;; candidate index, set it as current candidate
         (candidates-per-source (--remove
                                 (= 0 it)
                                 (--map (length (sallet-source-get-processed-candidates it))
                                        (sallet-state-get-sources sallet-state))))
         (offsets (-butlast (nreverse (--reduce-from (cons (+ it (car acc)) acc) (list 0) candidates-per-source))))
         (next (--first (< current it) offsets)))
    (unless next (setq next 0))
    (sallet-state-set-selected-candidate sallet-state next)))

(defun sallet--scroll-offset ()
  "Get the offset for scrolling up/down."
  (/ (window-height
      (get-buffer-window
       (sallet-state-get-candidate-buffer sallet-state))) 2))

(defun sallet-scroll-up ()
  "Scroll candidates upwards, revealing later candidates."
  (interactive)
  (let ((index (min (+ (sallet-state-get-selected-candidate sallet-state)
                       (sallet--scroll-offset))
                    (1- (sallet-state-get-number-of-all-candidates sallet-state)))))
    (sallet-state-set-selected-candidate sallet-state index)))

(defun sallet-scroll-down ()
  "Scroll candidates downwards, revealing previous candidates."
  (interactive)
  (let ((index (max (- (sallet-state-get-selected-candidate sallet-state)
                       (sallet--scroll-offset))
                    0)))
    (sallet-state-set-selected-candidate sallet-state index)))

(defun sallet-default-action ()
  "Default sallet action."
  (sallet-cleanup-candidate-window sallet-state)
  (-when-let ((source . cand) (sallet-state-get-selected-source sallet-state))
    (funcall (sallet-source-get-action source) source cand)))

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

(defun sallet-occur-async ()
  "Run async occur sallet."
  (interactive)
  (sallet (list sallet-source-occur-async)))

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

(defun sallet-imenu ()
  "Run imenu sallet."
  (interactive)
  (sallet (list sallet-source-imenu)))

;; TODO: write sallet for opening files

(defun sallet-process-filter-line-buffering-decorator (filter)
  "Decorate a process FILTER with line-buffering logic.

Return a new process filter based on FILTER.

FILTER is a function which could be used with `set-process-filter'.

This decorator buffers input until it can pass a complete line
further to the supplied FILTER.  It is useful as a buffer between
a process producing data and an Emacs function operating on the
data which expects to get complete lines as input."
  (let ((data ""))
    (lambda (process string)
      (let* ((line-data (split-string (concat data string) "\n")))
        (while (cdr line-data)
          (funcall filter process (car line-data))
          (!cdr line-data))
        (setq data (car line-data))))))

(defun sallet-process-filter-linewise-candidate-decorator (processor source state)
  "Turn a PROCESSOR into a candidate generating process filter.

PROCESSOR is a function which given a string (usually a complete
line of output of a program) generates one candidate from it.

SOURCE is an instance of sallet source.  The generated candidates
are placed in this source's candidates vector.

STATE is a sallet state."
  (let ((n 0))
    (sallet-source-set-candidates source (make-vector 32 nil))
    (sallet-process-filter-line-buffering-decorator
     (lambda (_process string)
       (let* ((buffer (sallet-source-get-candidates source))
              (cand (funcall processor string))
              (bl (length buffer)))
         (when (= n bl)
           (let ((new-buffer (make-vector (* 2 bl) nil))
                 (i 0))
             (mapc (lambda (x)
                     (aset new-buffer i x)
                     (setq i (1+ i)))
                   buffer)
             (setq buffer new-buffer)
             (sallet-source-set-candidates source new-buffer)))
         (when (= (mod n 256) 0)
           (sallet-update-candidates state source)
           ;; TODO: do we really want to render from here?  Seems like
           ;; too tight coupling
           (sallet-render-state state t))
         (aset buffer n cand)
         (setq n (1+ n))
         (sallet-source-set-candidates source buffer))))))

(defun sallet-register-point ()
  "Sallet for point registers."
  (interactive)
  (sallet (list sallet-source-register-point)))

(provide 'sallet)

;; Local Variables:
;; eval: (add-to-list 'imenu-generic-expression '("Sallet sources" "\\(^(sallet-defsource +\\)\\(\\_<.+?\\_>\\)" 2))
;; End:

;;; sallet.el ends here
