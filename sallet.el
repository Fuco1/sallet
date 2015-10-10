;;; sallet.el --- Select candidates in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 31st December 2014
;; Package-requires: ((dash "2.10.0") (s "1.9.0") (flx "0.4") (async "1.2") (shut-up "0.3.2") (ov "1.0"))
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

(defgroup sallet ()
  "Select candidates in a buffer."
  :group 'convenience
  :prefix "sallet-")

;; TODO: define source for files in the current directory

(defun sallet-filter-autobookmark-path-substring (candidates indices pattern)
  "Keep autobookmark CANDIDATES substring-matching PATTERN against file path."
  (let ((quoted-pattern (regexp-quote pattern)))
    (--keep (sallet-predicate-path-regexp (cadr (sallet-aref candidates it)) it quoted-pattern) indices)))

(defun sallet-filter-autobookmark-path-flx (candidates indices pattern)
  "Keep autobookmark CANDIDATES flx-matching PATTERN against file path."
  (--keep (sallet-predicate-path-flx (cadr (sallet-aref candidates it)) it pattern) indices))

;; TODO: add a matcher for major mode based on extension and
;; auto-mode-alist
(defun sallet-autobookmarks-matcher (candidates state)
  "Match an autobookmark candidate using special rules.

First, the prompt is split on whitespace.  This creates a list of
patterns.

A pattern starting with / flx-matches against the path to the
file bookmark represents.

A pattern starting with // substring-matches against the path to the
file bookmark represents.

Any other non-prefixed pattern is matched using the following rules:

- If the pattern is first of this type at the prompt, it is
  flx-matched against the bookmark name.
- All the following patterns are substring matched against the
  bookmark name."
  (let* ((prompt (sallet-state-get-prompt state))
         (indices (sallet-make-candidate-indices candidates)))
    (sallet-compose-filters-by-pattern
     '(("\\`//\\(.*\\)" 1 sallet-filter-autobookmark-path-substring)
       ("\\`/\\(.*\\)" 1 sallet-filter-autobookmark-path-flx)
       (t sallet-filter-flx-then-substring))
     candidates
     indices
     prompt)))

;; TODO: improve
(defun sallet-autobookmarks-renderer (candidate _ user-data)
  "Render an `autobookmarks-mode' candidate."
  (-let* (((name path . data) candidate)
          ((&alist 'visits visits) data))
    (format "%-55s%5s  %s"
            (sallet-compose-fontifiers
             ;; TODO: create a "fontify flx after regexp" function to
             ;; simplify this common pattern
             (propertize name 'face 'sallet-recentf-buffer-name) user-data
             '(sallet-fontify-regexp-matches . :regexp-matches)
             '(sallet-fontify-flx-matches . :flx-matches))
            (propertize (if visits (int-to-string visits) "0") 'face 'sallet-buffer-size)
            (abbreviate-file-name
             (sallet-compose-fontifiers
              (propertize path 'face 'sallet-recentf-file-path) user-data
              '(sallet-fontify-regexp-matches . :regexp-matches-path)
              '(sallet-fontify-flx-matches . :flx-matches-path))))))

(sallet-defsource autobookmarks nil
  "Files saved with `autobookmarks-mode'."
  (candidates (lambda ()
                (require 'autobookmarks)
                (-keep
                 (lambda (bookmark)
                   (-when-let (name
                               (cond
                                ((assoc 'defaults (cdr bookmark))
                                 (cadr (assoc 'defaults (cdr bookmark))))
                                ((assoc 'filename (cdr bookmark))
                                 (f-filename
                                  (cdr (assoc 'filename (cdr bookmark)))))))
                     (cons name bookmark)))
                 (-sort (-lambda ((_ . (&alist 'time a))
                                  (_ . (&alist 'time b)))
                          (time-less-p b a))
                        (abm-recent-buffers)))))
  (matcher sallet-autobookmarks-matcher)
  (renderer sallet-autobookmarks-renderer)
  (action (-lambda ((_ . x)) (abm-restore-killed-buffer x)))
  (header "Autobookmarks"))

(defun sallet--imenu-flatten (alist)
  "Compute imenu candidates."
  (--mapcat (if (imenu--subalist-p it)
                (-map (-lambda ((name pos . tags)) (-cons* name pos (car it) tags)) (sallet--imenu-flatten (cdr it)))
              (list (list (car it) (cdr it))))
            alist))

(defun sallet-filter-imenu-tags-flx (candidates indices pattern)
  "Keep buffer CANDIDATES flx-matching PATTERN against imenu tags."
  (--keep (sallet-predicate-path-flx
           (mapconcat 'identity (cddr (sallet-aref candidates it)) ", ")
           it pattern) indices))

(defun sallet-imenu-renderer (c _ user-data)
  (-let* (((x _ . tags) c)
          (face (cond ((member "Variables" tags)
                       'font-lock-variable-name-face)
                      ((member "Types" tags)
                       'font-lock-type-face)
                      (t 'font-lock-function-name-face))))
    (format "%-80s%s"
            (sallet-compose-fontifiers
             (propertize x 'face face) user-data
             '(sallet-fontify-regexp-matches . :regexp-matches)
             '(sallet-fontify-flx-matches . :flx-matches))
            (sallet-compose-fontifiers
             (mapconcat 'identity tags ", ") user-data
             '(sallet-fontify-regexp-matches . :regexp-matches-path)
             '(sallet-fontify-flx-matches . :flx-matches-path)))))

(defun sallet-imenu-matcher (a b)
  (funcall (sallet-make-matcher (lambda (c i p)
                          (sallet-compose-filters-by-pattern
                           '(("\\`/\\(.*\\)" 1 sallet-filter-imenu-tags-flx)
                             (t sallet-filter-flx-then-substring)) c i p))) a b))

(defun sallet-imenu-candidates ()
  ;; We need to clean the index for `imenu--make-index-alist' to
  ;; refresh.
  (setq imenu--index-alist nil)
  (let ((initial (symbol-name (symbol-at-point)))
        (cands (--map (if (cddr it) it (-snoc it ""))
                      (--remove (< (cadr it) 0) (sallet--imenu-flatten (imenu--make-index-alist))))))
    (if initial
        (-if-let (initial (--first (equal (car it) initial) cands))
            (cons initial (--remove (equal initial it) cands))
          cands)
      cands)))

(sallet-defsource imenu nil
  "Imenu."
  (candidates sallet-imenu-candidates)
  (matcher sallet-imenu-matcher)
  (sorter sallet-sorter-flx)
  (renderer sallet-imenu-renderer)
  (action (-lambda ((_ pos))
            (cond
             ((eq major-mode 'org-mode)
              (goto-char pos)
              (org-show-context)
              (org-show-entry))
             (t (goto-char pos)))))
  (header "Imenu"))

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
  (action (-lambda ((name))
            ;; TODO: doesn't seem to work
            (bmkp-jump-1 name 'switch-to-buffer nil)))
  (header "Bookmarks"))

(sallet-defsource bookmarks-file-only-closed-only (bookmarks-file-only)
  "Bookmarks source, files only, closed files only."
  (candidates (lambda () (--keep
                          (let ((path (cdr (assoc 'filename (cdr it)))))
                            (unless (get-file-buffer path)
                              (cons (substring-no-properties (car it)) path)))
                          (bmkp-file-alist-only)))))

;; TODO: write docstring
(defun sallet-occur-get-lines (buffer prompt &optional mode no-font-lock)
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
                       (if no-font-lock
                           (buffer-substring-no-properties lb le)
                         (font-lock-fontify-region lb le)
                         (buffer-substring lb le)))))
          (push (list line (point) (line-number-at-pos)) re)))
      (vconcat (nreverse re)))))

(sallet-defsource occur nil
  "Occur source."
  (candidates nil)
  (matcher nil)
  (renderer (-lambda ((line-string _ line-number) _ _)
              ;; TODO: add face to the number
              (format "%5d:%s" line-number line-string)))
  (generator '(let ((buffer (current-buffer)))
                (lambda (_ state)
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

(sallet-defsource occur-async (occur)
  "Async occur source."
  (async t)
  (renderer (-lambda ((line-string _ line-number) _ _)
              ;; TODO: add face to the number
              ;; TODO: fontify the result line here instead of in the async process
              (format "%5d:%s" line-number line-string)))
  ;; the buffer we search is the current buffer in the async instance
  (generator (lambda (_ state)
               (let ((prompt (sallet-state-get-prompt state)))
                 ;; TODO: move this into a separate setting... this
                 ;; is going to be quite common for "computing"
                 ;; sources
                 (when (>= (length prompt) 2)
                   (sallet-occur-get-lines (current-buffer) prompt :normal :no-font-lock))))))

(sallet-defsource occur-fuzzy (occur)
  "Fuzzy occur source."
  ;; matcher is used to rank & reorder best matches on top ...
  (matcher sallet-matcher-flx)
  (sorter sallet-sorter-flx)
  ;; ... while generator is the stupidest matcher possible
  (generator '(let ((buffer (current-buffer)))
                (lambda (_ state)
                  (let ((prompt (sallet-state-get-prompt state)))
                    (when (>= (length prompt) 2)
                      (sallet-occur-get-lines buffer prompt :fuzzy)))))))

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

(defun sallet-make-generator-linewise-asyncio
    (process-creator processor &optional min-prompt-length)
  "Make a linewise generator.

PROCESS-CREATOR is a function which when called returns a process
which produces the candidates.  It takes one argument, the
current prompt.

PROCESSOR is a function taking one line of output and producing a
candidate.

MIN-PROMPT-LENGTH is the length of prompt when we spawn the
process for the first time.

Return a generator."
  (setq min-prompt-length (or min-prompt-length 3))
  (lambda (source state)
    (let ((prompt (sallet-state-get-prompt state)))
      (when (>= (length prompt) min-prompt-length)
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
               (sallet-render-state state t))))
          (sit-for 0.01)
          proc)))))

;; TODO: add arguments such as path and other "session" data we need
;; to pass to grep
;; TODO: extract the "non-updating 2nd+ argument logic"
(defun sallet-grep-make-process-creator (file-name)
  "Return a process creator for grep sallet.

FILE-NAME is the file we are grepping."
  (let ((old ""))
    (lambda (prompt)
      (let ((input (split-string prompt " ")))
        (when (or (not old)
                  (not (equal (car input) old)))
          (setq old (car input))
          (start-process
           "grep" nil "grep" "-n"
           (car input)
           file-name))))))

(sallet-defsource grep (asyncio)
  "Grep."
  (generator
   (sallet-make-generator-linewise-asyncio
    (sallet-grep-make-process-creator (buffer-file-name))
    'identity))
  (matcher (sallet-make-matcher
            (sallet-ignore-first-token-filter
             (sallet-make-tokenized-filter
              'sallet-filter-substring))))
  (renderer (lambda (c _ _) c))
  (action (lambda (c) c)))

(defun sallet-grep ()
  "Run grep sallet."
  (interactive)
  (sallet (list sallet-source-grep)))

;; TODO: see `sallet-grep-make-process-creator'.
(defun sallet-gtags-files-make-process-creator ()
  "Return a process creator for gtags-files sallet."
  (let ((old ""))
    (lambda (prompt)
      (let ((input (split-string prompt " ")))
        (when (or (not old)
                  (not (equal (car input) old)))
          (setq old (car input))
          (with-temp-buffer
            (cd (locate-dominating-file default-directory "GTAGS"))
            (apply
             'start-process
             "global" nil "global" "-P"
             (-concat
              (sallet--smart-case (car input))
              ;; TODO: for this kind of flex matching we should
              ;; replace . with [^/] so that we search only in the
              ;; base name and not the directory tree.  Additionally,
              ;; / does flex matching on the path and non-prefixed
              ;; second and further strings substring-match the entire
              ;; path (if the first token starts with /, we use . in
              ;; the pattern to get full list over the entire project)
              ;; (list (mapconcat
              ;;        (lambda (x) (char-to-string x))
              ;;        (string-to-list (car input))
              ;;        ".*"))
              (list (concat ".*" (car input) ".*"))))))))))

;; TODO: after some timeout, start generating candidates automatically
(sallet-defsource gtags-files (asyncio)
  "Grep."
  (generator
   (sallet-make-generator-linewise-asyncio
    (sallet-gtags-files-make-process-creator)
    'identity
    1))
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
  (action (lambda (c)
            (-when-let (root (locate-dominating-file default-directory "GTAGS"))
              (find-file (concat root "/" c)))))
  (header (lambda (source)
            (sallet--wrap-header-string
             (format "File in project %s" (oref source project-root))
             source))))

(defun sallet-gtags-files ()
  "Run gtags files sallet."
  (interactive)
  (sallet (list sallet-source-gtags-files)))

(defmacro sallet-run-program-on-first (&rest body)
  "Run BODY." ;; TODO: better docs
  (declare (indent 0))
  `(let ((old ""))
     (lambda (prompt)
       (let ((input (split-string prompt " ")))
         (when (or (not old)
                   (not (equal (car input) old)))
           (setq old (car input))
           ;; TODO: write something to "start global in the root" or
           ;; figure out a way to print paths from root, not relative.
           ,@body)))))

(defmacro sallet-run-program-on-first (program-creator)
  "Run BODY." ;; TODO: better docs
  (declare (indent 0))
  (let ((old ""))
    (lambda (prompt)
      (let ((input (split-string prompt " ")))
        (when (or (not old)
                  (not (equal (car input) old)))
          (setq old (car input))
          (funcall program-creator (car input)))))))

;; (sallet-run-program-on-first
;;   (with-temp-buffer
;;     ;; TODO: this should come from outside?
;;     (cd (locate-dominating-file default-directory "GTAGS"))
;;     (apply
;;      'start-process
;;      "global" nil "global" "--result" "grep" "-T"
;;      (-concat
;;       (sallet--smart-case (car input))
;;       ;; TODO: extract this "match substring anywhere in the
;;       ;; string" logic
;;       (list (concat ".*" (car input) ".*")))
;;      ;; TODO: extract this "fuzzy regexp" generator logic
;;      ;; (mapconcat
;;      ;;  (lambda (x) (char-to-string x))
;;      ;;  (string-to-list (car input))
;;      ;;  ".*")
;;      )))

(defun sallet-tags-make-process-creator ()
  "Return a process creator for gtags tags sallet."
  (let ((old ""))
    (lambda (prompt)
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
(defun sallet-gtags-tags-action (c)
  (-when-let (root (locate-dominating-file default-directory "GTAGS"))
    (save-match-data
      (let (file line)
        (string-match "^\\(.*?\\):\\(.*?\\):" c)
        ;; sigh...
        (setq file  (match-string 1 c))
        (setq line (match-string 2 c))
        (find-file (concat root "/" file))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line)))
        (recenter-top-bottom)))))

(sallet-defsource gtags-tags (asyncio)
  "Grep."
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

;; TODO: see `sallet-grep-make-process-creator'.
(defun sallet-ag-make-process-creator (root)
  "Return a process creator for gtags-files sallet.

ROOT is the directory from where we launch ag(1)."
  (let ((old ""))
    (lambda (prompt)
      (let ((input (split-string prompt " ")))
        (when (or (not old)
                  (not (equal (car input) old)))
          (setq old (car input))
          (with-temp-buffer
            (cd root)
            (start-process
             "ag" nil "ag"
             "--nocolor" "--literal" "--line-number" "--smart-case"
             "--nogroup" "--column" prompt)))))))

;; TODO: match only on content, add / matcher for path.  We should
;; acomplish this by generating better candidates, not just lines
;; (identity)
(sallet-defsource ag (asyncio)
  "Grep."
  (generator
   '(sallet-make-generator-linewise-asyncio
     (sallet-ag-make-process-creator
      (read-directory-name
       "Project root: "
       (locate-dominating-file default-directory "GTAGS")))
     'identity))
  (renderer (lambda (candidate _ user-data)
              (sallet-fontify-regexp-matches
               (plist-get user-data :regexp-matches)
               candidate)))
  ;; TODO: finish the action
  (action (lambda (c) c)))

(defun sallet-ag ()
  "Run ag sallet."
  (interactive)
  (sallet (list sallet-source-ag)))

(defun sallet-ag-files-make-process-creator (root)
  "Return a process creator for ag-files sallet.

ROOT is the directory from where we launch ag(1)."
  (let ((old ""))
    (lambda (prompt)
      (let ((input (split-string prompt " ")))
        (when (or (not old)
                  (not (equal (car input) old)))
          (setq old (car input))
          (with-temp-buffer
            (cd root)
            (start-process
             "ag" nil "ag" "--nocolor" "--literal"
             "--smart-case" "-g" prompt)))))))

(sallet-defsource ag-files (asyncio)
  "Grep."
  (ag-root nil)
  (generator
   '(sallet-make-generator-linewise-asyncio
     (sallet-ag-files-make-process-creator
      ;; TODO: we need to somehow store this thing in the source's
      ;; property `ag-root'.
      (read-directory-name
       "Project root: "
       (locate-dominating-file default-directory "GTAGS")))
     'identity))
  (renderer (lambda (candidate _ user-data)
              (sallet-fontify-regexp-matches
               (plist-get user-data :regexp-matches)
               candidate)))
  ;; TODO: finish the action
  (action (lambda (c) (find-file c))))

(defun sallet-ag-files ()
  "Run ag sallet."
  (interactive)
  (sallet (list sallet-source-ag-files)))

(defun sallet-locate-make-process-creator ()
  "Return a process creator for locate sallet."
  (let ((old ""))
    (lambda (prompt)
      (let ((input (split-string prompt " ")))
        (when (or (not old)
                  (not (equal (car input) old)))
          (setq old (car input))
          (setq input (car input))
          (apply
           'start-process
           "locate" nil "locate"
           (-concat
            ;; TODO: write something that dispatches on pattern like
            ;; we have for filters
            (unless (eq (aref input 0) ?/)
              (list "--basename"))
            (sallet--smart-case input)
            (list
             (if (eq (aref input 0) ?/)
                 (substring input 1)
               input)))))))))

(sallet-defsource locate (asyncio)
  "Grep."
  (generator
   (sallet-make-generator-linewise-asyncio
    (sallet-locate-make-process-creator)
    'identity))
  (matcher (lambda (candidates state)
             (let* ((prompt (sallet-state-get-prompt state))
                    (indices (sallet-make-candidate-indices candidates)))
               (sallet-compose-filters-by-pattern
                '(("\\`/\\(.*\\)" 1 sallet-filter-substring)
                  ("\\`\\.\\(.*\\)" 1 sallet-filter-file-extension)
                  (t sallet-filter-substring))
                candidates
                indices
                prompt))))
  (renderer (lambda (candidate _ user-data)
              (sallet-fontify-regexp-matches
               (plist-get user-data :regexp-matches)
               candidate)))
  (action (lambda (c) (find-file c))))

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
(defun sallet-render-source (state source offset)
  "Render.

OFFSET is the number of already rendered candidates before
this source.

Return number of rendered candidates."
  (with-current-buffer (sallet-state-get-candidate-buffer state)
    (let* ((selected (sallet-state-get-selected-candidate state))
           (prompt (sallet-state-get-prompt state))
           (processed-candidates (sallet-source-get-processed-candidates source))
           (renderer (sallet-source-get-renderer source))
           (coffset (- selected offset))
           (i 0))
      (sallet-render-header source)
      (-each processed-candidates
        (lambda (n)
          ;; `n' can be a number or a list returned from the
          ;; matcher---the `car' of which is then the index, the rest
          ;; is arbitrary meta data ignored at this stage (it is
          ;; useful when at the sorter stage)
          (let* ((candidate (sallet-source-get-candidate source (sallet-car-maybe n))))
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
            (setq offset (+ offset (sallet-render-source state source offset)))))
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
(defun sallet-process-source-async (state source)
  "Process SOURCE asynchronously in separate emacs."
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
          (let ((proc (sallet-process-source-async state source)))
            (sallet-state-set-futures state (plist-put futures source-id proc))))))))

(defun sallet-minibuffer-post-command (state)
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
  (let* ((buffer (get-buffer-create "*Sallet candidates*"))
         ;; make this lexically scoped
         (state (sallet-init-state sources buffer)))
    ;; TODO: add better modeline, show number of sources/candidates etc...
    (with-current-buffer buffer
      (kill-all-local-variables)
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
    (condition-case var
        (minibuffer-with-setup-hook (lambda () (sallet-minibuffer-setup state))
          ;; TODO: add support to pass maps
          ;; TODO propertize prompt
          (read-from-minibuffer ">>> " nil (let ((map (make-sparse-keymap)))
                                             (set-keymap-parent map minibuffer-local-map)
                                             (define-key map (kbd "C-n") 'sallet-candidate-up)
                                             (define-key map (kbd "C-p") 'sallet-candidate-down)
                                             (define-key map (kbd "C-o") 'sallet-candidate-next-source)
                                             map))
          (sallet-default-action))
      ;; TODO: do we want `kill-buffer-and-window?'
      (quit (sallet-cleanup-candidate-window state))
      (error (sallet-cleanup-candidate-window state)))))

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

(defun sallet-default-action ()
  "Default sallet action."
  (sallet-cleanup-candidate-window sallet-state)
  (-when-let ((source . cand) (sallet-state-get-selected-source sallet-state))
    (funcall (sallet-source-get-action source) cand)))

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
      (let* ((data (concat data string))
             (line-data (split-string data "\n")))
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
           (sallet-render-state state t))
         (aset buffer n cand)
         (setq n (1+ n))
         (sallet-source-set-candidates source buffer))))))

(provide 'sallet)

;; Local Variables:
;; eval: (add-to-list 'imenu-generic-expression '("Sallet sources" "\\(^(sallet-defsource +\\)\\(\\_<.+?\\_>\\)" 2))
;; End:

;;; sallet.el ends here
