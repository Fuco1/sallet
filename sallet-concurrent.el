;; -*- lexical-binding: t -*-

(require 'deferred)
(require 'sallet-core)
(require 'ov)

(defcustom csallet-spinner-string "⣷⣯⣟⡿⢿⣻⣽⣾"
  "String of characters used to loop as a spinner."
  :type '(radio
          (const :tag "Basic ASCII (-\\|/)" "-\\|/")
          (const :tag "Unicode Braille Positive (⠈⠐⠠⢀⡀⠄⠂⠁)" "⠈⠐⠠⢀⡀⠄⠂⠁")
          (const :tag "Unicode Braille Negative (⣷⣯⣟⡿⢿⣻⣽⣾)" "⣷⣯⣟⡿⢿⣻⣽⣾")
          (const :tag "Unicode Arrows (←↖↑↗→↘↓↙)" "←↖↑↗→↘↓↙")
          (string :tag "Custom")))

(defun -insert-by! (item comparator list)
  "Insert new ITEM according to COMPARATOR into a sorted LIST."
  (if (not list)
      (list item)
    (let ((done nil)
          (start list))
      (while (and (consp list)
                  (cdr list)
                  (not done))
        (if (not (funcall comparator item (car list)))
            (!cdr list)
          (setcdr list (cons (car list) (cdr list)))
          (setcar list item)
          (setq done t)))
      (unless done
        (if (not (funcall comparator item (car list)))
            (setcdr list (cons item (cdr list)))
          (setcdr list (cons (car list) (cdr list)))
          (setcar list item)))
      start)))

(defun csallet-occur-generator (prompt buffer)
  "Generate list of lines matching PROMPT in BUFFER."
  (let ((continue (point-min))
        (cur-line 0))
    (lambda (_ _)
      (let ((re nil)
            (has-match nil)
            (end-time (time-add (current-time) (list 0 0 10000 0))))
        (with-current-buffer buffer
          (save-excursion
            (when continue
              (goto-char continue)
              (while (and
                      (time-less-p (current-time) end-time)
                      (setq has-match (search-forward prompt nil t)))
                (setq cur-line (+ cur-line (count-lines continue (point))))
                (push (list (thing-at-point 'line)
                            (list :line cur-line)) re)
                (forward-line)
                (setq continue (point)))
              (unless has-match (setq continue nil)))
            (list :candidates (nreverse re)
                  :finished (not continue))))))))

(defun csallet-make-cached-generator (generator)
  "Make cached generator.

GENERATOR is a function which returns a simple list of
candidates, all of them in single invocation.

First time the resulting function is called it will return all
the generated data.  On the subsequent calls it returns no
additional candidates and declares itself as finished."
  (let (already-run)
    ;; ignored argument due to deferred requiring it in the callback
    (lambda (_ _)
      (list :candidates (unless already-run
                          (prog1 (funcall generator)
                            (setq already-run t)))))))

(defun csallet-make-mapped-generator (generator fn)
  "Make mapped generator.

GENERATOR is a function which returns a simple list of
candidates, all of them in a single invocation.

FN is then applied to each candidate as they are streamed to the
following stages.

The assumption here is that the GENERATOR function returns very
fast but the FN processing each candidate might be slow."
  (let ((stage (csallet-make-buffered-stage fn))
        (already-run nil))
    (lambda (_ pipeline-data)
      (funcall stage
               (unless already-run
                 (prog1 (funcall generator)
                   (setq already-run t)))
               pipeline-data))))

(defun csallet-make-process-generator (process-creator processor prompt)
  "Make a generator processing process output line by line.

PROCESS-CREATOR is a function which returns a running process
responsible for generating the candidates.  It takes two
arguments, a buffer which is the buffer where the output is
buffered and the PROMPT.

PROCESSOR is a function taking one argument, the current line,
and returns a candidate.

PROMPT is the current prompt.

The return value is a plist with two keys:

- :constructor is a function used to generate the candidates,
- :destructor is a function called in the on-cancel event of the
  pipeline, see `csallet-make-pipeline'."
  (let* ((buffer (generate-new-buffer " *sallet-concurrent-process*"))
         (process nil)
         (continue (point-min)))
    (list
     :constructor
      (lambda (_ _)
        (unless process
          (setq process (funcall process-creator buffer prompt)))
        (let ((end-time (time-add (current-time) (list 0 0 10000 0)))
              (re nil))
          (with-current-buffer buffer
            (when continue
              (goto-char continue)
              (while (and
                      continue
                      (save-excursion
                        (goto-char (line-end-position))
                        (looking-at-p "\n"))
                      (time-less-p (current-time) end-time))
                (if (and (not (process-live-p process))
                         (or (looking-at-p "^$")
                             (eobp)))
                    (setq continue nil)
                  (let ((candidate
                         (funcall processor (buffer-substring-no-properties
                                             (point) (line-end-position)))))
                    (push (list candidate nil) re))
                  (forward-line 1)
                  (setq continue (point))))))
          (list :candidates (nreverse re)
                :finished (not continue))))
      :destructor (lambda ()
                    (when (and process (process-live-p process))
                      (delete-process process))
                    (when (buffer-live-p buffer)
                      (kill-buffer buffer))))))

(defun csallet-make-buffered-stage (processor)
  "Make a buffered stage function out of PROCESSOR.

PROCESSOR is a function taking in one candidate and returning
either nil if the candidate should be discarded or an updated
candidate (really any non-nil value) which will be passed to the
next stage.

Return a function taking in a list of candidates to process.  If
the candidates can not be processed in the allocated time (10ms)
they are buffered internally.  On the next invocation the
passed-in candidates are added to the end of the buffer and the
buffered candidates are processed first."
  (let ((processable-candidates nil))
    (lambda (additional-candidates pipeline-data)
      (setq processable-candidates
            (-concat processable-candidates additional-candidates))
      (let ((end-time (time-add (current-time) (list 0 0 10000 0)))
            (re nil))
        (-each-while
            processable-candidates
            (lambda (_) (time-less-p (current-time) end-time))
          (lambda (candidate)
            (-when-let (valid-candidate (funcall processor candidate))
              (push valid-candidate re))
            (!cdr processable-candidates)))
        (list :candidates (nreverse re)
              :finished (= 0 (length processable-candidates)))))))

(defun csallet-sallet-filter-wrapper (filter prompt)
  "Wrap a sallet FILTER to behave like csallet candidate processor.

FILTER has the sallet interface, so on input it expects

- a vector of candidates
- a list of indices
- the pattern to search.

PROMPT is the patter passed to FILTER.

Return a function taking in csallet candidate (a list of
candidate and user-data)."
  (lambda (candidate)
    (-when-let (ok (funcall filter
                            (vector (sallet-car-maybe candidate))
                            (list (list 0 (sallet-list-maybe candidate 'cadr)))
                            prompt))
      (list
       (sallet-car-maybe candidate)
       (sallet-list-maybe (car ok) 'cdr)))))

(defun csallet-occur-filter (candidate user-data pattern)
  (when (string-match-p (regexp-quote pattern) candidate)
    (list candidate user-data)))

(defun csallet-occur-matcher (prompt)
  (csallet-make-buffered-stage
   (lambda (candidate)
     (csallet-occur-filter (sallet-car-maybe candidate)
                           (sallet-list-maybe candidate 'cadr)
                           prompt))))

(defun csallet-occur-render-candidate (candidate)
  (-let* (((candidate user-data) candidate))
    (format "%5d: %s"
            (plist-get user-data :line)
            candidate)))

(defun csallet-make-buffered-sorter (indexer)
  (let ((processable-candidates nil)
        (sorted-candidates nil))
    (lambda (additional-candidates pipeline-data)
      (setq processable-candidates (-concat processable-candidates additional-candidates))
      (let ((end-time (time-add (current-time) (list 0 0 50000 0)))
            (re nil))
        (-each-while processable-candidates
            (lambda (_) (time-less-p (current-time) end-time))
          (lambda (candidate)
            (let ((index (funcall indexer candidate)))
              (setq sorted-candidates
                    (-insert-at index candidate sorted-candidates))
              (let* ((user-data (cadr candidate))
                     (user-data (plist-put user-data :index index)))
                (push (list (car candidate) user-data) re))
              (!cdr processable-candidates))))
        (list :candidates (nreverse re)
              :finished (= 0 (length processable-candidates)))))))

(defun csallet-make-sorting-updater (comparator)
  (let ((sorted-candidates nil)
        (processable-candidates nil)
        (comparator (lambda (a b)
                      (let ((result (funcall comparator a b)))
                        (unless result
                          (forward-line 1))
                        result))))
    (lambda (additional-candidates pipeline-data)
      (setq processable-candidates (-concat processable-candidates additional-candidates))
      (let ((end-time (time-add (current-time) (list 0 0 25000 0))))
        (-each-while processable-candidates
            (lambda (_) (time-less-p (current-time) end-time))
          (-lambda (candidate)
            (!cdr processable-candidates)
            (goto-char (point-min))
            (setq sorted-candidates
                  (-insert-by! candidate comparator sorted-candidates))
            (insert (plist-get (cadr candidate) :rendered-candidate) "\n")))
        (list :finished (= 0 (length processable-candidates)))))))

;; copied from org-combine-plists
(defun csallet--merge-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(defun csallet-bind-processor (processor)
  (-lambda ((&plist :candidates candidates
                    :finished finished
                    :pipeline-data pipeline-data))
    (-let* (((data &as &plist
                   :candidates next-candidates
                   :pipeline-data next-pipeline-data)
             (funcall processor candidates pipeline-data))
            (next-finished (if (plist-member data :finished)
                               (plist-get data :finished)
                             t)))
      (list :candidates next-candidates
            :finished (and finished next-finished)
            :pipeline-data (csallet--merge-plists
                            pipeline-data
                            next-pipeline-data
                            (list :finished (and finished next-finished)))))))

(defun csallet--candidate-counter (property-name)
  "Return a function counting input candidates and saving the count in PROPERTY-NAME.

The total that has passed in all pipeline cycles is saved in
PROPERTY-NAME with the suffix -total appended."
  (let ((total 0)
        (total-property-name (intern (concat (symbol-name property-name)
                                             "-total"))))
    (lambda (candidates _)
      (let ((count (length candidates)))
        (list :candidates candidates
              :pipeline-data
          (list property-name count
                total-property-name (cl-incf total count)))))))

(cl-defun csallet-make-pipeline (canvas
                                 generator
                                 &key
                                 (matcher
                                  (lambda (candidates _)
                                    `(:candidates ,candidates)))
                                 (updater
                                  (csallet-make-buffered-stage
                                   (-lambda ((_ (&plist :rendered-candidate rc)))
                                     (insert rc "\n"))))
                                 (renderer (-lambda ((candidate)) candidate))
                                 on-start
                                 on-cancel)
  "Make an asynchronous pipeline.

GENERATOR is a stage (function) generating candidates.  It can
also be a plist with two keys: :constructor and :destructor, in
which case :constructor should be a function which generates the
candidates (a regular stage function) and :destructor is a
function with no arguments automatically added to the `on-cancel'
hook of the pipeline.

The constructor/destructor pairs are usually used with external
processes, where one starts the process (and emits the
candidates) and the other kills the process and cleans up when
the pipeline is cancelled.  See also
`csallet-make-process-generator'.

ON-START is a list of functions executed before the pipeline starts.

ON-CANCEL is a list of functions executed after the pipeline is
cancelled."
  (let ((current-deferred)
        (generated-counter (csallet--candidate-counter :generated-count))
        (matched-counter (csallet--candidate-counter :matched-count))
        (rendered-counter (csallet--candidate-counter :rendered-count))
        (tick 0)
        (renderer
         (csallet-make-buffered-stage
          (-lambda ((input &as candidate user-data))
            (let ((rendered-candidate
                   (propertize
                    (funcall renderer input)
                    'csallet-candidate candidate)))
              (setq user-data (plist-put
                               user-data
                               :rendered-candidate rendered-candidate))
              (list candidate user-data))))))
    (when (and (listp generator)
               (plist-member generator :constructor)
               (plist-member generator :destructor))
      (setq on-cancel (-snoc on-cancel (plist-get generator :destructor)))
      (setq generator (plist-get generator :constructor)))
    (lambda (op)
      (pcase op
        (`start
         (--each on-start (funcall it))
         (deferred:nextc (deferred:succeed nil)
           (deferred:lambda (done)
             (when (consp done)
               (setq done (plist-get done :finished)))
             (unless done
               (deferred:$
                 (setq current-deferred (deferred:wait 1))
                 (deferred:nextc it (lambda (_)
                                      (cl-incf tick)
                                      `(:finished t)))
                 (deferred:nextc it (csallet-bind-processor generator))
                 (deferred:nextc it (csallet-bind-processor generated-counter))
                 (deferred:nextc it (csallet-bind-processor matcher))
                 (deferred:nextc it (csallet-bind-processor matched-counter))
                 (deferred:nextc it (csallet-bind-processor renderer))
                 (deferred:nextc it (csallet-bind-processor rendered-counter))
                 (deferred:nextc it
                   (csallet-bind-processor
                    (csallet--run-in-canvas updater canvas)))
                 (deferred:nextc it
                   ;; TODO: Make a "side-effect" processor which
                   ;; automatically passes candidates to the output
                   ;; without changing them
                   (csallet-bind-processor
                    (-lambda (candidates
                              (&plist
                               :generated-count-total generated-count-total
                               :matched-count-total matched-count-total
                               :rendered-count-total rendered-count-total
                               :finished finished))
                      (csallet-at-header canvas
                        (csallet-with-current-source (:header)
                          ;; TODO: make spinner update time consistent
                          ;; (100ms delay?)
                          ;; TODO: make the string customizable:
                          ;; https://stackoverflow.com/questions/2685435/cooler-ascii-spinners
                          (let ((spinner
                                 (aref csallet-spinner-string
                                       (mod tick (length csallet-spinner-string)))))
                            (put-text-property
                             (point) (1+ (line-end-position))
                             'display (propertize
                                       (format-spec
                                        (or header " • source [%m/%g]%S\n")
                                        `((?m . ,matched-count-total)
                                          (?g . ,generated-count-total)
                                          (?r . ,rendered-count-total)
                                          (?s . ,spinner)
                                          (?S . ,(if finished ""
                                                   (format " (%c)" spinner)))))
                                       'face 'sallet-source-header))))
                        (put-text-property
                         (point) (1+ (line-end-position))
                         'csallet-header t))
                      (list :candidates candidates))))
                 (deferred:nextc it self))))))
        (`cancel
         (let ((this current-deferred)
               next)
           (while (setq next (deferred-next this))
             (deferred:cancel this)
             (setq this next))
           (deferred:cancel this))
         (--each on-cancel (funcall it)))))))

(defun csallet--get-canvases ()
  (with-csallet-buffer
    (ov-in 'csallet-index)))

(defun csallet--run-in-canvas (stage canvas)
  "Run STAGE in CANVAS."
  (lambda (candidates pipeline-data)
    ;; enable visibility when we render the first candidate
    (when (> (length candidates) 0)
      (overlay-put canvas 'display nil)
      (overlay-put canvas 'csallet-visible t))
    (csallet-with-canvas canvas
      (goto-char (point-max))
      (funcall stage candidates pipeline-data))
    ;; TODO: move this logic elsewhere
    (--when-let (--find (ov-val it 'csallet-visible) (csallet--get-canvases))
      (when (= (with-csallet-buffer (point)) (ov-beg it))
        (csallet-candidate-down)))))

(defmacro csallet-with-canvas (canvas &rest body)
  (declare (indent 1))
  `(with-current-buffer (overlay-buffer ,canvas)
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (overlay-start ,canvas))
         (forward-line 1)
         (narrow-to-region (point) (1- (overlay-end ,canvas)))
         ,@body))))

(defmacro csallet-with-current-source (context &rest body)
  "Bind CONTEXT in the current canvas then execute BODY in candidate buffer.

If CONTEXT is a symbol, then bind the current canvas to it.

If CONTEXT is a list, bind the properties according to the following map:

- :action -> bind current default action
- :candidate -> bind current candidate
- :canvas -> bind current canvas

If the property keyword is followed by a symbol, this is used as
the variable, otherwise it is derived from the keyword by
dropping the leading colon."
  (declare (indent 1) (debug (symbolp body)))
  (let ((properties nil)
        (bindings nil))
    (cond
     ((symbolp context)
      (push `(:canvas . ,context) properties))
     ((listp context)
      (while context
        (pcase context
          (`(,prop ,(and maybe-variable
                         (guard (and (not (keywordp maybe-variable))
                                     (symbolp maybe-variable)))) . ,_)
           (push `(,prop . ,maybe-variable) properties)
           (!cdr context)
           (!cdr context))
          (`(,prop . ,_)
           (push `(,prop . ,(intern (substring (symbol-name prop) 1))) properties)
           (!cdr context))))))
    (setq bindings
          (-map
           (-lambda ((prop . var))
             (pcase prop
               (:action `(,var (ov-val (ov-at) 'csallet-default-action)))
               (:candidate `(,var (get-text-property (point) 'csallet-candidate)))
               (:canvas `(,var (ov-at)))
               (:header `(,var (ov-val (ov-at) 'csallet-header-format)))))
           properties))
    `(with-csallet-buffer
       (let ,bindings
         ,@body))))

(defmacro csallet-at-header (canvas &rest body)
  (declare (indent 1))
  `(with-current-buffer (overlay-buffer ,canvas)
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (overlay-start ,canvas))
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SOURCES

(defun csallet-source-ag ()
  (lambda (prompt canvas)
    (csallet-make-pipeline
     canvas
     (csallet-make-process-generator
      (lambda (buffer prompt)
        (when (> (length prompt) 0)
          (start-process
           "ag" buffer "ag"
           "--nocolor" "--literal" "--line-number" "--smart-case"
           "--nogroup" "--column" "--" prompt)))
      'identity prompt))))

(defun csallet-ag ()
  (interactive)
  (csallet (csallet-source-ag)))

(defun csallet-locate-action (candidate)
  (sallet-locate-action nil candidate))

(defun csallet-source-locate ()
  (lambda (prompt canvas)
    (ov-put canvas
            'csallet-header-format
            (format " • locate --all --ignore-case %s [%%m/%%g]%%S\n" prompt)
            'csallet-default-action 'csallet-locate-action)
    (csallet-make-pipeline
     canvas
     (csallet-make-process-generator
      (lambda (buffer prompt)
        (when (> (length prompt) 0)
          (start-process "locate" buffer "locate"
                         "--all" "--ignore-case" prompt)))
      'identity prompt))))

(defun csallet-locate ()
  (interactive)
  (csallet (csallet-source-locate)))

(defun csallet-source-occur ()
  (let ((current-buffer (current-buffer)))
    (lambda (prompt canvas)
      (when (> (length prompt) 0)
        (csallet-make-pipeline
         canvas
         (csallet-occur-generator prompt current-buffer)
         :matcher (csallet-occur-matcher prompt)
         :updater (csallet-make-sorting-updater
                   (-lambda ((a) (b)) (< (length a) (length b)))))))))

(defun csallet-occur ()
  (interactive)
  (csallet (csallet-source-occur)))

(defun csallet-buffer-render-candidate (candidate)
  (sallet-buffer-renderer (car candidate) nil (cadr candidate)))

(defun csallet-buffer-matcher (prompt)
  (csallet-make-buffered-stage
   (csallet-sallet-filter-wrapper
    (lambda (candidates indices pattern)
      (sallet-compose-filters-by-pattern
       '(("\\`\\*\\(.*\\)" 1 sallet-filter-buffer-major-mode)
         ("\\`@\\(.*\\)" 1 sallet-filter-buffer-imenu)
         ("\\`#\\(.*\\)" 1 sallet-filter-buffer-fulltext)
         ("\\`//\\(.*\\)" 1 sallet-filter-buffer-default-directory-substring)
         ("\\`/\\(.*\\)" 1 sallet-filter-buffer-default-directory-flx)
         (t sallet-filter-flx-then-substring))
       candidates indices pattern))
    prompt)))

(defun csallet-buffer-action (candidate)
  (switch-to-buffer candidate))

(defun csallet-buffer-preview-buffer (candidate)
  (display-buffer candidate `(display-buffer-use-some-window)))

(defun csallet-make-source-keymap (&rest actions)
  (let ((map (make-sparse-keymap)))
    (-each actions
      (-lambda ((key command))
        (define-key map (kbd key)
          (lambda ()
            (interactive)
            (csallet-with-current-source (:candidate)
              (funcall command candidate))))))
    map))

(defun csallet-source-buffer ()
  (lambda (prompt canvas)
    (ov-put canvas
            'keymap (csallet-make-source-keymap
                     '("C-z" csallet-buffer-preview-buffer))
            'csallet-default-action 'csallet-buffer-action
            'csallet-header-format " • Buffers [%m/%g/%r]%S\n")
    (csallet-make-pipeline
     canvas
     (csallet-make-cached-generator 'sallet-buffer-candidates)
     :matcher (csallet-buffer-matcher prompt)
     :renderer 'csallet-buffer-render-candidate)))

(defun csallet-buffer ()
  (interactive)
  (csallet
   (csallet-source-buffer)
   (csallet-source-autobookmarks)
   (csallet-source-locate)
   ))

(defun csallet-autobookmarks-action (candidate)
  (abm-restore-killed-buffer candidate))

(defun csallet-autobookmarks-render-candidate (candidate)
  (sallet-autobookmarks-renderer (car candidate) nil (cadr candidate)))

(defun csallet-autobookmarks-matcher (prompt)
  (csallet-make-buffered-stage
   (csallet-sallet-filter-wrapper
    (lambda (candidates indices pattern)
      (sallet-compose-filters-by-pattern
       '(("\\`//\\(.*\\)" 1 sallet-filter-autobookmark-path-substring)
         ("\\`/\\(.*\\)" 1 sallet-filter-autobookmark-path-flx)
         ("\\`\\*\\(.*\\)" 1 sallet-filter-autobookmark-mode-flx)
         (t sallet-filter-flx-then-substring))
       candidates indices pattern))
    prompt)))

(defun csallet-source-autobookmarks ()
  (lambda (prompt canvas)
    (ov-put canvas
            'csallet-default-action 'csallet-autobookmarks-action
            'csallet-header-format " • Autobookmarks [%m/%g/%r]%S\n")
    (csallet-make-pipeline
     canvas
     (csallet-make-mapped-generator
      'abm-recent-buffers
      (lambda (c) (list (sallet-autobookmarks--candidate-creator c))))
     :matcher (csallet-autobookmarks-matcher prompt)
     :renderer 'csallet-autobookmarks-render-candidate
     :updater (csallet-make-sorting-updater
               (-on 'sallet-autobookmarks--candidates-comparator 'car)))))

(defun csallet-autobookmarks ()
  (interactive)
  (csallet (csallet-source-autobookmarks)))

(defun csallet-mixed ()
  (interactive)
  (csallet
   (csallet-source-buffer)
   (csallet-source-locate)
   (csallet-source-ag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RUNTIME

(defun csallet--get-buffer ()
  (get-buffer-create "*sallet-concurrent*"))

(defmacro with-csallet-buffer (&rest body)
  "Eval BODY inside csallet buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (csallet--get-buffer)
     ,@body))

(defmacro with-csallet-window (&rest body)
  "Eval BODY inside csallet buffer."
  (declare (indent 0) (debug t))
  `(with-selected-window (get-buffer-window (csallet--get-buffer))
     ,@body))

(defvar csallet--running-sources nil)

(defun csallet--cleanup-source (running-source)
  (ignore-errors
    (when running-source
      (funcall running-source 'cancel))))

(defun csallet--cleanup ()
  (unwind-protect
      (mapc 'csallet--cleanup-source csallet--running-sources)
    (setq csallet--running-sources nil)))

(defun csallet-default-action ()
  (csallet-with-current-source (:candidate :action)
    (funcall action candidate)))

(defun csallet--window-cleanup ()
  "Called after we exit csallet session."
  (kill-buffer (csallet--get-buffer))
  (csallet--cleanup)
  (--each (buffer-list)
    (when (string-match-p "\\` \\*Minibuf-[0-9]+\\*\\'"(buffer-name it))
      (remove-hook 'post-command-hook 'csallet--minibuffer-post-command-hook t))))

(defun csallet (&rest sources)
  (switch-to-buffer (csallet--get-buffer))
  (bury-buffer (csallet--get-buffer))
  (condition-case _var
      (minibuffer-with-setup-hook (lambda () (csallet--minibuffer-setup sources))
        (csallet--run-sources "" sources)
        (csallet--maybe-update-keymap)
        ;; TODO: add support to pass maps
        ;; TODO: propertize prompt
        (read-from-minibuffer
         ">>> " nil
         (let ((map (make-sparse-keymap)))
           (set-keymap-parent map minibuffer-local-map)
           (define-key map (kbd "C-n") 'csallet-candidate-down)
           (define-key map (kbd "C-p") 'csallet-candidate-up)
           (define-key map (kbd "C-o") 'csallet-candidate-next-source)
           (define-key map (kbd "C-v") 'csallet-scroll-up)
           (define-key map (kbd "M-v") 'csallet-scroll-down)
           map))
        (csallet-default-action)
        (csallet--window-cleanup))
    (quit (csallet--window-cleanup))
    (error (csallet--window-cleanup))))

(defun csallet--set-window-point ()
  (set-window-point (get-buffer-window (csallet--get-buffer)) (point)))

(defun csallet-next-candidate (_canvas)
  (forward-line 1)
  (csallet--set-window-point))

(defun csallet-previous-candidate (_canvas)
  (forward-line -1)
  (csallet--set-window-point))

(defun csallet-first-candidate-p (canvas)
  (save-excursion
    (forward-line -1)
    (get-text-property (point) 'csallet-header)))

(defun csallet-last-candidate-p (canvas)
  (save-excursion
    (forward-line 1)
    (= (point) (1- (ov-end canvas)))))

(defun csallet-candidate-up ()
  (interactive)
  (csallet-with-current-source canvas
    (let ((prev-fn (or (ov-val canvas 'csallet-previous-candidate-function)
                       'csallet-previous-candidate))
          (first-candidate-fn (or (ov-val canvas 'csallet-first-candidate-p-function)
                                  'csallet-first-candidate-p)))
      ;; if we are at the very first candidate of the source, jump to
      ;; previous source instead
      (if (funcall first-candidate-fn canvas)
          (--when-let (ov-prev (1+ (ov-beg canvas)))
            (when (not (equal it canvas))
              (goto-char (ov-end it))
              (forward-line -2)
              (csallet--set-window-point)))
        (funcall prev-fn canvas)))))

(defun csallet-candidate-down ()
  (interactive)
  (csallet-with-current-source canvas
    (let ((next-fn (or (ov-val canvas 'csallet-next-candidate-function)
                       'csallet-next-candidate))
          (last-candidate-fn (or (ov-val canvas 'csallet-last-candidate-p-function)
                                 'csallet-last-candidate-p)))
      ;; if we are at the very last line of the source (which is kept
      ;; empty), jump to next source instead
      (if (funcall last-candidate-fn canvas)
          (--when-let (ov-next (1- (ov-end canvas)))
            (goto-char (ov-beg it))
            (forward-line 1)
            (csallet--set-window-point))
        (funcall next-fn canvas)))))

(defun csallet-candidate-next-source ()
  (interactive)
  (csallet-with-current-source canvas
    (let ((next (or (ov-next 'csallet-visible)
                    (progn
                      (goto-char (point-min))
                      (if (ov-val (ov-at) 'csallet-visible)
                          (ov-at)
                        (ov-next 'csallet-visible))))))
      (when next
        (goto-char (ov-beg next))
        (forward-line 1)
        (csallet--set-window-point)))))

(defun csallet-scroll-up ()
  (interactive)
  (with-csallet-window
    (scroll-up)))

(defun csallet-scroll-down ()
  (interactive)
  (with-csallet-window
    (scroll-down)))

(defun csallet--maybe-update-keymap ()
  "Setup current source map as transient map in the minibuffer"
  (with-csallet-buffer
    (-when-let* ((ov (ov-at))
                 (keymap (ov-val ov 'keymap)))
      ;; We use set-transient-map here to not hide minor-mode-map.
      (if (fboundp 'set-transient-map)
          (set-transient-map keymap)
        (set-temporary-overlay-map keymap)))))

(defvar csallet--minibuffer-post-command-hook nil
  "Closure used to update sallet window on minibuffer events.

The closure is stored in function slot.")

(defun csallet--minibuffer-setup (sources)
  "Setup `post-command-hook' in minibuffer to update sallet STATE."
  (fset 'csallet--minibuffer-post-command-hook
        (let ((old-prompt ""))
          (lambda ()
            (let ((prompt (buffer-substring-no-properties 5 (point-max))))
              (unless (equal old-prompt prompt)
                (setq old-prompt prompt)
                (csallet--run-sources prompt sources)))
            (csallet--maybe-update-keymap))))
  (add-hook 'post-command-hook 'csallet--minibuffer-post-command-hook nil t))

(defun csallet--run-sources (prompt sources)
  (csallet--cleanup)
  (with-csallet-buffer
    (kill-all-local-variables)
    (setq truncate-lines t)
    (buffer-disable-undo)
    ;; (setq cursor-type nil)
    (ov-clear)
    (erase-buffer))
  (let ((canvases
         (with-csallet-buffer
           (save-excursion
             (--map-indexed
              (let ((canvas (make-overlay (point) (progn (insert "\n\n") (point)))))
                (overlay-put canvas 'display "")
                (overlay-put canvas 'csallet-index it-index)
                ;; (overlay-put canvas 'face (list :background (ov--random-color)))
                canvas)
              sources)))))
    (setq csallet--running-sources
          (-map
           (-lambda ((canvas . source))
             (csallet--run-source source prompt canvas))
           (-zip canvases sources)))))

(defun csallet--run-source (source prompt canvas)
  (-when-let (pipeline (funcall source prompt canvas))
    (funcall pipeline 'start)
    pipeline))
