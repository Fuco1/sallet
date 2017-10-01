;; -*- lexical-binding: t -*-

(require 'deferred)
(require 'sallet-core)

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
                      (kill-process process))))))

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
    (-when-let (((_ . user-data))
                (funcall filter
                         (vector (sallet-car-maybe candidate))
                         (list (list 0 (sallet-list-maybe candidate 'cadr)))
                         prompt))
      (list candidate user-data))))

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
                    (-insert-at index candidate sorted-candidates ))
              (let* ((user-data (cadr candidate))
                     (user-data (plist-put user-data :index index)))
                (push (list (car candidate) user-data) re))
              (!cdr processable-candidates))))
        (list :candidates (nreverse re)
              :finished (= 0 (length processable-candidates)))))))

(defun csallet-make-buffered-updater (comparator renderer)
  (let ((sorted-candidates nil)
        (processable-candidates nil)
        (comparator (lambda (a b)
                      (let ((result (funcall comparator a b)))
                        (unless result
                          (forward-line 1))
                        result))))
    (lambda (additional-candidates pipeline-data)
      (setq processable-candidates (-concat processable-candidates additional-candidates))
      (let ((end-time (time-add (current-time) (list 0 0 50000 0))))
        (-each-while processable-candidates
            (lambda (_) (time-less-p (current-time) end-time))
          (-lambda (candidate)
            (!cdr processable-candidates)
            (goto-char (point-min))
            (setq sorted-candidates
                  (-insert-by! candidate comparator sorted-candidates))
            (insert (funcall renderer candidate))))
        (list :finished (= 0 (length processable-candidates)))))))

(defun csallet-occur-updater (comparator renderer)
  (csallet-make-buffered-updater comparator renderer))

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
                               (setq next-finished (plist-get data :finished))
                             t)))
      (list :candidates next-candidates
            :finished (and finished next-finished)
            :pipeline-data (csallet--merge-plists
                            pipeline-data
                            next-pipeline-data)))))

(defun csallet--candidate-counter (property-name)
  "Return a function counting input candidates and saving the count in PROPERTY-NAME."
  (lambda (candidates _)
    (list :candidates candidates
          :pipeline-data `(,property-name ,(length candidates)))))

(cl-defun csallet-make-pipeline (canvas
                                 generator
                                 matcher
                                 updater
                                 &key
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
        (total-generated 0)
        (total-matched 0))
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
                 (deferred:nextc it (lambda (_) `(:finished t)))
                 (deferred:nextc it (csallet-bind-processor generator))
                 (deferred:nextc it
                   (csallet-bind-processor
                    (csallet--candidate-counter :generated-count)))
                 (deferred:nextc it (csallet-bind-processor matcher))
                 (deferred:nextc it
                   (csallet-bind-processor
                    (csallet--candidate-counter :matched-count)))
                 (deferred:nextc it
                   (csallet-bind-processor
                    (csallet--run-in-canvas updater canvas)))
                 (deferred:nextc it
                   (csallet-bind-processor
                    (-lambda (candidates (&plist :generated-count generated-count
                                                 :matched-count matched-count))
                      (cl-incf total-generated generated-count)
                      (cl-incf total-matched matched-count)
                      (csallet-at-header canvas
                        (put-text-property
                         (point) (1+ (line-end-position))
                         'display (propertize
                                   (format " • source [%d/%d]\n"
                                           total-matched
                                           total-generated)
                                   'face 'sallet-source-header)))
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

(defun csallet--run-in-canvas (processor canvas)
  "Run PROCESSOR in CANVAS."
  (lambda (candidates pipeline-data)
    ;; enable visibility when we render the first candidate
    (when (> (length candidates) 0)
      (overlay-put canvas 'display nil))
    (csallet-with-canvas canvas
      (goto-char (point-max))
      (funcall processor candidates pipeline-data))))

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
      'identity prompt)
     (lambda (candidates _) `(:candidates ,candidates))
     (csallet-make-buffered-stage
      (-lambda ((candidate))
        (insert candidate "\n"))))))

(defun csallet-ag ()
  (interactive)
  (csallet (csallet-source-ag)))

(defun csallet-source-locate ()
  (lambda (prompt canvas)
    (csallet-make-pipeline
     canvas
     (csallet-make-process-generator
      (lambda (buffer prompt)
        (when (> (length prompt) 0)
          (start-process "locate" buffer "locate"
                         "--all" "--ignore-case" prompt)))
      'identity prompt)
     (lambda (candidates _) `(:candidates ,candidates))
     (csallet-make-buffered-stage
      (-lambda ((candidate))
        (insert candidate "\n"))))))

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
         (csallet-occur-matcher prompt)
         (csallet-occur-updater
          (-lambda ((a) (b)) (< (length a) (length b)))
          'csallet-occur-render-candidate))))))

(defun csallet-occur ()
  (interactive)
  (csallet (csallet-source-occur)))

(defun csallet-buffer-updater (renderer)
  (csallet-make-buffered-stage
   (lambda (candidate)
     (insert (funcall renderer candidate) "\n"))))

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

(defun csallet-source-buffer ()
  (lambda (prompt canvas)
    (csallet-make-pipeline
     canvas
     (csallet-make-cached-generator 'sallet-buffer-candidates)
     (csallet-buffer-matcher prompt)
     (csallet-buffer-updater 'csallet-buffer-render-candidate))))

(defun csallet-buffer ()
  (interactive)
  (csallet (csallet-source-buffer)))

(defun csallet-mixed ()
  (interactive)
  (csallet
   (csallet-source-buffer)
   (csallet-source-locate)
   (csallet-source-ag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RUNTIME

(defvar csallet--running-sources nil)

(defun csallet--cleanup-source (running-source)
  (ignore-errors
    (when running-source
      (funcall running-source 'cancel))))

(defun csallet--cleanup ()
  (unwind-protect
      (mapc 'csallet--cleanup-source csallet--running-sources)
    (setq csallet--running-sources nil)))

(defun csallet (&rest sources)
  (condition-case _var
      (minibuffer-with-setup-hook (lambda () (csallet--minibuffer-setup sources))
        (csallet--run-sources "" sources)
        ;; TODO: add support to pass maps
        ;; TODO: propertize prompt
        (read-from-minibuffer ">>> ")
        (csallet--cleanup))
    (quit (csallet--cleanup))
    (error (csallet--cleanup))))

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
                (csallet--run-sources prompt sources))))))
  (add-hook 'post-command-hook 'csallet--minibuffer-post-command-hook nil t))

(defun csallet--run-sources (prompt sources)
  (csallet--cleanup)
  (let ((sallet-buffer (get-buffer-create "*sallet-concurrent*")))
    (with-current-buffer sallet-buffer
      (kill-all-local-variables)
      (setq truncate-lines t)
      (buffer-disable-undo)
      ;; (setq cursor-type nil)
      (ov-clear)
      (erase-buffer))
    (let ((canvases
           (with-current-buffer sallet-buffer
             (--map
              (let ((canvas (make-overlay (point) (progn (insert "\n\n") (point)))))
                (overlay-put canvas 'display "")
                ;; (overlay-put canvas 'face (list :background (ov--random-color)))
                canvas)
              sources))))
      (setq csallet--running-sources
            (-map
             (-lambda ((canvas . source))
               (csallet--run-source source prompt canvas))
             (-zip canvases sources))))))

(defun csallet--run-source (source prompt canvas)
  (-when-let (pipeline (funcall source prompt canvas))
    (funcall pipeline 'start)
    pipeline))
