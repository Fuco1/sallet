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
  (let ((continue (point-min))
        (cur-line 0))
    (lambda (_)
      (let ((re nil)
            (end-time (time-add (current-time) (list 0 0 10000 0))))
        (with-current-buffer buffer
          (save-excursion
            (when continue
              (goto-char continue)
              (while (and
                      (time-less-p (current-time) end-time)
                      (search-forward prompt nil t))
                (setq cur-line (+ cur-line (count-lines continue (point))))
                (push (list (thing-at-point 'line)
                            (list :line cur-line)) re)
                (forward-line)
                (setq continue (point)))
              (setq continue nil))
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
    (lambda (_)
      (list :candidates (unless already-run
                          (prog1 (funcall generator)
                            (setq already-run t)))
            :finished t))))

(defun csallet-make-buffered-processor (processor)
  (let ((processable-candidates nil))
    (lambda (additional-candidates)
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

(defun csallet-occur-filter (candidate user-data pattern)
  (when (string-match-p (regexp-quote pattern) candidate)
    (list candidate user-data)))

(defun csallet-buffer-filter (candidate user-data pattern)
  (when (string-match-p (regexp-quote pattern) candidate)
    (list candidate user-data)))

(defun csallet-occur-matcher (prompt)
  (csallet-make-buffered-processor
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
    (lambda (additional-candidates)
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

(defun csallet-make-buffered-updater (sallet-buffer comparator renderer)
  (let ((sorted-candidates nil)
        (processable-candidates nil)
        (comparator (lambda (a b)
                      (let ((result (funcall comparator a b)))
                        (unless result
                          (forward-line 1))
                        result))))
    (lambda (additional-candidates)
      (setq processable-candidates (-concat processable-candidates additional-candidates))
      (let ((end-time (time-add (current-time) (list 0 0 50000 0))))
        (-each-while processable-candidates
            (lambda (_) (time-less-p (current-time) end-time))
          (-lambda (candidate)
            (with-current-buffer sallet-buffer
              (!cdr processable-candidates)
              (goto-char (point-min))
              (setq sorted-candidates
                    (-insert-by! candidate comparator sorted-candidates))
              (insert (funcall renderer candidate)))))
        (list :finished (= 0 (length processable-candidates)))))))

(defun csallet-occur-updater (sallet-buffer comparator renderer)
  (csallet-make-buffered-updater sallet-buffer comparator renderer))

(defun csallet-bind-processor (processor)
  (-lambda ((&plist :candidates candidates
                    :finished finished))
    (-let (((&plist :candidates updated-candidates
                    :finished next-finished)
            (funcall processor candidates)))
      (list :candidates updated-candidates
            :finished (and finished next-finished)))))

(defun csallet-make-pipeline (generator
                              matcher
                              updater)
  (let ((current-deferred))
    (lambda (op)
      (pcase op
        (`start
         (deferred:nextc (deferred:succeed nil)
           (deferred:lambda (done)
             (unless done
               (deferred:$
                 (setq current-deferred (deferred:wait 1))
                 (deferred:nextc it (csallet-bind-processor generator))
                 (deferred:nextc it (csallet-bind-processor matcher))
                 (deferred:nextc it (csallet-bind-processor updater))
                 (deferred:nextc it
                   (-lambda ((&plist :finished finished)) finished))
                 (deferred:nextc it self))))))
        (`cancel
         (let ((this current-deferred)
               next)
           (while (setq next (deferred-next this))
             (deferred:cancel this)
             (setq this next))
           (deferred:cancel this)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SOURCES

(defun csallet-source-occur ()
  (let ((current-buffer (current-buffer)))
    (lambda (prompt)
      (when (> (length prompt) 0)
        (csallet-make-pipeline
         (csallet-occur-generator prompt current-buffer)
         (csallet-occur-matcher prompt)
         (csallet-occur-updater
          (get-buffer-create "*sallet-concurrent*")
          (-lambda ((a) (b)) (< (length a) (length b)))
          'csallet-occur-render-candidate))))))

(defun csallet-occur ()
  (interactive)
  (csallet (csallet-source-occur)))

(defun csallet-buffer-updater (sallet-buffer renderer)
  (let ((processor
         (csallet-make-buffered-processor
          (lambda (candidate)
            (insert (funcall renderer candidate) "\n")))))
    (lambda (additional-candidates)
      (with-current-buffer sallet-buffer
        (funcall processor additional-candidates)))))

(defun csallet-buffer-render-candidate (candidate)
  (car candidate))

(defun csallet-source-buffer ()
  (lambda (prompt)
    (csallet-make-pipeline
     (csallet-make-cached-generator 'sallet-buffer-candidates)
     (csallet-occur-matcher prompt)
     (csallet-buffer-updater
      (get-buffer-create "*sallet-concurrent*")
      'csallet-buffer-render-candidate))))

(defun csallet-buffer ()
  (interactive)
  (csallet (csallet-source-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RUNTIME

(defvar csallet--deferred nil)

(defun csallet--cleanup ()
  (when csallet--deferred
    (unwind-protect
        (funcall csallet--deferred 'cancel)
      (setq csallet--deferred nil))))

(defun csallet (source)
  (condition-case _var
      (minibuffer-with-setup-hook (lambda () (csallet--minibuffer-setup source))
        (csallet--run-source "" source)
        ;; TODO: add support to pass maps
        ;; TODO: propertize prompt
        (read-from-minibuffer ">>> ")
        (csallet--cleanup))
    (quit (csallet--cleanup))
    (error (csallet--cleanup))))

(defvar csallet--minibuffer-post-command-hook nil
  "Closure used to update sallet window on minibuffer events.

The closure is stored in function slot.")

(defun csallet--minibuffer-setup (source)
  "Setup `post-command-hook' in minibuffer to update sallet STATE."
  (fset 'csallet--minibuffer-post-command-hook
        (let ((old-prompt ""))
          (lambda ()
            (let ((prompt (buffer-substring-no-properties 5 (point-max))))
              (unless (equal old-prompt prompt)
                (setq old-prompt prompt)
                (csallet--run-source prompt source))))))
  (add-hook 'post-command-hook 'csallet--minibuffer-post-command-hook nil t))

(defun csallet--run-source (prompt source)
  (csallet--cleanup)
  (with-current-buffer (get-buffer-create "*sallet-concurrent*")
    (erase-buffer))
  (setq csallet--deferred (funcall source prompt))
  (when csallet--deferred
    (funcall csallet--deferred 'start)))
