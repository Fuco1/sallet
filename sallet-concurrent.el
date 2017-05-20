;; -*- lexical-binding: t -*-

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
  (let ((continue (point-min)))
    (lambda (end-time)
      (let (re)
        (with-current-buffer buffer
          (save-excursion
            (goto-char continue)
            (while (and
                    (time-less-p (current-time) end-time)
                    (setq continue (re-search-forward prompt nil t)))
              (push (thing-at-point 'line) re)
              (forward-line)
              (setq continue (point)))
            (list :candidates (nreverse re)
                  :finished (not continue))))))))

(defun csallet-occur-filter (candidate user-data pattern)
  (when (string-match-p pattern candidate)
    (list candidate user-data)))

;; TODO: add some utility to make "buffering matcher"

(defun csallet-occur-matcher (prompt)
  (lambda (candidates)
    (--keep (csallet-occur-filter it nil prompt) candidates)))

;; (defun csallet-occur-sorter (comparator)
;;   (let (sorted-candidates
;;         (tick 1))
;;     (lambda (additional-candidates)
;;       (-each additional-candidates
;;         (-lambda ((candidate user-data))
;;           (let ((updated-user-data (plist-put user-data :tick tick)))
;;             (setq sorted-candidates
;;                   (-insert-by! (list candidate updated-user-data)
;;                                (-on comparator 'car)
;;                                sorted-candidates)))))
;;       (cl-incf tick)
;;       sorted-candidates)))

(defun csallet-occur-render-candidate (candidate)
  (-let* (((candidate user-data) candidate))
    (format "%s: %s" (plist-get user-data :tick) candidate)))

;; (defun csallet-occur-renderer (sallet-buffer)
;;   (let ((last-tick 0))
;;     (lambda (candidates)
;;       (with-current-buffer sallet-buffer
;;         (goto-char (point-min))
;;         (-each candidates
;;           (-lambda ((candidate
;;                      (user-data &as &plist :tick tick)))
;;             ;; (insert (csallet-occur-render-candidate candidate user-data))
;;             (if (> tick last-tick)
;;                 (insert (csallet-occur-render-candidate candidate user-data))
;;               (forward-line 1))
;;             ))
;;         (cl-incf last-tick)))))

(defun csallet-occur-updater (sallet-buffer comparator renderer)
  (csallet-make-updater sallet-buffer comparator renderer))

;; TODO: figure out how to postpone/queue update here
(defun csallet-make-updater (sallet-buffer comparator renderer)
  (let (sorted-candidates
        (comparator (lambda (a b)
                      (let ((result (funcall comparator a b)))
                        (unless result
                          (forward-line 1))
                        result))))
    (lambda (additional-candidates)
      (with-current-buffer sallet-buffer
        (-each additional-candidates
          (-lambda (candidate)
            (goto-char (point-min))
            (setq sorted-candidates
                  (-insert-by! candidate comparator sorted-candidates))
            (insert (funcall renderer candidate))))))))

(defun csallet-start-pipeline (generator
                               matcher
                               updater)
  (let ((run-list))
    (lambda (op)
      (pcase op
        (`start
         (setq future
               (deferred:next
                 (deferred:lambda ()
                   (-let* ((end-time (time-add (current-time) (list 0 0 10000 0)))
                           ((&plist :candidates candidates
                                    :finished finished)
                            (funcall generator end-time)))
                     (push (deferred:$
                             (deferred:succeed candidates)
                             (deferred:nextc it matcher)
                             (deferred:nextc it updater))
                           run-list)
                     (unless finished
                       (let ((next (deferred:next self)))
                         (push next run-list)
                         next))))))
         (push future run-list))
        (`cancel
         (--each run-list (deferred:cancel it)))))))

(csallet-start-pipeline
 (csallet-occur-generator "a" (current-buffer))
 (csallet-occur-matcher "a")
 (csallet-occur-sorter (lambda (_ _) nil))
 (csallet-occur-renderer (get-buffer-create "*concurrent sallet*")))
