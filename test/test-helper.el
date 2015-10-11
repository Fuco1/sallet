(let ((package-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path package-dir))
(require 'sallet)
