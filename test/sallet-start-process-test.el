(ert-deftest sallet-process-args-test nil
  (should (equal (sallet-process-args (list "grep" "-n" (when t (list "-E" "foo")) "this.el"))
                 (list "grep" "-n" "-E" "foo" "this.el")))
  (should (equal (sallet-process-args (list "grep" "-n" (when nil (list "-E" "foo")) "this.el"))
                 (list "grep" "-n" "this.el")))
  (should (equal (sallet-process-args (list "grep" "-n" "this.el"))
                 (list "grep" "-n" "this.el")))
  (should (equal (sallet-process-args (list "grep" "-n" (when t "foo") "this.el"))
                 (list "grep" "-n" "foo" "this.el"))))
