(require 'sallet)

(describe "Process helpers"
  (it "should construct argument lists"

    (expect
     (sallet-process-args (list "grep" "-n" (when t (list "-E" "foo")) "this.el"))
     :to-equal
      (list "grep" "-n" "-E" "foo" "this.el"))

    (expect
     (sallet-process-args (list "grep" "-n" (when nil (list "-E" "foo")) "this.el"))
     :to-equal
      (list "grep" "-n" "this.el"))
    (expect
     (sallet-process-args (list "grep" "-n" "this.el"))
     :to-equal
      (list "grep" "-n" "this.el"))

    (expect
     (sallet-process-args (list "grep" "-n" (when t "foo") "this.el"))
     :to-equal
      (list "grep" "-n" "foo" "this.el"))))
