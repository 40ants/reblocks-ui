(defsystem "reblocks-ui"
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko"
  :license "BSD"
  :class :package-inferred-system
  :pathname "src"
  :serial t
  :depends-on ("reblocks-parenscript"
               ;; We need this while will not support package inferred systems:
               ;; https://github.com/ultralisp/ultralisp/issues/3
               "40ants-doc"
               
               "reblocks-ui/core"
               "reblocks-ui/form")
  :description "A set of UI widgets for Reblocks web framework!"
  :homepage "https://40ants.com/reblocks-ui/"
  :source-control (:git "https://github.com/40ants/reblocks-ui")
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq)
                (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op reblocks-ui-test))))


(asdf:register-system-packages "log4cl" '("LOG"))
