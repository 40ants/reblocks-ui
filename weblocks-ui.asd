(defsystem weblocks-ui
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko"
  :license "BSD"
  :class :package-inferred-system
  :pathname "src"
  :depends-on (:weblocks-parenscript
               "weblocks-ui/core"
               "weblocks-ui/form")
  :description "A set of UI widgets for weblocks!"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.rst"
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
  :in-order-to ((test-op (test-op weblocks-ui-test))))
