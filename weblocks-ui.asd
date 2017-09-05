#|
  This file is a part of weblocks-ui project.
  Copyright (c) 2017 Alexander Artemenko
|#

#|
  A set of UI widgets for weblocks!

  Author: Alexander Artemenko
|#


(in-package :cl-user)
(defpackage weblocks-ui-asd
  (:use :cl :asdf))
(in-package :weblocks-ui-asd)


(defsystem weblocks-ui
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko"
  :license "BSD"
  :depends-on (:weblocks)
  :serial t
  :components ((:module "src"
                :components
                ((:file "core")
                 (:file "app")
                 (:file "form")
                 (:file "weblocks-ui"))))
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

