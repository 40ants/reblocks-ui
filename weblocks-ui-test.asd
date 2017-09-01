#|
  This file is a part of weblocks-ui project.
  Copyright (c) 2017 Alexander Artemenko
|#

(in-package :cl-user)
(defpackage weblocks-ui-test-asd
  (:use :cl :asdf))
(in-package :weblocks-ui-test-asd)

(defsystem weblocks-ui-test
  :author "Alexander Artemenko"
  :license "BSD"
  :depends-on (:weblocks-ui
               :prove
               :hamcrest-prove)
  :components ((:module "t"
                :components
                ((:test-file "weblocks-ui"))))
  :description "Test system for weblocks-ui"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
