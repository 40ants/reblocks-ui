(in-package :cl-user)
(defpackage reblocks-ui-test
  (:use :cl
        :reblocks-ui
        :prove
        :hamcrest.matchers))
(in-package :reblocks-ui-test)


(plan 1)

(subtest "Replace this test with real staff."
  (assert-that (foo 1 2 3)
               (contains 1 2 3)))

(finalize)
