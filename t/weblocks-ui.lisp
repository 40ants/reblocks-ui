(in-package :cl-user)
(defpackage weblocks-ui-test
  (:use :cl
        :weblocks-ui
        :prove
        :hamcrest.matchers))
(in-package :weblocks-ui-test)


(plan 1)

(subtest "Replace this test with real staff."
  (assert-that (foo 1 2 3)
               (contains 1 2 3)))

(finalize)
