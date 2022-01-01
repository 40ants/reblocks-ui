(defsystem reblocks-ui-test
  :author "Alexander Artemenko"
  :license "BSD"
  :depends-on (:reblocks-ui
               :prove
               :hamcrest-prove)
  :components ((:module "t"
                :components
                ((:test-file "reblocks-ui"))))
  :description "Test system for reblocks-ui"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
