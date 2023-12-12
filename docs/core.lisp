(uiop:define-package #:reblocks-ui-docs/core
  (:use #:cl
        #:reblocks-ui/core)
  (:import-from #:40ants-doc
                #:defsection)
  (:export #:@core))
(in-package #:reblocks-ui-docs/core)


(defsection @core (:title "Core")
  (@api section))


(defsection @api (:title "API")
  (ui-widget class)
  (widget class)
  (*foundation-dependencies* variable))
