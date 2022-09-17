(uiop:define-package #:reblocks-ui-docs/popup
  (:use #:cl
        #:reblocks-ui/popup)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:reblocks-ui-docs/popup)


(defsection @popup ()
  (popup-widget class)
  (show-popup generic-function)
  (hide-popup generic-function)
  (render-popup-content generic-function)
  (visible-p (accessor popup-widget)))
