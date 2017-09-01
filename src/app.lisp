(defpackage #:weblocks.ui.app
  (:use #:cl)
  (:export
   #:webapp))
(in-package weblocks.ui.app)


(defclass webapp (weblocks:weblocks-webapp)
  ((responsiveness-enabled-p :initform t :initarg :responsiveness-enabled-p))
  (:metaclass weblocks:webapp-class))


(defmethod weblocks.dependencies:get-dependencies ((self webapp))
  (log:debug "Returning new-style dependencies for UI application.")
  
  (append (list ;; (weblocks.dependencies:make-remote-js-dependency
                ;;  "https://code.jquery.com/jquery-1.12.4.js"
                ;;  :integrity "sha256-ZosEbRLbNQzLpnKIkEdrPv7lOy9C27hHQ+Xp8a4MxAQ=")
                (weblocks.dependencies:make-dependency
                 "https://code.jquery.com/jquery-1.8.2.js"
;;                 :integrity "sha256-ZosEbRLbNQzLpnKIkEdrPv7lOy9C27hHQ+Xp8a4MxAQ="
                 )
                (weblocks.dependencies:make-dependency
                 "https://maxcdn.bootstrapcdn.com/bootstrap/2.2.1/js/bootstrap.js"
                 :integrity "sha384-qJ8AGIuPqPo4I2zhyRYO6vQaJrGBbTpIiXGihDsGpfMCHJD0IKH0gkPQToGD5ek3")
                (weblocks.dependencies:make-dependency
                 "https://maxcdn.bootstrapcdn.com/bootstrap/2.2.1/css/bootstrap.min.css"
                 :integrity "sha384-SxD9NhT6x7xbJZq3DchEPXL/Go+GKOxlfipD1Y+5v4WFOnsU6MNTmzRe9oPoXwCA")
                (weblocks.dependencies:make-dependency
                 "/Users/art/common-lisp/weblocks-twitter-bootstrap-application/twitter-bootstrap.css"))
          
          (call-next-method)))


(weblocks:defwidget ui-root-widget ()
  ())


(defmethod weblocks::make-root-widget ((app webapp))
  (make-instance 'ui-root-widget
                 :name "ui-root"))


;; (defmethod weblocks:render-widget ((widget ui-root-widget) &rest rest)
;;   "FO
;; "
;;   (declare (ignorable rest))
;;   "BAR")

;; (defmethod weblocks:render-widget-children ((widget ui-root-widget) &rest rest)
;;   "FO
;; "
;;   (declare (ignorable rest))
;;   (weblocks:with-html
;;     (:p "dsdad")
;;     (:div (call-next-method))))

