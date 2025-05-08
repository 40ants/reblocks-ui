(uiop:define-package #:reblocks-ui/popup
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-ui/core)
  (:import-from #:reblocks-lass)
  (:export #:visible-p
           #:popup-widget
           #:show-popup
           #:render-popup-content
           #:hide-popup))
(in-package #:reblocks-ui/popup)


(defwidget popup-widget (reblocks-ui:ui-widget)
  ((visible :initform nil
            :accessor visible-p))
  (:documentation "This widgets shows a popup window.

                   Inherit from this class and define a method for
                   RENDER-POPUP-CONTENT generic-function. Then you
                   will be able to instantiate your class instance
                   and call SHOW-POPUP generic function."))


(defgeneric show-popup (widget)
  (:documentation "Shows popup window.")
  (:method ((widget popup-widget))
    (setf (visible-p widget) t)
    (reblocks/widget:update widget)))


(defgeneric hide-popup (widget)
  (:documentation "Hides popup window.")
  (:method ((widget popup-widget))
    (setf (visible-p widget) nil)
    (reblocks/widget:update widget)))


(defmethod reblocks/widget:get-css-classes ((widget popup-widget))
  (append (when (visible-p widget)
            (list "active"))
          (list* "popup"
                 (call-next-method))))


(defgeneric render-popup-content (widget)
  (:documentation "Renders inner HTML for popup window.
                   You need to define a method for this generic function
                   and specialize it for your own class.")
  (:method ((widget popup-widget))
    (reblocks/html:with-html ()
      (:p (format nil "Define RENDER-POPUP-CONTENT method for ~S class."
                  (class-name (class-of widget)))))))


(defmethod reblocks/widget:render ((widget popup-widget))
  (reblocks/html:with-html ()
    (:div :class "popup-content"
          (render-popup-content widget))))


(defun make-css ()
  (reblocks-lass:make-dependency
    '(body
      (.popup
       :position fixed
       :top 0
       :left 0
       :width 100%
       :height 100vh
       :background "rgba(0,0,0,0.5)"
       :opacity 0
       :display none
       :pointer-events none
       :transition 0.5s all

       (.popup-content
        :position absolute
        :top 50%
        :left 50%
        ;; Here we are centering content and
        ;; shrinking it to a point in the center of the screen
        :transform "translate(-50%, -50%) scale(0)" 
        :background "#fff"
        ;; TODO: probably we need to customize this param
        ;; or to define it as min-width.
        :width 400px
        :padding 25px
        :transition 0.5s all))
      ((:and .popup .active)
       :display block
       :opacity 1
       :pointer-events all
       :transition 0.5s all

       (.popup-content
        ;; Seems these animations don't work when we
        ;; to UPDATE widget. Maybe we need to
        ;; SEND-SCRIPT instead and just to add a class
        ;; on the frontend, to make everything animated!
        :transform "translate(-50%, -50%) scale(1)"
        :transition 0.5s all)))))


(defmethod reblocks/dependencies:get-dependencies ((widget popup-widget))
  (list* (make-css)
         (call-next-method)))
