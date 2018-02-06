(defpackage #:weblocks-ui/core
  (:use #:cl)
  (:nicknames #:weblocks-ui)
  
  (:import-from #:weblocks-parenscript)
  (:import-from #:parenscript
                #:chain)
  (:import-from #:weblocks/widget
                #:defwidget)
  (:import-from #:weblocks/dependencies
                #:make-dependency
                #:get-dependencies)
  (:import-from #:weblocks/utils/string
                #:attributize-name
                #:humanize-name)
  (:import-from #:weblocks/utils/i18n
                #:translate)
  (:import-from #:weblocks/html
                #:with-html-string)
  (:export
   #:widget
   #:*foundation-dependencies*))
(in-package weblocks-ui/core)


(defwidget widget ()
  ()
  (:documentation "Use this class as a parent for all widgets, who use UI."))


(defvar *foundation-dependencies*
  (list (make-dependency
          "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/js/foundation.min.js"
          :integrity "sha256-mRYlCu5EG+ouD07WxLF8v4ZAZYCA6WrmdIXyn1Bv9Vk="
          :crossorigin "anonymous")
        (make-dependency
          "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/css/foundation.min.css"
          :integrity "sha256-GSio8qamaXapM8Fq9JYdGNTvk/dgs+cMLgPeevOYEx0="
          :crossorigin "anonymous")
        
        ;; After page will load, we have to activate Foundation plugins
        (weblocks-parenscript:make-dependency
          (chain (j-query document)
                 (ready (lambda ()
                          (chain (j-query document)
                                 (foundation)))))))

  "Dependencies for widgets based on Foundation framework.

   Also may be useful if you want to include them as a whole app's
   dependencies.

   To calculate right integity value, use:
   curl https://url | openssl dgst -sha256 -binary | openssl enc -base64 -A")


(defmethod get-dependencies ((widget widget))
  (log:debug "Returning new-style dependencies for UI widget.")

  (append *foundation-dependencies*
          (call-next-method)))


(defun button-wt (&key value name id class disabledp submitp)
  (with-html-string
    (:input :name name
            :type "submit"
            :id id
            :class (format nil "btn ~A ~A"
                           class
                           (when submitp "btn-primary"))
            :value value
            :disabled (when disabledp "disabled")
            :onclick "disableIrrelevantButtons(this);")))

;; (deftemplate :button-wt 'button-wt 
;;              :application-class 'weblocks::twitter-bootstrap-webapp)


(defun render-button (name  &key (value (translate (humanize-name name)))
                                 id
                                 (class "submit")
                                 disabledp)
  "Renders a button in a form.

   'name' - name of the html control. The name is attributized before
   being rendered.
   'value' - a value on html control. Humanized name is default.
   'id' - id of the html control. Default is nil.
   'class' - a class used for styling. By default, \"submit\".
   'disabledp' - button is disabled if true."
    (button-wt
      :value value
      :name (attributize-name name)
      :id id
      :class class 
      :disabledp disabledp 
      :submitp (string= name "submit")))



