(defpackage #:weblocks.ui.form
  (:use #:cl)
  (:import-from #:weblocks
                #:with-html
                #:humanize-name)
  (:import-from #:cl-who
                #:htm
                #:esc)
  (:export
   #:render-button
   #:render-form-and-button
   #:with-html-form
   #:render-textarea
   #:render-link))
(in-package weblocks.ui.form)


(defmacro with-html-form ((method-type action &key id class enctype (use-ajax-p t) extra-submit-code
                                                (submit-fn "initiateFormAction(\"~A\", $(this), \"~A\")")) &body body)
  "Transforms to cl-who (:form) with standard form code (AJAX support, actions, etc.)"
  (let ((action-code (gensym)))
    `(let ((,action-code (weblocks::function-or-action->action ,action)))
       (with-html
         (:form :id ,id :class ,class :action (weblocks.request:request-path-info)
                :method (weblocks::attributize-name ,method-type) :enctype ,enctype
                :onsubmit (when ,use-ajax-p
                            (format nil "~@[~A~]~A; return false;"
                                    ,extra-submit-code
                                    (format nil ,submit-fn
                                            (quri:url-encode (or ,action-code ""))
                                            (weblocks::session-name-string-pair))))
                (:fieldset
                 ,@body
                 (:input :name weblocks.variables:*action-string* :type "hidden" :value ,action-code))))
       (weblocks::log-form ,action-code :id ,id :class ,class))))


(defun render-button (name  &key (value (humanize-name name)) id (class "button") disabledp)
  "Renders a button in a form.

   'name' - name of the html control. The name is attributized before
   being rendered.
   'value' - a value on html control. Humanized name is default.
   'id' - id of the html control. Default is nil.
   'class' - a class used for styling. By default, \"submit\".
   'disabledp' - button is disabled if true."
  (with-html
    ;; We could use <button...> here, but this way, it will
    ;; be impossible to distinguish which button was clicked if
    ;; there are many buttons in the form.
    (:input :name (weblocks::attributize-name name)
            :type "submit"
            :id id
            :class class
            :disabled (when disabledp "disabled")
            :onclick "disableIrrelevantButtons(this);"
            :value value)))


(defun render-link (action label &key (ajaxp t) id class title render-fn)
  "Renders an action into a href link. If AJAXP is true (the
default), the link will be rendered in such a way that the action will
be invoked via AJAX or will fall back to a regular request if
JavaScript is not available. When the user clicks on the link the
action will be called on the server.

ACTION may be a function or a result of a call to MAKE-ACTION.
ID, CLASS and TITLE represent their HTML counterparts.
RENDER-FN is an optional function of one argument that is reponsible
for rendering the link's content (i.e. its label). The default rendering
function just calls PRINC-TO-STRING on the label and renders it
with escaping. Internally, render-fn should use weblocks:with-html macro
to write output into the right stream."

  (let* ((*print-pretty* nil)
         (action-code (weblocks:function-or-action->action action))
         (url (weblocks:make-action-url action-code))
         (on-click (when ajaxp
                     (format nil "initiateAction(\"~A\", \"~A\"); return false;"
                             ;; TODO (svetlyak40wt): session-name-string-pair returns "" now
                             ;;                      need to investigate if it is critical for
                             ;;                      initiateAction call.
                             action-code
                             (weblocks:session-name-string-pair)))))
    
    (with-html
      (:a :id id
          :class class
          :href url
          :onclick on-click
          :title title
          (if render-fn
              (funcall render-fn label)
              (htm (esc (princ-to-string label))))))))


(defun render-form-and-button (name action &key (value (humanize-name name))
                                             (method :get)
                                             button-id
                                             (button-class "button")
                                             (use-ajax-p t)
                                             form-id
                                             form-class)
  "Renders a button within a form. This function can be used a short
cut to quickly render a sumbit button."
  (with-html-form (method action
                          :use-ajax-p use-ajax-p
                          :id form-id :class form-class)
    (render-button name :value value :id button-id :class button-class)))



(defun render-textarea (name &key
                               (label (weblocks::humanize-name name))
                               value
                               id
                               class
                               disabledp)
  "Renders a textarea

   'name' - name of the html control. The name is attributized before
   being rendered.
   'value' - a value on html control. Humanized name is default.
   'id' - id of the html control. Default is nil.
   'class' - a class used for styling. By default, \"submit\".
   'disabledp' - button is disabled if true."
  (with-html
    (when label
      (htm (:label :for id
                   (esc label))))
    (:textarea :name (weblocks::attributize-name name)
               :id id
               :class class
               :disabled (when disabledp "disabled")
               (esc value))))
