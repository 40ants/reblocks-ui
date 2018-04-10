(defpackage #:weblocks-ui/form
  (:use #:cl)
  (:import-from #:weblocks/actions
                #:make-action-url
                #:function-or-action->action)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks/request
                #:get-path)
  (:import-from #:weblocks/utils/string
                #:humanize-name
                #:attributize-name)
  (:import-from #:quri
                #:url-encode)
  (:import-from #:weblocks/variables
                #:*action-string*)
  (:export
   #:render-button
   #:render-form-and-button
   #:with-html-form
   #:render-textarea
   #:render-link))
(in-package weblocks-ui/form)


(defmacro with-html-form ((method-type
                           action &key
                                    id
                                    class
                                    enctype
                                    (use-ajax-p t)
                                    extra-submit-code
                                    (submit-fn "initiateFormAction(\"~A\", $(this), \"~A\")"))
                          &body body)
  "Transforms to a form like (:form) with standard form code (AJAX support, actions, etc.)"
  (let ((action-code (gensym)))
    `(let ((,action-code (function-or-action->action ,action)))
       (with-html
         (:form :id ,id :class ,class :action (get-path)
                :method (attributize-name ,method-type) :enctype ,enctype
                :onsubmit (when ,use-ajax-p
                            (format nil "~@[~A~]~A; return false;"
                                    ,extra-submit-code
                                    (format nil ,submit-fn
                                            (url-encode (or ,action-code ""))
                                            ;; Function session-name-string-pair was removed
                                            ;; during weblocks refactoring, so we just
                                            ;; 
                                            ""
                                            ;; (weblocks::session-name-string-pair)
                                            )))
                (:fieldset
                 ,@body
                 (:input :name *action-string* :type "hidden" :value ,action-code))))
       ;; TODO: may be return log-from into the Weblocks
       ;; (weblocks::log-form ,action-code :id ,id :class ,class)
       )))


(defun render-button (name  &key
                              (value (humanize-name name))
                              id
                              (class "button")
                              (onclick "disableIrrelevantButtons(this);")
                              disabledp)
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
    (:input :name (attributize-name name)
            :type "submit"
            :id id
            :class class
            :disabled (when disabledp "disabled")
            :onclick onclick
            :value value)))


;; TODO: find a way how to pass button's value into the action's arguments
;;       right now jQuery.fn.serializeObject does not serialize values from <button> tags
(defun render-html-button (text &key
                             value
                             (name (attributize-name text))
                             id
                             (class "button")
                             (type "submit")
                             (onclick "disableIrrelevantButtons(this);")
                             disabledp)
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
    (:button :name name
             :type type
             :id id
             :class class
             :disabled (when disabledp "disabled")
             :onclick onclick
             :value value
             text)))


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
         (action-code (function-or-action->action action))
         (url (make-action-url action-code))
         (on-click (when ajaxp
                     (format nil "initiateAction(\"~A\"); return false;"
                             action-code))))
    
    (with-html
      (:a :id id
          :class class
          :href url
          :onclick on-click
          :title title
          (if render-fn
              (funcall render-fn label)
              (princ-to-string label))))))


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
                               (label (humanize-name name))
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
      (:label :for id
              label))
    (:textarea :name (attributize-name name)
               :id id
               :class class
               :disabled (when disabledp "disabled")
               value)))
