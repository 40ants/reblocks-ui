(defpackage #:reblocks-ui/form
  (:use #:cl)
  (:import-from #:reblocks/actions
                #:make-action-url
                #:make-action)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:reblocks/utils/string
                #:humanize-name
                #:attributize-name)
  (:import-from #:quri
                #:url-encode)
  (:import-from #:reblocks/variables
                #:*action-string*)
  (:import-from #:log4cl-extras/error
                #:print-backtrace
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/context
                #:with-fields)
  (:import-from #:reblocks/widget)
  (:export
   #:render-button
   #:render-form-and-button
   #:with-html-form
   #:render-textarea
   #:render-link
   #:field-error
   #:error-placeholder
   #:form-error-placeholder
   #:form-error))
(in-package reblocks-ui/form)


(define-condition form-error (error)
  ((message :initarg :message
            :accessor error-message))
  (:report (lambda (condition stream)
             (format stream "Form error: ~A"
                     (error-message condition)))))


(define-condition field-error (form-error)
  ((name :initarg :name
         :reader field-name))
  (:report (lambda (condition stream)
             (format stream "Error for field ~S: ~A"
                     (field-name condition)
                     (error-message condition)))))


(defun field-error (name message)
  "Signals an error which will be shown for the whole form.lisp

   You need to use ERROR-PLACEHOLDER function inside the WITH-HTML-FORM macro
   to set a place where an error message should be shown. Otherwise, the error
   will be logged and ignored.

   If there is no a ERROR-PLACEHOLDER call with corresponding NAME argument,
   then error message can be shown for the whole form in a place where
   FORM-ERROR-PLACEHOLDER function was called."
  (error 'field-error
         :name name
         :message message))


(defun form-error (message)
  "Signals an error which will be shown for the whole form.lisp

   You need to use FORM-ERROR-PLACEHOLDER function inside the WITH-HTML-FORM macro
   to set a place where an error message should be shown. Otherwise, the error
   will be logged and ignored."
  (error 'form-error
         :message message))


(defun %render-form (method-type
                     action
                     body
                     &key id
                          class
                          enctype
                          (use-ajax-p t)
                          extra-submit-code
                          requires-confirmation-p
                          (confirm-question "Are you sure?")
                          (submit-fn "initiateFormAction(\"~A\", $(this), \"~A\")")
                          ;; We need this reference to update a widget
                          ;; after the field error was handled:
                          widget
                          ;; A hashmap with placeholders widgets
                          error-placeholders)
  (let* ((action (if (and (functionp action)
                          widget)
                     ;; We need this wrapper to handle form errors
                     (lambda (&rest args)
                       (block handled
                         (handler-bind ((error
                                          (lambda (c)
                                            (let ((name (typecase c
                                                          (field-error (field-name c))
                                                          (t "form-error")))
                                                  (message (typecase c
                                                             (form-error (error-message c))
                                                             (t (format nil "~A" c)))))
                                              ;; Here we want to log all unusual exception only,
                                              ;; because FORM-ERROR conditions are signaled
                                              ;; when something is wrong with user input and
                                              ;; usually we don't want to see them in application logs:
                                              (unless (typep c 'form-error)
                                                (with-fields (:traceback (print-backtrace :stream nil
                                                                                          :condition c))
                                                  (log:error "Unhandled exception")))
                                              
                                              (when (and error-placeholders
                                                         (gethash name error-placeholders))
                                                (let ((placeholder (gethash name error-placeholders)))
                                                  (setf (error-placeholder-message placeholder)
                                                        message)
                                                  (reblocks/widget:update placeholder))
                                                (return-from handled))))))
                           (apply action args))))
                     ;; Error handling works only when callback is a function
                     action))
         (action-code (make-action action))
         (on-submit (cond
                      (use-ajax-p
                       (format nil "~@[~A~]~A; return false;"
                               extra-submit-code
                               (format nil submit-fn
                                       (url-encode (or action-code ""))
                                       ;; Function session-name-string-pair was removed
                                       ;; during weblocks refactoring, so we just
                                       ;; 
                                       ""
                                       ;; (weblocks::session-name-string-pair)
                                       )))))
         (popup-name (when requires-confirmation-p
                       (symbol-name
                        (gensym "popup"))))
         ;; Here we'll close the popup and execute the real submit code.
         ;; It should be executed only when user hits "OK" button
         (on-confirmation-submit (when requires-confirmation-p
                                   (format nil
                                           "close_~A(); ~A;"
                                           popup-name
                                           on-submit)))
         (on-form-submit (if requires-confirmation-p
                             (format nil "show_~A(); return false;"
                                     popup-name)
                             on-submit)))

    (with-html
      (when requires-confirmation-p
        (:div
         :class "reveal"
         :id popup-name
         :dataset (:reveal t)

         (:form :id id :class class
                :action (get-path)
                :method (attributize-name method-type)
                :enctype enctype
                :onsubmit on-confirmation-submit

                ;; (:h1 confirm-question)
                (funcall confirm-question)

                ;; We need this to make forms work when JS is turned off
                (:input :name *action-string* :type "hidden" :value action-code)

                (:div :class "float-right"
                      (:input :type "button"
                              :class "success button"
                              :name "cancel"
                              :value "Cancel"
                              :onclick (format nil "close_~A(); return false;"
                                               popup-name))
                      (:input :type "submit"
                              :class "alert button"
                              :name "ok"
                              :value "Ok"))))
        
        (:script (format nil "

function show_~A () {
   $('~A').foundation('open');
}

function close_~A () {
   $('~A').foundation('close');
}

// Without this call, object will not
// be initialized propertly, when widget get
// loaded by AJAX call:
$('~A').foundation();

"
                         popup-name
                         popup-name
                         popup-name
                         popup-name
                         popup-name) 
                 
                 ;; (parenscript:ps*
                 ;;  `(defun show-popup ()))
                 ))


      (:form :id id
             :class class
             :action (get-path)
             :method (attributize-name method-type)
             :enctype enctype
             :onsubmit on-form-submit
              
             ;; We need this to make forms work when JS is turned off
             (:input :name *action-string* :type "hidden" :value action-code)

             ;; And here is the form's content itself
             (funcall body))
      
      )
    ;; TODO: may be return log-from into the Weblocks
    ;; (weblocks::log-form ,action-code :id ,id :class ,class)
    )
  )


(reblocks/widget:defwidget error-placeholder ()
  ((name :initarg :name
         :reader error-placeholder-name)
   (message :initform nil
            :accessor error-placeholder-message)))


(defmethod reblocks/widget:render ((widget error-placeholder))
  (when (error-placeholder-message widget)
    (with-html
      (:p :class (format nil "error ~A-error" (error-placeholder-name widget))
          (error-placeholder-message widget)))))


(defun error-placeholder (name &key (widget-class 'error-placeholder))
  "This function creates and renders a widget to show an error message related to some form field.

   It should be called inside WITH-HTML-FORM macro.

   NAME argument should be a string denoting a form field. Later, you can call FIELD-ERROR function
   to signal an error from the action function. You will need to pass the NAME as the first argument
   to the FIELD-ERROR function."
  (declare (ignore name widget-class))
  (error "This function should be called inside WITH-HTML-FORM macro."))


(defun form-error-placeholder (&key (widget-class 'error-placeholder))
  "This function creates and renders a widget to show an error for the whole form.

   It should be called inside WITH-HTML-FORM macro.

   Later, you can call FORM-ERROR function to signal an error from the action function."
  (declare (ignore widget-class))
  (error "This function should be called inside WITH-HTML-FORM macro."))



(defmacro with-html-form ((method-type
                           action &key
                                  id
                                  class
                                  enctype
                                  (use-ajax-p t)
                                  extra-submit-code
                                  requires-confirmation-p
                                  (confirm-question "Are you sure?")
                                  (submit-fn "initiateFormAction(\"~A\", $(this), \"~A\")")
                                  ;; We need this reference to update a widget
                                  ;; after the field error was handled:
                                  widget)
                          &body body
                          &environment env)
  "Transforms to a form like (:form) with standard form code (AJAX support, actions, etc.)"
  (let ((body `(lambda ()
                 ,@(spinneret::parse-html body env)))
        (confirm-question `(lambda ()
                             ,(spinneret::parse-html
                               (typecase confirm-question
                                 (string (list :h1 confirm-question))
                                 (t confirm-question))
                               env))))
    `(let ((error-placeholders (make-hash-table :test 'equal)))
       (flet ((error-placeholder (name &key (widget-class 'error-placeholder))
                (check-type name string)
                (let ((widget (make-instance widget-class
                                             :name name)))
                  (setf (gethash name error-placeholders)
                        widget)
                  (reblocks/widget:render widget)))
              (form-error-placeholder (&key (widget-class 'error-placeholder))
                (let* ((name "form-error")
                       (widget (make-instance widget-class
                                              :name name)))
                  (setf (gethash name error-placeholders)
                        widget)
                  (reblocks/widget:render widget))))
         
         (%render-form ,method-type
                       ,action
                       ,body
                       :id ,id
                       :class ,class
                       :enctype ,enctype
                       :use-ajax-p ,use-ajax-p
                       :extra-submit-code ,extra-submit-code
                       :requires-confirmation-p ,requires-confirmation-p
                       :confirm-question ,confirm-question
                       :submit-fn ,submit-fn
                       :widget ,widget
                       :error-placeholders error-placeholders)))))


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
         (action-code (make-action action))
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
