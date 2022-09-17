(uiop:define-package #:reblocks-ui-docs/form
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks)
  (:import-from #:reblocks/doc/example
                #:defexample)
  (:import-from #:reblocks-ui/form
                #:with-html-form
                #:error-placeholder
                #:form-error-placeholder
                #:field-error)
  (:import-from #:reblocks/html
                #:with-html)
  (:export #:@form))
(in-package #:reblocks-ui-docs/form)


(defsection @form (:title "HTML Forms"
                   ;; :ignore-words ("API"
                   ;;                "AJAX"
                   ;;                "HTML"
                   ;;                "UI"
                   ;;                "BSD"
                   ;;                "GET"
                   ;;                "POST"
                   ;;                "JS")
                   )
  (@confirmation-demo section)
  (@errors-demo section)
  (@api section))


(defsection @api (:title "API")
  (with-html-form macro)
  (reblocks-ui/core:ui-widget class)
  (reblocks-ui/form:error-placeholder class)
  (reblocks-ui/form:error-placeholder function)
  (reblocks-ui/form:form-error-placeholder function)

  (reblocks-ui/form:field-error condition)
  (reblocks-ui/form:field-error function)

  (reblocks-ui/form:form-error condition)
  (reblocks-ui/form:form-error function)

  (reblocks-ui/form:render-button function)
  (reblocks-ui/form:render-form-and-button function)
  (reblocks-ui/form:render-link function)
  (reblocks-ui/form:render-textarea function))


(defsection @confirmation-demo (:title "Submit Confirmation")
  "You might also want to warn user about some destructive actions.

   To do this, provide REQUIRES-CONFIRMATION-P argument to the
   WITH-HTML-FORM macro. Optionally you might provide CONFIRMATION-QUESTION
   argument with a text of the question. Pay attention how does question
   changes when you are clicking a button in this demo:"
  (confirmation-example reblocks-example))


(defsection @errors-demo (:title "Showing Errors")
  "Form processing usually includes error checking.
   You can use [ERROR-PLACEHOLDER][function] and [FORM-ERROR-PLACEHOLDER][function] inside
   the body of WITH-HTML-FORM macro to mark places where errors should be show.

   There can be only one form error placeholder and it will show errors which are not
   related to some particular field. Field error placeholders are named and usually
   should be placed above or below a field.

   After you've used placeholder inside the form, use FIELD-ERROR function inside an
   action's code to display the error related to the field or just signal any ERROR
   to show a form-wide error.

   An example below, shows both types of error. Enter \"bob\" as a login, to check
   how a form-wide error will look like:"
  (errors-example reblocks-example))


(defexample confirmation-example ()
  (reblocks/widget:defwidget demo ()
    ((done :initform nil
           :accessor done)))
  
  (defmethod reblocks/widget:render ((widget demo))
    (cond
      ((done widget)
       (with-html
         (with-html-form
             (:POST (lambda (&rest args)
                      (declare (ignore args))
                      (setf (done widget)
                            nil)
                      (reblocks/widget:update widget))
              :requires-confirmation-p t
              ;; Here we providing an alternative question
              :confirm-question "Changed your mind?")
           (:p
            (:input :type "submit"
                    :class "button alert"
                    :value "Reset")))))
      (t
       (with-html-form
           (:POST (lambda (&rest args)
                    (declare (ignore args))
                    (setf (done widget)
                          t)
                    (reblocks/widget:update widget))
            :requires-confirmation-p t)
         (:p
          (:input :type "submit"
                  :class "button"
                  :value "Submit")))))))


(defexample errors-example ()
  (reblocks/widget:defwidget login-widget ()
    ((login
      :initform nil
      :accessor login)
     (password
      :initform nil
      :accessor password)))

  (defun login-is-ok (login)
    (cond
      ((zerop (length login))
       (reblocks-ui/form:field-error "login"
                                     "Login is required."))
      ((not (alpha-char-p (elt login 0)))
       (reblocks-ui/form:field-error "login"
                                     "Login should start from a aplha character."))
      (t t)))

  (defun password-is-ok (pass)
    (cond
      ((zerop (length pass))
       (reblocks-ui/form:field-error "password"
                                     "Password is required"))
      ((< (length pass) 8)
       (reblocks-ui/form:field-error "password"
                                     (format nil "Minimum length is 8 symbols. You entered ~A."
                                             (length pass))))
      ((zerop (count-if #'digit-char-p pass))
       (reblocks-ui/form:field-error "password"
                                     (format nil "Password should contain some digits.")))
      ((zerop (count-if #'alpha-char-p pass))
       (reblocks-ui/form:field-error "password"
                                     (format nil "Password should contains some alpha characters.")))
      (t t)))
  
  (defmethod reblocks/widget:render ((widget login-widget))
    (cond
      ((and (login widget)
            (password widget))
       (with-html
         (with-html-form
             (:POST (lambda (&key &allow-other-keys)
                      (setf (login widget) nil
                            (login widget) nil)
                      (reblocks/widget:update widget)))
           (:p (format nil "Hello, ~A." (login widget)))
           (:p
            (:input :type "submit"
                    :class "button alert"
                    :value "Reset")))))
      (t
       (with-html-form
           (:POST (lambda (&key login password &allow-other-keys)
                    (when (string-equal login "bob")
                      ;; This will show a form error
                      ;; in place of FORM-ERROR-PLACEHOLDER:
                      (error "Login ~A is already taken."
                             login))
                    
                    ;; If we'll use AND instead of EVERY,
                    ;; then error will be shown only for the first
                    ;; error.
                    (when (every #'identity
                                 (list (login-is-ok login)
                                       (password-is-ok password)))
                      (setf (login widget) login
                            (password widget) password)
                      (reblocks/widget:update widget))))
         
         (reblocks-ui/form:form-error-placeholder)
         (:p
          (:input :type "text"
                  :name "login"
                  :placeholder "Enter your login, it should start from the letter.")
          (reblocks-ui/form:error-placeholder "login")
          
          (:input :type "password"
                  :name "password"
                  :placeholder "Use chars and numbers")
          (reblocks-ui/form:error-placeholder "password")
          
          (:input :type "submit"
                  :class "button"
                  :value "Login")))))))
