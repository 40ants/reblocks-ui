(uiop:define-package #:reblocks-ui-docs/index
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks)
  (:import-from #:reblocks/doc/example
                #:defexample)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:reblocks-ui/form
                #:with-html-form
                #:error-placeholder
                #:form-error-placeholder
                #:field-error)
  (:import-from #:reblocks/html
                #:with-html)
  (:export
   #:@index
   #:@readme))
(in-package #:reblocks-ui-docs/index)


(defmethod docs-config ((system (eql (asdf:find-system "reblocks-ui-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
  #-quicklisp
  (asdf:load-system :40ants-doc-theme-40ants)

  (let ((server-url
          ;; When local examples server is running,
          ;; we'll be using it instead of production:
          (unless reblocks/doc/example::*port*
            "https://com-40ants-reblocks-ui.herokuapp.com/examples")))
    (list :theme
          (find-symbol "40ANTS-THEME"
                       (find-package "40ANTS-DOC-THEME-40ANTS"))
          :dynamic-bindings (list (cons 'reblocks/doc/example:*server-url*
                                        server-url)))))


(defsection @index (:title "Reblocks-UI"
                    :ignore-words ("API"
                                   "AJAX"
                                   "HTML"
                                   "UI"
                                   "BSD"
                                   "GET"
                                   "POST"
                                   "JS")
                    :external-docs ("https://40ants.com/reblocks/"))
  (reblocks-ui system)
  (@simple-demo section)
  (@confirmation-demo section)
  (@errors-demo section)
  (@api section))


(40ants-doc:defsection-copy @readme @index)


(defsection @intro (:title "Introduction")
  "
Reblocks-ui brings [Zurb Foundation][foundation] styling to your Weblocks application.

When you inherit your widgets from REBLOCKS-UI/CORE:UI-WIDGET. Reblocks will fetch
Zurb Foundation styles and JS automatically:

```lisp
(defwiget my-widget (reeblocks-ui:ui-widget)
  ((...))
```

[foundation]: https://foundation.zurb.com/
[quickstart]: https://40ants.com/reblocks/quickstart/
")


(defsection @api (:title "API")
  (with-html-form macro)
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


(defsection @simple-demo (:title "Simple Demo")
  "This demo shows how to process form data in a callback
   and to update the widget accordingly:"
  (simple-example reblocks-example))


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


(defexample simple-example ()
  (reblocks/widget:defwidget hello ()
    ((name
      :initform nil
      :accessor name)))
  
  (defmethod reblocks/widget:render ((widget hello))
    (cond
      ((name widget)
       (with-html
         (with-html-form
             (:POST (lambda (&key name &allow-other-keys)
                      (setf (name widget)
                            nil)
                      (reblocks/widget:update widget)))
           (:p "Hello "
               (name widget))
           (:p
            (:input :type "submit"
                    :class "button alert"
                    :value "Reset")))))
      (t
       (with-html-form
           (:POST (lambda (&key name &allow-other-keys)
                    (setf (name widget)
                          name)
                    (reblocks/widget:update widget)))
         (:p
          (:input :type "text"
                  :name "name"
                  :placeholder "Enter your name")
          (:input :type "submit"
                  :class "button"
                  :value "Add")))))))


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
