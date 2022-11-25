(uiop:define-package #:reblocks-ui-examples/form-errors
    (:use #:cl)
    (:import-from #:reblocks/app
                  #:defapp)
    (:import-from #:reblocks/widget
                  #:update
                  #:defwidget)
    (:import-from #:reblocks-ui/form
                  #:field-error
                  #:with-html-form)
    (:import-from #:reblocks/dependencies
                  #:get-dependencies)
    (:export #:start))
(in-package #:reblocks-ui-examples/form-errors)


(defapp form-errors-app
  :prefix "/")


(defwidget form-widget (reblocks-ui:widget)
  ((success :initform nil
            :accessor success)))


(defmethod reblocks/session:init ((app form-errors-app))  
  (make-instance 'form-widget))


(defmethod reblocks/widget:render ((widget form-widget))
  (flet ((on-submit (&key email password &allow-other-keys)
           (unless (position #\@ email :test #'char-equal)
             (field-error "email" "Email should have @ in it."))
           (when (< (length password)
                    8)
             (field-error "password" "Password should have 8 or more symbols."))
           
           (let ((errors-count (reblocks-ui/form:get-field-errors-count)))
             (unless (zerop errors-count)
               (reblocks-ui/form:form-error (format nil "Form contains ~A error~:P."
                                                    errors-count))))
           

           ;; If everything success
           (setf (success widget) t)
           (update widget))
         
         (on-try-again (&rest args)
           (declare (ignore args))
           (setf (success widget) nil)
           (update widget)))
    (cond
      ((success widget)
       (with-html-form (:post #'on-try-again)
         (:p "Congratulations!")
         (:p "Form was submitted successfully!")
         (:p
          (:input :type "submit"
                  :class "button success"
                  :value "Try Again"))))
      (t
       (with-html-form (:post #'on-submit)
         (:input :name "email"
                 :type "text")
         (reblocks-ui/form:error-placeholder "email")
         
         (:input :name "password"
                 :type "password")
         (reblocks-ui/form:error-placeholder "password")

         ;; Without this line, error in the action will
         ;; fall into the debugger or return 500 error.
         (reblocks-ui/form:form-error-placeholder)

         (:input :type "submit"
                 :class "button success"
                 :value "SignUp"))))))


(defun start (&rest args &key port interface)
  (declare (ignore port interface))
  (apply #'reblocks/server:start
         :apps '(form-errors-app)
         args))



(defmethod get-dependencies ((widgete form-widget))
  (list* (reblocks-lass:make-dependency
           `(.form-widget :max-width 12em
                          :margin-left auto
                          :margin-right auto
                          :margin-top 10rem))
         (call-next-method)))
