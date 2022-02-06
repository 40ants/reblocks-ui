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
                #:with-html-form)
  (:import-from #:reblocks/html
                #:with-html)
  (:export
   #:@index))
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
            "https://com-40ants-reblocks-ui-examples.herokuapp.com/examples")))
    (list :theme
          (find-symbol "40ANTS-THEME"
                       (find-package "40ANTS-DOC-THEME-40ANTS"))
          :dynamic-bindings (list (cons 'reblocks/doc/example:*server-url*
                                        server-url)))))


(defsection @index (:title "Reblocks-UI")
  "This is a test"
  (with-html-form-example reblocks-example))


(defexample with-html-form-example ()
  (reblocks/widget:defwidget hello ()
    ((name
      :initform nil
      :accessor name)))
  
  (defmethod reblocks/widget:render ((widget hello))
    (cond
      ((name widget)
       (with-html
         (:p "Hello "
             (name widget))
         (:p (with-html-form
                 (:POST (lambda (&key name &allow-other-keys)
                          (setf (name widget)
                                nil)
                          (reblocks/widget:update widget)))
               (:input :type "submit"
                       :class "button"
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
