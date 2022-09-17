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
  (:import-from #:reblocks-ui-docs/popup
                #:@popup)
  (:import-from #:reblocks-ui-docs/form
                #:@form)
  (:import-from #:reblocks-ui-docs/core
                #:@core)
  (:export #:@index
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
  (@intro section)
  (@simple-demo section)
  (@core section)
  (@form section)
  (@popup section))


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


(defsection @simple-demo (:title "Simple Demo")
  "This demo shows how to process form data in a callback
   and to update the widget accordingly:"
  (simple-example reblocks-example))


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
