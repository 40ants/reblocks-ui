(uiop:define-package #:reblocks-ui-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:reblocks-ui-docs/changelog)


(defchangelog (:ignore-words ("CSS"
                              "JS"
                              "UI"
                              "40ANTS-DOC"))
  (0.15.0 2022-11-26
          "
New
===

Functions REBLOCKS-UI/FORM:GET-FIELD-ERRORS-COUNT and REBLOCKS-UI/FORM:GET-FIELD-ERRORS
were added to make it possible to check if there was any field errors during validation
and to interrupt further form processing.

An example was added to show how to process form errors and use placeholders. You'll find
it inside examples/form-errors.lisp file. Load reblocks-ui-examples system to load it.

Fixed
=====

Also, function a but inside REBLOCKS-UI/FORM:FORM-ERROR function was fixed and now it
will interrupt action and show an error inside a placeholder, created with
REBLOCKS-UI/FORM:FORM-ERROR-PLACEHOLDER function or open debugger otherwise.

Previosly this function call was ignored if there was a form error placeholder widget on a page.

")
  (0.14.0 2022-09-17
          "
New
===

* New class REBLOCKS-UI/POPUP:POPUP-WIDGET was added.
  It can be used to show popup windows inside the main screen.
")
  (0.13.0 2022-02-14
          "
Backward Incompabilities
========================

* Now action execution continues after the call to REBLOCKS-UI/FORM:FORM-ERROR
  or REBLOCKS-UI/FORM:FIELD-ERROR. This way, all form errors could be shown at once.
* CSS classes for placeholders were changed. REBLOCKS-UI/FORM:ERROR-PLACEHOLDER
  now uses \"form-error\" instead of just \"error\" and REBLOCKS-UI/FORM:FORM-ERROR-PLACEHOLDER
  uses \"alert callout\" instead of \"error\".
* Argument WIDGET was removed from REBLOCKS-UI/FORM:WITH-HTML-FORM macro.


Changes
=======

* Documentation was rewritten using 40ANTS-DOC. Now it contains
  more information plus interactive demos.


Fixes
=====

* Fixed to work with latest Spinneret which now escapes double
  and single quotes.
")
  (0.12.0 2022-01-04
          "
Move to Reblocks.
")
  (0.11.0 2021-06-02
          "
Added a way to handle and show form and field errors.

Use `error-placeholder` function and `form-error-placeholder` function
inside the `with-html-form` macro. And signal errors using `field-error`
and `form-error` functions.
")
  (0.10.0 2021-01-07
          "
* Macro `with-html-form` now can accept parameters `requires-confirmation-p` and `confirm-question`.
  If `requires-confirmation-p` is given, form shows a popup asking if user really wants to execute an action.
  Argument `confirm-question` can accept a string or a forms for spinneret. They will be used to render
  the content of the popup.
")
  (0.9.0 2020-01-31
         "
* `widget` was renamed to `ui-widget` and a deprecation warning was added.
")
  (0.8.0 2018-05-04
         "
* Do not use `fieldset` in `with-html-form`. It usually draws a box
  around the form elements and this isn't always wanted.
")
  (0.7.1 2018-02-10
         "
* Function `render-button` now supports keyword argument `:onclick`.
")
  (0.7.0 2018-02-06
         "
* Added initialization of Foundation's javascript plugins.
  This introduces dependency from `weblocks-parenscript`.
")
  (0.6.0 2018-02-01
         "
System was moved to package inferred class and fixed to work with
weblocks >= 0.25.0.
")
  (0.5.0 2018-01-11
         "
* Now  `*foundation-dependencies*` is exported from
  `weblocks.ui.core`.
  This is a list of CSS an JS dependencies of Zurb Foundation,
  which you can with to add to your application wide dependencies
  list to render pages without widgets, such as error pages.
")
  (0.4.0 2018-01-07
         "
* Fixed to work with weblocks >= 0.22.0
")
  (0.3.0 2017-11-23
         "
* Package `weblocks.ui.form` was fixed to use `spinneret` instead of
  `cl-who`, because `reblocks` version of `Weblocks` moved to it.

")
  (0.2.0 2017-11-11
         "
* Now UI widgets are depend on new `weblocks.widget:widget` class.

  This is breaking rendering for widgets. Replace old `weblocks:render-body`
  methods with `weblocks.widget:render`.
")
  (0.1.1 2017-09-21
         "
* Code was fixed to work with weblocks 0.14.0 where `*action-string*`
  was moved to weblocks.variables.
* Added `weblocks.ui.form:render-link` function, from old `weblocks/utils/html.lisp`.
")
  (0.1.0 2017-09-01
         "* Initial version."))
