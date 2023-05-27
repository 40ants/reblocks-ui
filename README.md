<a id="x-28REBLOCKS-UI-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Reblocks-UI

<a id="reblocks-ui-asdf-system-details"></a>

## REBLOCKS-UI ASDF System Details

* Version: 0.12.0

* Description: A set of `UI` widgets for Reblocks web framework!

* Licence: `BSD`

* Author: Alexander Artemenko

* Homepage: [https://40ants.com/reblocks-ui/][233f]

* Source control: [GIT][1818]

* Depends on: [40ants-doc][2c00], [log4cl][7f8b], [log4cl-extras][691c], [parenscript][7921], [quri][2103], [reblocks][184b], [reblocks-parenscript][c07c]

<a id="x-28REBLOCKS-UI-DOCS-2FINDEX-3A-3A-40INTRO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Introduction

Reblocks-ui brings [Zurb Foundation][4fd9] styling to your Weblocks application.

When you inherit your widgets from [`reblocks-ui/core:ui-widget`][923e]. Reblocks will fetch
Zurb Foundation styles and `JS` automatically:

```lisp
(defwiget my-widget (reeblocks-ui:ui-widget)
  ((...))
```
<a id="x-28REBLOCKS-UI-DOCS-2FINDEX-3A-3A-40SIMPLE-DEMO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Simple Demo

This demo shows how to process form data in a callback
and to update the widget accordingly:


```lisp
(reblocks/widget:defwidget hello
    nil
  ((name :initform nil :accessor name)))

(defmethod reblocks/widget:render ((widget hello))
  (cond
   ((name widget)
    (reblocks/html:with-html
      (reblocks-ui/form:with-html-form (:post
                                        (lambda (&key name &allow-other-keys)
                                          (setf (name widget) nil)
                                          (reblocks/widget:update widget)))
        (:p "Hello " (name widget))
        (:p (:input :type "submit" :class "button alert" :value "Reset")))))
   (t
    (reblocks-ui/form:with-html-form (:post
                                      (lambda (&key name &allow-other-keys)
                                        (setf (name widget) name)
                                        (reblocks/widget:update widget)))
      (:p (:input :type "text" :name "name" :placeholder "Enter your name")
       (:input :type "submit" :class "button" :value "Add"))))))

```


Go to [HTML documentation](https://40ants.com/reblocks-ui/) to see this code in action.

<a id="x-28REBLOCKS-UI-DOCS-2FCORE-3A-40CORE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Core

<a id="x-28REBLOCKS-UI-DOCS-2FCORE-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### API

<a id="x-28REBLOCKS-UI-2FCORE-3AUI-WIDGET-20CLASS-29"></a>

#### [class](ce5f) `reblocks-ui/core:ui-widget` (widget)

Use this class as a parent for all widgets, who use `UI`.

<a id="x-28REBLOCKS-UI-2FCORE-3AWIDGET-20CLASS-29"></a>

#### [class](6446) `reblocks-ui/core:widget` (ui-widget)

Use this class as a parent for all widgets, who use `UI`. Warning: 'widget' was renamed to 'ui-widget' and will be removed after 2020-06-01.

<a id="x-28REBLOCKS-UI-2FCORE-3A-2AFOUNDATION-DEPENDENCIES-2A-20-28VARIABLE-29-29"></a>

#### [variable](b894) `reblocks-ui/core:*foundation-dependencies*` (#<REBLOCKS/DEPENDENCIES:REMOTE-DEPENDENCY url: "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/js/foundation.min.js">
 #<REBLOCKS/DEPENDENCIES:REMOTE-DEPENDENCY url: "https://cdnjs.cloudflare.com/ajax/libs/foundation/6.4.3/css/foundation.min.css">
 #<REBLOCKS-PARENSCRIPT:PARENSCRIPT-DEPENDENCY >)

Dependencies for widgets based on Foundation framework.

Also may be useful if you want to include them as a whole app's
dependencies.

To calculate right integity value, use:
curl https://url | openssl dgst -sha256 -binary | openssl enc -base64 -A

<a id="x-28REBLOCKS-UI-DOCS-2FFORM-3A-40FORM-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## HTML Forms

<a id="x-28REBLOCKS-UI-DOCS-2FFORM-3A-3A-40CONFIRMATION-DEMO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Submit Confirmation

You might also want to warn user about some destructive actions.

To do this, provide `REQUIRES-CONFIRMATION-P` argument to the
[`with-html-form`][f976] macro. Optionally you might provide `CONFIRMATION-QUESTION`
argument with a text of the question. Pay attention how does question
changes when you are clicking a button in this demo:


```lisp
(reblocks/widget:defwidget demo
    nil
  ((done :initform nil :accessor done)))

(defmethod reblocks/widget:render ((widget demo))
  (cond
   ((done widget)
    (reblocks/html:with-html
      (reblocks-ui/form:with-html-form (:post
                                        (lambda (&rest args)
                                          (declare (ignore args))
                                          (setf (done widget) nil)
                                          (reblocks/widget:update widget))
                                        :requires-confirmation-p t
                                        :confirm-question "Changed your mind?")
        (:p (:input :type "submit" :class "button alert" :value "Reset")))))
   (t
    (reblocks-ui/form:with-html-form (:post
                                      (lambda (&rest args)
                                        (declare (ignore args))
                                        (setf (done widget) t)
                                        (reblocks/widget:update widget))
                                      :requires-confirmation-p t)
      (:p (:input :type "submit" :class "button" :value "Submit"))))))

```


Go to [HTML documentation](https://40ants.com/reblocks-ui/) to see this code in action.

<a id="x-28REBLOCKS-UI-DOCS-2FFORM-3A-3A-40ERRORS-DEMO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Showing Errors

Form processing usually includes error checking.
You can use [`error-placeholder`][8637] and [`form-error-placeholder`][3868] inside
the body of [`with-html-form`][f976] macro to mark places where errors should be show.

There can be only one form error placeholder and it will show errors which are not
related to some particular field. Field error placeholders are named and usually
should be placed above or below a field.

After you've used placeholder inside the form, use [`field-error`][5162] function inside an
action's code to display the error related to the field or just signal any `ERROR`
to show a form-wide error.

An example below, shows both types of error. Enter "bob" as a login, to check
how a form-wide error will look like:


```lisp
(reblocks/widget:defwidget login-widget
    nil
  ((login :initform nil :accessor login)
   (password :initform nil :accessor password)))

(defun login-is-ok (login)
  (cond
   ((zerop (length login))
    (reblocks-ui/form:field-error "login" "Login is required."))
   ((not (alpha-char-p (elt login 0)))
    (reblocks-ui/form:field-error "login"
                                  "Login should start from a aplha character."))
   (t t)))

(defun password-is-ok (pass)
  (cond
   ((zerop (length pass))
    (reblocks-ui/form:field-error "password" "Password is required"))
   ((< (length pass) 8)
    (reblocks-ui/form:field-error "password"
                                  (format nil
                                          "Minimum length is 8 symbols. You entered ~A."
                                          (length pass))))
   ((zerop (count-if #'digit-char-p pass))
    (reblocks-ui/form:field-error "password"
                                  (format nil
                                          "Password should contain some digits.")))
   ((zerop (count-if #'alpha-char-p pass))
    (reblocks-ui/form:field-error "password"
                                  (format nil
                                          "Password should contains some alpha characters.")))
   (t t)))

(defmethod reblocks/widget:render ((widget login-widget))
  (cond
   ((and (login widget) (password widget))
    (reblocks/html:with-html
      (reblocks-ui/form:with-html-form (:post
                                        (lambda (&key &allow-other-keys)
                                          (setf (login widget) nil
                                                (login widget) nil)
                                          (reblocks/widget:update widget)))
        (:p (format nil "Hello, ~A." (login widget)))
        (:p (:input :type "submit" :class "button alert" :value "Reset")))))
   (t
    (reblocks-ui/form:with-html-form (:post
                                      (lambda
                                          (
                                           &key login password
                                           &allow-other-keys)
                                        (when (string-equal login "bob")
                                          (error "Login ~A is already taken."
                                                 login))
                                        (when
                                            (every #'identity
                                                   (list (login-is-ok login)
                                                         (password-is-ok
                                                          password)))
                                          (setf (login widget) login
                                                (password widget) password)
                                          (reblocks/widget:update widget))))
      (reblocks-ui/form:form-error-placeholder)
      (:p
       (:input :type "text" :name "login" :placeholder
        "Enter your login, it should start from the letter.")
       (reblocks-ui/form:error-placeholder "login")
       (:input :type "password" :name "password" :placeholder
        "Use chars and numbers")
       (reblocks-ui/form:error-placeholder "password")
       (:input :type "submit" :class "button" :value "Login"))))))

```


Go to [HTML documentation](https://40ants.com/reblocks-ui/) to see this code in action.

<a id="x-28REBLOCKS-UI-DOCS-2FFORM-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### API

<a id="x-28REBLOCKS-UI-2FFORM-3AWITH-HTML-FORM-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](f1cd) `reblocks-ui/form:with-html-form` (METHOD-TYPE ACTION &KEY ID CLASS ENCTYPE (USE-AJAX-P T) EXTRA-SUBMIT-CODE REQUIRES-CONFIRMATION-P (CONFIRM-QUESTION "Are you sure?") (SUBMIT-FN "initiateFormAction(\"~A\", $(this), \"~A\")")) &BODY BODY

Wraps a body with (:form ...) using [`reblocks/html:with-html`][f21e].

* `METHOD-TYPE` argument should be a keyword `:GET` or `:POST`.

* `ACTION` argument should be a function callback which will be called on
  form submit. Form fields will be passed as keyword arguments, using their
  names. To make your code more robust, use `&ALLOW-OTHER-KEYS` in the lambda list.

* `ID`, `CLASS` and `ENCTYPE` arguments are transformed into appropriate arguments
  of `HTML` `<form ...>...</form>` node.

* `EXTRA-SUBMIT-CODE` argument might contain a list of string with simple `JS` code,
  which will be called on form submit before code provided in `SUBMIT-FN` argument.

* By default, form submission is done using `AJAX`. If you want to
  do old-school `GET` or `POST` request, set `USE-AJAX-P` argument to `NIL`.

* If `REQUIRES-CONFIRMATION-P` is true, then user will be asked a question
  defined by `CONFIRM-QUESTION` argument. Zurb Foundation's
  [modal window][5ce9] will be used
  to show a popup. See [`Submit Confirmation`][5fb1] section for
  an example of code.

<a id="x-28REBLOCKS-UI-2FCORE-3AUI-WIDGET-20CLASS-29"></a>

#### [class](ce5f) `reblocks-ui/core:ui-widget` (widget)

Use this class as a parent for all widgets, who use `UI`.

<a id="x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20CLASS-29"></a>

#### [class](10aa) `reblocks-ui/form:error-placeholder` (widget)

<a id="x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20FUNCTION-29"></a>

#### [function](dc72) `reblocks-ui/form:error-placeholder` name &key (widget-class 'error-placeholder)

This function creates and renders a widget to show an error message related to some form field.

It should be called inside [`with-html-form`][f976] macro.

`NAME` argument should be a string denoting a form field. Later, you can call [`field-error`][5162] function
to signal an error from the action function. You will need to pass the `NAME` as the first argument
to the [`field-error`][5162] function.

<a id="x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-PLACEHOLDER-20FUNCTION-29"></a>

#### [function](ac32) `reblocks-ui/form:form-error-placeholder` &key (widget-class 'form-error-placeholder)

This function creates and renders a widget to show an error for the whole form.

It should be called inside [`with-html-form`][f976] macro.

Later, you can call [`form-error`][ea04] function to signal an error from the action function.

<a id="x-28REBLOCKS-UI-2FFORM-3AFIELD-ERROR-20CONDITION-29"></a>

#### [condition](4d61) `reblocks-ui/form:field-error` (form-error)

<a id="x-28REBLOCKS-UI-2FFORM-3AFIELD-ERROR-20FUNCTION-29"></a>

#### [function](95c7) `reblocks-ui/form:field-error` name message

Signals an error which will be shown for the whole form.lisp

You need to use [`error-placeholder`][8637] function inside the [`with-html-form`][f976] macro
to set a place where an error message should be shown. Otherwise, the error
will be logged and ignored.

If there is no a `error-placeholder` ([`1`][8637] [`2`][5578]) call with corresponding `NAME` argument,
then error message can be shown for the whole form in a place where
[`form-error-placeholder`][3868] function was called.

<a id="x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20CONDITION-29"></a>

#### [condition](e214) `reblocks-ui/form:form-error` (error)

<a id="x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20FUNCTION-29"></a>

#### [function](de5c) `reblocks-ui/form:form-error` message

Signals an error which will be shown for the whole form.lisp

You need to use [`form-error-placeholder`][3868] function inside the [`with-html-form`][f976] macro
to set a place where an error message should be shown. Otherwise, the error
will be logged and ignored.

<a id="x-28REBLOCKS-UI-2FFORM-3AGET-FIELD-ERRORS-COUNT-20FUNCTION-29"></a>

#### [function](ebf1) `reblocks-ui/form:get-field-errors-count`

Returns total number of errors, reported by [`field-error`][5162] function.

You can use this function and call `form-error` ([`1`][ea04] [`2`][3765]) or interrupt action if
the result is not zero.

<a id="x-28REBLOCKS-UI-2FFORM-3AGET-FIELD-ERRORS-20FUNCTION-29"></a>

#### [function](28d7) `reblocks-ui/form:get-field-errors` field-name

Returns all errors, reported for the field with name given in `FIELD-NAME`.

<a id="x-28REBLOCKS-UI-2FFORM-3ARENDER-BUTTON-20FUNCTION-29"></a>

#### [function](fb67) `reblocks-ui/form:render-button` NAME &KEY (VALUE (HUMANIZE-NAME NAME)) ID (CLASS "button") (ONCLICK "disableIrrelevantButtons(this);") DISABLEDP

Renders a button in a form.

* `NAME` - name of the html control. The name is attributized before
  being rendered.

* `VALUE` - a value on html control. Humanized name is default.

* `ID` - id of the html control. Default is nil.

* `CLASS` - a class used for styling. By default, "submit".

* `DISABLEDP` - button is disabled if true.

<a id="x-28REBLOCKS-UI-2FFORM-3ARENDER-FORM-AND-BUTTON-20FUNCTION-29"></a>

#### [function](55be) `reblocks-ui/form:render-form-and-button` NAME ACTION &KEY (VALUE (HUMANIZE-NAME NAME)) (METHOD :GET) BUTTON-ID (BUTTON-CLASS "button") (USE-AJAX-P T) FORM-ID FORM-CLASS

Renders a button within a form. This function can be used a short
cut to quickly render a sumbit button.

<a id="x-28REBLOCKS-UI-2FFORM-3ARENDER-LINK-20FUNCTION-29"></a>

#### [function](0723) `reblocks-ui/form:render-link` action label &key (ajaxp t) id class title render-fn

Renders an action into a href link. If `AJAXP` is true (the
default), the link will be rendered in such a way that the action will
be invoked via `AJAX` or will fall back to a regular request if
JavaScript is not available. When the user clicks on the link the
action will be called on the server.

`ACTION` may be a function or a result of a call to [`reblocks/actions:make-action-url`][f521].
`ID`, `CLASS` and `TITLE` represent their `HTML` counterparts.
`RENDER-FN` is an optional function of one argument that is reponsible
for rendering the link's content (i.e. its label). The default rendering
function just calls `PRINC-TO-STRING` on the label and renders it
with escaping. Internally, render-fn should use reblocks:with-html macro
to write output into the right stream.

`WARNING`! This function generates <a href="...">...</a> element and any
bot, crawling the internet will hit this action with `GET` request.

<a id="x-28REBLOCKS-UI-2FFORM-3ARENDER-TEXTAREA-20FUNCTION-29"></a>

#### [function](8f06) `reblocks-ui/form:render-textarea` name &key (label (humanize-name name)) value id class disabledp

Renders a textarea.

* `NAME` - name of the html control. The name is attributized before
  being rendered.

* `VALUE` - a value on html control. Humanized name is default.

* `ID` - id of the html control. Default is nil.

* `CLASS` - a class used for styling. By default, "submit".

* `DISABLEDP` - button is disabled if true.

<a id="x-28REBLOCKS-UI-DOCS-2FPOPUP-3A-3A-40POPUP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Untitled

<a id="x-28REBLOCKS-UI-2FPOPUP-3APOPUP-WIDGET-20CLASS-29"></a>

### [class](6bb1) `reblocks-ui/popup:popup-widget` (ui-widget)

This widgets shows a popup window.

Inherit from this class and define a method for
[`render-popup-content`][8fc7] generic-function. Then you
will be able to instantiate your class instance
and call [`show-popup`][5ada] generic function.

<a id="x-28REBLOCKS-UI-2FPOPUP-3ASHOW-POPUP-20GENERIC-FUNCTION-29"></a>

### [generic-function](2931) `reblocks-ui/popup:show-popup` widget

Shows popup window.

<a id="x-28REBLOCKS-UI-2FPOPUP-3AHIDE-POPUP-20GENERIC-FUNCTION-29"></a>

### [generic-function](552c) `reblocks-ui/popup:hide-popup` widget

Hides popup window.

<a id="x-28REBLOCKS-UI-2FPOPUP-3ARENDER-POPUP-CONTENT-20GENERIC-FUNCTION-29"></a>

### [generic-function](bc83) `reblocks-ui/popup:render-popup-content` widget

Renders inner `HTML` for popup window.
You need to define a method for this generic function
and specialize it for your own class.

<a id="x-28REBLOCKS-UI-2FPOPUP-3AVISIBLE-P-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20REBLOCKS-UI-2FPOPUP-3APOPUP-WIDGET-29-29"></a>

### [accessor](6232) `reblocks-ui/popup:visible-p` (popup-widget) (= nil)


[233f]: https://40ants.com/reblocks-ui/
[923e]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FCORE-3AUI-WIDGET-20CLASS-29
[5578]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20CLASS-29
[8637]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20FUNCTION-29
[5162]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AFIELD-ERROR-20FUNCTION-29
[3765]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20CONDITION-29
[ea04]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20FUNCTION-29
[3868]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-PLACEHOLDER-20FUNCTION-29
[f976]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AWITH-HTML-FORM-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[8fc7]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FPOPUP-3ARENDER-POPUP-CONTENT-20GENERIC-FUNCTION-29
[5ada]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FPOPUP-3ASHOW-POPUP-20GENERIC-FUNCTION-29
[5fb1]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-DOCS-2FFORM-3A-3A-40CONFIRMATION-DEMO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[f521]: https://40ants.com/reblocks/actions/#x-28REBLOCKS-2FACTIONS-3AMAKE-ACTION-URL-20FUNCTION-29
[f21e]: https://40ants.com/reblocks/rendering/#x-28REBLOCKS-2FHTML-3AWITH-HTML-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[4fd9]: https://foundation.zurb.com/
[5ce9]: https://get.foundation/sites/docs/reveal.html
[1818]: https://github.com/40ants/reblocks-ui
[ce5f]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/core.lisp#L27
[6446]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/core.lisp#L31
[b894]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/core.lisp#L41
[ebf1]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L148
[28d7]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L159
[10aa]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L276
[dc72]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L306
[ac32]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L318
[f1cd]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L356
[fb67]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L411
[e214]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L44
[0723]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L470
[55be]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L506
[4d61]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L52
[8f06]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L522
[95c7]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L61
[de5c]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/form.lisp#L77
[6bb1]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/popup.lisp#L15
[6232]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/popup.lisp#L16
[2931]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/popup.lisp#L26
[552c]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/popup.lisp#L33
[bc83]: https://github.com/40ants/reblocks-ui/blob/3b3af2b4dd62fff9d62faef1e1c8170b964ca1b5/src/popup.lisp#L47
[2c00]: https://quickdocs.org/40ants-doc
[7f8b]: https://quickdocs.org/log4cl
[691c]: https://quickdocs.org/log4cl-extras
[7921]: https://quickdocs.org/parenscript
[2103]: https://quickdocs.org/quri
[184b]: https://quickdocs.org/reblocks
[c07c]: https://quickdocs.org/reblocks-parenscript

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
