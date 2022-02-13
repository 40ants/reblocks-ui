<a id="x-28REBLOCKS-UI-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Reblocks-UI

<a id="reblocks-ui-asdf-system-details"></a>

## REBLOCKS-UI ASDF System Details

* Version: 0.12.0

* Description: A set of `UI` widgets for Reblocks web framework!

* Licence: `BSD`

* Author: Alexander Artemenko

<a id="x-28REBLOCKS-UI-DOCS-2FINDEX-3A-3A-40SIMPLE-DEMO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Simple Demo

This demo shows how to process form data in a callback
and to update the widget accordingly:


<div class=demo>
 <iframe
         sandbox="allow-forms allow-same-origin allow-scripts"
         id=example-34
         src="http://localhost:40001/examples/reblocks-ui-docs/index/simple-example?iframe-id=example-34"
         style="width: 100%; height: 10em; border: 0"></iframe>
</div>
<script>
window.addEventListener('message', function(e) {
  let message = e.data;
  let iframe_id = message.iframe_id;
  let iframe = document.querySelector('#' + iframe_id);
  if (iframe) {
    iframe.style.height = message.height + 'px';
    iframe.style.width = message.width + 'px';
  } else {
    console.log('Unable to find iframe with id', iframe_id);
  }
} , false);
</script>

<a id="x-28REBLOCKS-UI-DOCS-2FINDEX-3A-3A-40CONFIRMATION-DEMO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Submit Confirmation

You might also want to warn user about some destructive actions.

To do this, provide `REQUIRES-CONFIRMATION-P` argument to the
[`with-html-form`][8736] macro. Optionally you might provide `CONFIRMATION-QUESTION`
argument with a text of the question. Pay attention how does question
changes when you are clicking a button in this demo:


<div class=demo>
 <iframe
         sandbox="allow-forms allow-same-origin allow-scripts"
         id=example-35
         src="http://localhost:40001/examples/reblocks-ui-docs/index/confirmation-example?iframe-id=example-35"
         style="width: 100%; height: 10em; border: 0"></iframe>
</div>
<script>
window.addEventListener('message', function(e) {
  let message = e.data;
  let iframe_id = message.iframe_id;
  let iframe = document.querySelector('#' + iframe_id);
  if (iframe) {
    iframe.style.height = message.height + 'px';
    iframe.style.width = message.width + 'px';
  } else {
    console.log('Unable to find iframe with id', iframe_id);
  }
} , false);
</script>

<a id="x-28REBLOCKS-UI-DOCS-2FINDEX-3A-3A-40ERRORS-DEMO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Showing Errors

Form processing usually includes error checking.
You can use [`error-placeholder`][d70e] and [`form-error-placeholder`][d938] inside
the body of [`with-html-form`][8736] macro to mark places where errors should be show.

There can be only one form error placeholder and it will show errors which are not
related to some particular field. Field error placeholders are named and usually
should be placed above or below a field.

After you've used placeholder inside the form, use [`field-error`][8a1e] function inside an
action's code to display the error related to the field or just signal any `ERROR`
to show a form-wide error.

An example below, shows both types of error. Enter "bob" as a login, to check
how a form-wide error will look like:


<div class=demo>
 <iframe
         sandbox="allow-forms allow-same-origin allow-scripts"
         id=example-36
         src="http://localhost:40001/examples/reblocks-ui-docs/index/errors-example?iframe-id=example-36"
         style="width: 100%; height: 10em; border: 0"></iframe>
</div>
<script>
window.addEventListener('message', function(e) {
  let message = e.data;
  let iframe_id = message.iframe_id;
  let iframe = document.querySelector('#' + iframe_id);
  if (iframe) {
    iframe.style.height = message.height + 'px';
    iframe.style.width = message.width + 'px';
  } else {
    console.log('Unable to find iframe with id', iframe_id);
  }
} , false);
</script>

<a id="x-28REBLOCKS-UI-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28REBLOCKS-UI-2FFORM-3AWITH-HTML-FORM-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

### [macro](875d) `reblocks-ui/form:with-html-form` (METHOD-TYPE ACTION &KEY ID CLASS ENCTYPE (USE-AJAX-P T) EXTRA-SUBMIT-CODE REQUIRES-CONFIRMATION-P (CONFIRM-QUESTION "Are you sure?") (SUBMIT-FN "initiateFormAction(\"~A\", $(this), \"~A\")")) &BODY BODY

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
  to show a popup. See [`Submit Confirmation`][4596] section for
  an example of code.

<a id="x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20CLASS-29"></a>

### [class](56ff) `reblocks-ui/form:error-placeholder` (widget)

<a id="x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20FUNCTION-29"></a>

### [function](c9c0) `reblocks-ui/form:error-placeholder` name &key (widget-class 'error-placeholder)

This function creates and renders a widget to show an error message related to some form field.

It should be called inside [`with-html-form`][8736] macro.

`NAME` argument should be a string denoting a form field. Later, you can call [`field-error`][8a1e] function
to signal an error from the action function. You will need to pass the `NAME` as the first argument
to the [`field-error`][8a1e] function.

<a id="x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-PLACEHOLDER-20FUNCTION-29"></a>

### [function](3658) `reblocks-ui/form:form-error-placeholder` &key (widget-class 'form-error-placeholder)

This function creates and renders a widget to show an error for the whole form.

It should be called inside [`with-html-form`][8736] macro.

Later, you can call [`form-error`][4a1a] function to signal an error from the action function.

<a id="x-28REBLOCKS-UI-2FFORM-3AFIELD-ERROR-20CONDITION-29"></a>

### [condition](8d72) `reblocks-ui/form:field-error` (form-error)

<a id="x-28REBLOCKS-UI-2FFORM-3AFIELD-ERROR-20FUNCTION-29"></a>

### [function](3336) `reblocks-ui/form:field-error` name message

Signals an error which will be shown for the whole form.lisp

You need to use [`error-placeholder`][d70e] function inside the [`with-html-form`][8736] macro
to set a place where an error message should be shown. Otherwise, the error
will be logged and ignored.

If there is no a `error-placeholder` ([`1`][d70e] [`2`][1b14]) call with corresponding `NAME` argument,
then error message can be shown for the whole form in a place where
[`form-error-placeholder`][d938] function was called.

<a id="x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20CONDITION-29"></a>

### [condition](dd91) `reblocks-ui/form:form-error` (error)

<a id="x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20FUNCTION-29"></a>

### [function](87c5) `reblocks-ui/form:form-error` message

Signals an error which will be shown for the whole form.lisp

You need to use [`form-error-placeholder`][d938] function inside the [`with-html-form`][8736] macro
to set a place where an error message should be shown. Otherwise, the error
will be logged and ignored.

<a id="x-28REBLOCKS-UI-2FFORM-3ARENDER-BUTTON-20FUNCTION-29"></a>

### [function](5e67) `reblocks-ui/form:render-button` NAME &KEY (VALUE (HUMANIZE-NAME NAME)) ID (CLASS "button") (ONCLICK "disableIrrelevantButtons(this);") DISABLEDP

Renders a button in a form.

* `NAME` - name of the html control. The name is attributized before
  being rendered.

* `VALUE` - a value on html control. Humanized name is default.

* `ID` - id of the html control. Default is nil.

* `CLASS` - a class used for styling. By default, "submit".

* `DISABLEDP` - button is disabled if true.

<a id="x-28REBLOCKS-UI-2FFORM-3ARENDER-FORM-AND-BUTTON-20FUNCTION-29"></a>

### [function](249d) `reblocks-ui/form:render-form-and-button` NAME ACTION &KEY (VALUE (HUMANIZE-NAME NAME)) (METHOD :GET) BUTTON-ID (BUTTON-CLASS "button") (USE-AJAX-P T) FORM-ID FORM-CLASS

Renders a button within a form. This function can be used a short
cut to quickly render a sumbit button.

<a id="x-28REBLOCKS-UI-2FFORM-3ARENDER-LINK-20FUNCTION-29"></a>

### [function](c125) `reblocks-ui/form:render-link` action label &key (ajaxp t) id class title render-fn

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

<a id="x-28REBLOCKS-UI-2FFORM-3ARENDER-TEXTAREA-20FUNCTION-29"></a>

### [function](f82e) `reblocks-ui/form:render-textarea` name &key (label (humanize-name name)) value id class disabledp

Renders a textarea.

* `NAME` - name of the html control. The name is attributized before
  being rendered.

* `VALUE` - a value on html control. Humanized name is default.

* `ID` - id of the html control. Default is nil.

* `CLASS` - a class used for styling. By default, "submit".

* `DISABLEDP` - button is disabled if true.


[56ff]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L246
[c9c0]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L276
[3658]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L288
[875d]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L299
[5e67]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L367
[dd91]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L37
[c125]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L426
[8d72]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L45
[249d]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L459
[f82e]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L475
[3336]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L54
[87c5]: NIL/blob/7ff69d85acbc61bfaf72f95d1b056d6fce09c019/src/form.lisp#L70
[f521]: https://40ants.com/reblocks/actions/#x-28REBLOCKS-2FACTIONS-3AMAKE-ACTION-URL-20FUNCTION-29
[f21e]: https://40ants.com/reblocks/rendering/#x-28REBLOCKS-2FHTML-3AWITH-HTML-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[5ce9]: https://get.foundation/sites/docs/reveal.html
[1b14]: index.html#x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20CLASS-29
[d70e]: index.html#x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20FUNCTION-29
[8a1e]: index.html#x-28REBLOCKS-UI-2FFORM-3AFIELD-ERROR-20FUNCTION-29
[4a1a]: index.html#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20FUNCTION-29
[d938]: index.html#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-PLACEHOLDER-20FUNCTION-29
[8736]: index.html#x-28REBLOCKS-UI-2FFORM-3AWITH-HTML-FORM-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[4596]: index.html#x-28REBLOCKS-UI-DOCS-2FINDEX-3A-3A-40CONFIRMATION-DEMO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
