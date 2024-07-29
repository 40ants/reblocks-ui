<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E17-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.17.0 (2024-07-28)

<a id="fixed"></a>

### Fixed

* Code was fixed to work with latest changed to Reblocks, introduced in [PR 57][fff6].

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E16-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.16.0 (2022-12-12)

<a id="changed"></a>

### Changed

* Functions `reblocks-ui/form:error-placeholder` ([`1`][8637] [`2`][5578]) and [`reblocks-ui/form:form-error-placeholder`][3868] now
  can be called not only inside the [`reblocks-ui/form:with-html-form`][f976] macro body, but also in any
  function within.
* Added `CSS` attribute display none/block for popup.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E15-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.15.0 (2022-11-26)

<a id="new"></a>

### New

Functions [`reblocks-ui/form:get-field-errors-count`][6059] and [`reblocks-ui/form:get-field-errors`][bcb5]
were added to make it possible to check if there was any field errors during validation
and to interrupt further form processing.

An example was added to show how to process form errors and use placeholders. You'll find
it inside examples/form-errors.lisp file. Load reblocks-ui-examples system to load it.

<a id="fixed"></a>

### Fixed

Also, function a but inside [`reblocks-ui/form:form-error`][ea04] function was fixed and now it
will interrupt action and show an error inside a placeholder, created with
[`reblocks-ui/form:form-error-placeholder`][3868] function or open debugger otherwise.

Previosly this function call was ignored if there was a form error placeholder widget on a page.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E14-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.14.0 (2022-09-17)

<a id="new"></a>

### New

* New class [`reblocks-ui/popup:popup-widget`][50b7] was added.
  It can be used to show popup windows inside the main screen.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E13-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.13.0 (2022-02-14)

<a id="backward-incompabilities"></a>

### Backward Incompabilities

* Now action execution continues after the call to `reblocks-ui/form:form-error` ([`1`][ea04] [`2`][3765])
  or `reblocks-ui/form:field-error` ([`1`][5162] [`2`][62fd]). This way, all form errors could be shown at once.
* `CSS` classes for placeholders were changed. `reblocks-ui/form:error-placeholder` ([`1`][8637] [`2`][5578])
  now uses "form-error" instead of just "error" and [`reblocks-ui/form:form-error-placeholder`][3868]
  uses "alert callout" instead of "error".
* Argument `WIDGET` was removed from [`reblocks-ui/form:with-html-form`][f976] macro.

<a id="changes"></a>

### Changes

* Documentation was rewritten using `40ANTS-DOC`. Now it contains
  more information plus interactive demos.

<a id="fixes"></a>

### Fixes

* Fixed to work with latest Spinneret which now escapes double
  and single quotes.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E12-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.12.0 (2022-01-04)

Move to Reblocks.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E11-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.11.0 (2021-06-02)

Added a way to handle and show form and field errors.

Use `error-placeholder` function and `form-error-placeholder` function
inside the `with-html-form` macro. And signal errors using `field-error`
and `form-error` functions.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.0 (2021-01-07)

* Macro `with-html-form` now can accept parameters `requires-confirmation-p` and `confirm-question`.
  If `requires-confirmation-p` is given, form shows a popup asking if user really wants to execute an action.
  Argument `confirm-question` can accept a string or a forms for spinneret. They will be used to render
  the content of the popup.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E9-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.0 (2020-01-31)

* `widget` was renamed to `ui-widget` and a deprecation warning was added.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E8-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.8.0 (2018-05-04)

* Do not use `fieldset` in `with-html-form`. It usually draws a box
  around the form elements and this isn't always wanted.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E7-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.1 (2018-02-10)

* Function `render-button` now supports keyword argument `:onclick`.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E7-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.0 (2018-02-06)

* Added initialization of Foundation's javascript plugins.
  This introduces dependency from `weblocks-parenscript`.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E6-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.0 (2018-02-01)

System was moved to package inferred class and fixed to work with
weblocks >= 0.25.0.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E5-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.0 (2018-01-11)

* Now  `*foundation-dependencies*` is exported from
  `weblocks.ui.core`.
  This is a list of `CSS` an `JS` dependencies of Zurb Foundation,
  which you can with to add to your application wide dependencies
  list to render pages without widgets, such as error pages.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E4-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.0 (2018-01-07)

* Fixed to work with weblocks >= 0.22.0

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.3.0 (2017-11-23)

* Package `weblocks.ui.form` was fixed to use `spinneret` instead of
  `cl-who`, because `reblocks` version of `Weblocks` moved to it.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0 (2017-11-11)

* Now `UI` widgets are depend on new `weblocks.widget:widget` class.

This is breaking rendering for widgets. Replace old `weblocks:render-body`
  methods with `weblocks.widget:render`.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E1-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.1 (2017-09-21)

* Code was fixed to work with weblocks 0.14.0 where `*action-string*`
  was moved to weblocks.variables.
* Added `weblocks.ui.form:render-link` function, from old `weblocks/utils/html.lisp`.

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0 (2017-09-01)

* Initial version.


[5578]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20CLASS-29
[8637]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20FUNCTION-29
[62fd]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AFIELD-ERROR-20CONDITION-29
[5162]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AFIELD-ERROR-20FUNCTION-29
[3765]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20CONDITION-29
[ea04]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20FUNCTION-29
[3868]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-PLACEHOLDER-20FUNCTION-29
[bcb5]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AGET-FIELD-ERRORS-20FUNCTION-29
[6059]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AGET-FIELD-ERRORS-COUNT-20FUNCTION-29
[f976]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FFORM-3AWITH-HTML-FORM-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[50b7]: https://40ants.com/reblocks-ui/#x-28REBLOCKS-UI-2FPOPUP-3APOPUP-WIDGET-20CLASS-29
[fff6]: https://github.com/40ants/reblocks/pull/57

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
