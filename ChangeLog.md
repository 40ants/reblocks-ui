<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28REBLOCKS-UI-DOCS-2FCHANGELOG-3A-3A-7C0-2E13-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.13.0 (2022-02-14)

<a id="backward-incompabilities"></a>

### Backward Incompabilities

* Now action execution continues after the call to `reblocks-ui/form:form-error` ([`1`][4a1a] [`2`][5424])
  or `reblocks-ui/form:field-error` ([`1`][8a1e] [`2`][ecbf]). This way, all form errors could be shown at once.

* `CSS` classes for placeholders were changed. `reblocks-ui/form:error-placeholder` ([`1`][d70e] [`2`][1b14])
  now uses "form-error" instead of just "error" and [`reblocks-ui/form:form-error-placeholder`][d938]
  uses "alert callout" instead of "error".

* Argument `WIDGET` was removed from [`reblocks-ui/form:with-html-form`][8736] macro.

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


[1b14]: index.html#x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20CLASS-29
[d70e]: index.html#x-28REBLOCKS-UI-2FFORM-3AERROR-PLACEHOLDER-20FUNCTION-29
[ecbf]: index.html#x-28REBLOCKS-UI-2FFORM-3AFIELD-ERROR-20CONDITION-29
[8a1e]: index.html#x-28REBLOCKS-UI-2FFORM-3AFIELD-ERROR-20FUNCTION-29
[5424]: index.html#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20CONDITION-29
[4a1a]: index.html#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-20FUNCTION-29
[d938]: index.html#x-28REBLOCKS-UI-2FFORM-3AFORM-ERROR-PLACEHOLDER-20FUNCTION-29
[8736]: index.html#x-28REBLOCKS-UI-2FFORM-3AWITH-HTML-FORM-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
