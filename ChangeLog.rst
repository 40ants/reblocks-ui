===========
 ChangeLog
===========

0.7.0 (2018-02-06)
==================

* Added initialization of Foundation's javascript plugins.
  This introduces dependency from ``weblocks-parenscript``.

0.6.0 (2018-02-01)
==================

System was moved to package inferred class and fixed to work with
weblocks >= 0.25.0.

0.5.0 (2018-01-11)
==================

* Now  ``*foundation-dependencies*`` is exported from
  ``weblocks.ui.core``.
  This is a list of CSS an JS dependencies of Zurb Foundation,
  which you can with to add to your application wide dependencies
  list to render pages without widgets, such as error pages.

0.4.0 (2018-01-07)
==================

* Fixed to work with weblocks >= 0.22.0

0.3.0
=====

* Package ``weblocks.ui.form`` was fixed to use ``spinneret`` instead of
  ``cl-who``, because ``reblocks`` version of ``Weblocks`` moved to it.

0.2.0
=====

* Now UI widgets are depend on new ``weblocks.widget:widget`` class.

  This is breaking rendering for widgets. Replace old ``weblocks:render-body``
  methods with ``weblocks.widget:render``.

0.1.1
=====

* Code was fixed to work with weblocks 0.14.0 where ``*action-string*``
  was moved to weblocks.variables.
* Added ``weblocks.ui.form:render-link`` function, from old ``weblocks/utils/html.lisp``.

0.1.0
=====

* Number features here.
* Like that.
* Add new versions to the top.
* Specify dates as ``2017-04-19``.
* Read `KeepAChangelog.com <http://keepachangelog.com/>`_ for futher
  explanations.
