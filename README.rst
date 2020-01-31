=================
 weblocks-ui
=================

.. insert-your badges like that:

.. image:: https://travis-ci.org/40ants/cl-hamcrest.svg?branch=master
    :target: https://travis-ci.org/40ants/cl-hamcrest

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from

Weblocks-ui brings `Foundation`_ styling to your Weblocks application.

Usage
=====

Weblocks-ui defines a ``ui-widget``. Your widgets than inherit from it
get automatic styling:


.. code-block:: common-lisp

   (defwiget my-widget (weblocsk-ui:ui-widget)
     ((…))

The dependencies (Foundation and Jquery) are automatically fetched
from a CDN (Cloudflare).

``weblocks-ui/form:with-html-form`` is used to quickly set up correct
Weblocks forms (with Ajax calls, actions, etc).

This example is an except of the `quickstart tutorial`_:

.. code-block:: common-lisp

    (defmethod render ((task-list task-list))
      (with-html
        (:h1 "Tasks")
        (loop for task in (tasks task-list) do
          (render task))
        (with-html-form (:POST (lambda (&key title &allow-other-keys)
                                       (add-task task-list title)))
          (:input :type "text"
                  :name "title"
                  :placeholder "Task's title")
          (:input :type "submit"
                  :value "Add"))))



.. Everything after this comment will be omitted from HTML docs.
.. include-to

Building Documentation
======================


How to build documentation
--------------------------

To build documentation, you need Sphinx. It is a
documentaion building tool written in Python.

To install it, you need a virtualenv. Read
this instructions
`how to install it
<https://virtualenv.pypa.io/en/stable/installation/#installation>`_.

Also, you'll need a `cl-launch <http://www.cliki.net/CL-Launch>`_.
It is used by documentation tool to run a script which extracts
documentation strings from lisp systems.

Run these commands to build documentation::

  virtualenv env
  source env/bin/activate
  pip install -r docs/requirements.txt
  invoke build_docs

These commands will create a virtual environment and
install some python libraries there. Command ``invoke build_docs``
will build documentation and upload it to the GitHub, by replacing
the content of the ``gh-pages`` branch.


Authors
=======

* Alexander Artemenko

Copyright
=========

Copyright (c) 2017 Alexander Artemenko

License
=======

Licensed under the BSD License.

.. _Foundation: https://foundation.zurb.com/
.. _quickstart tutorial: http://40ants.com/weblocks/quickstart.html
