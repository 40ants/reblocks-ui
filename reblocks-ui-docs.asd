(defsystem "reblocks-ui-docs"
  :author "Alexander Artemenko"
  :license "BSD"
  :class :package-inferred-system
  :pathname "docs"
  :serial t
  :depends-on ("reblocks-ui-docs/index"
               "reblocks-ui-docs/changelog")
  :description "Documentation for Reblocks UI widgets."
  :homepage "https://40ants.com/reblocks-ui/"
  :source-control (:git "https://github.com/40ants/reblocks-ui"))


(asdf:register-system-packages "reblocks"
                               '("REBLOCKS/DOC/EXAMPLE"))


