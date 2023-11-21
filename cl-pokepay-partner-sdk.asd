(defsystem "cl-pokepay-partner-sdk"
  :depends-on ("alexandria"
               "dexador"
               "com.inuoe.jzon"
               "ironclad"
               "jose"
               "uuid"
               "local-time")
  :serial t
  :components ((:file "crypto")
               (:file "main")))
