(asdf:defsystem #:dotwar-client
  :description "A client for Dotwar"
  :author "Joey Herguth <oitzujoey@gmail.com>"
  :license "MIT License"
  :version "0.1.0"
  :serial t
  :depends-on (#:dexador
               #:com.inuoe.jzon
               #:sketch
               #:alexandria)
  :components ((:file "package")
               (:file "vector" :depends-on ("package"))
               (:file "reference" :depends-on ("package"))
               (:file "matrix" :depends-on ("package"))
               (:file "dotwar-autopilot" :depends-on ("vector" "reference" "matrix"))
               (:file "dotwar-client" :depends-on ("package" "vector"))))
