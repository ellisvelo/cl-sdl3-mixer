(asdf:defsystem #:sdl3-mixer-examples
  :description "A few examples to demonstrate the usage of sdl3-mixer"
  :author "Shawn Ellis" ;; based upon Bryan Baraoidan's sdl2-mixer examples
  :license "MIT"
  :depends-on (#:sdl3
               #:sdl3-mixer)
  :pathname "examples"
  :serial t
  :components ((:file "package")
               (:file "simple")
               (:file "music")))
