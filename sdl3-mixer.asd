(asdf:defsystem #:sdl3-mixer
  :description "Bindings for SDL3_mixer using autowrap"
  :author ("Shawn Ellis <shawn.ellis17@gmail.com>")
  :license "MIT"
  :version "1.0"
  :depends-on (#:alexandria
               #:cl-autowrap
               #:sdl3
               #:trivial-garbage)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "library")
   (:file "autowrap")
   (:file "conditions")
   (:file "general")
   (:module autowrap-spec
    :pathname "spec"
    :components
    ((:static-file "SDL3_mixer.h")))))
