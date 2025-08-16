(in-package #:sdl3-mixer)

(cffi:define-foreign-library libsdl3-mixer
  (:darwin (:or (:framework "SDL3_mixer") (:default "libSDL3_mixer")))
  (:unix (:or "libSDL3_mixer.so" "libSDL3_mixer.so.0" "libSDL3_mixer"))
  (:windows "SDL3_mixer.dll")
  (t (:default "libSDL3_mixer")))

(cffi:use-foreign-library libsdl3-mixer)
