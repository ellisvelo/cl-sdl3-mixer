
(defpackage #:sdl3-mixer
  (:use #:cl #:alexandria #:autowrap.minimal #:plus-c #:sdl3-ffi.functions)
  (:export
   ;; Conditions
   #:sdl-mixer-error
   ;; General
   #:version
   #:init
   #:quit
   #:create-mixer-device
   #:get-mixer-format
   #:get-mixer-gain
   #:set-mixer-gain
   #:destroy-mixer
   #:load-audio
   #:destroy-audio
   #:create-track
   #:get-track-audio
   #:set-track-audio
   #:get-track-gain
   #:set-track-gain
   #:play-track
   #:set-track-stopped-callback
   #:destroy-track
   #:playing-p
   #:pause-track
   #:resume-track
   #:paused-track-p
   #:stop-track)
  (:documentation "A Common Lisp wrapper for the SDL_Mixer 3.x C Library"))
