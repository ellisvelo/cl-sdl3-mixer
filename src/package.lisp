
(defpackage #:sdl3-mixer
  (:use #:cl #:alexandria #:autowrap.minimal #:plus-c #:sdl3-ffi.functions
        #:cl-custom-hash-table)
  (:export
   ;; Conditions
   #:sdl-mixer-error
   ;; Mixer
   #:+prop-play-fade-in-milliseconds-number+
   #:+prop-play-fade-in-milliseconds-number+
   #:+prop-play-fade-in-start-gain-float+
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
   #:get-track-loops
   #:set-track-loops
   #:play-track
   #:set-track-stopped-callback
   #:get-track-mixer
   #:destroy-track
   #:track-playing-p
   #:pause-track
   #:resume-track
   #:paused-track-p
   #:stop-track
   #:convert-track-ms-to-frames
   #:convert-track-frames-to-ms)
  (:documentation "A Common Lisp wrapper for the SDL_Mixer 3.x C Library"))
