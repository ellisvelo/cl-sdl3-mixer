(in-package #:cl-user)

(defpackage #:sdl3-mixer
  (:use #:cl #:alexandria #:autowrap.minimal #:plus-c #:sdl3-ffi.functions)
  (:export
   ;; Conditions
   #:sdl-mixer-error
   ;; General
   #:version
   #:init
   #:quit
   #:open-audio
   #:close-audio
   #:query-spec
   #:load-wav
   #:allocate-channels
   #:volume
   #:play-channel
   #:set-channel-finished-callback
   #:halt-channel
   #:playing
   #:free-chunk
   #:load-music
   #:free-music
   #:play-music
   #:halt-music
   #:fade-in-music
   #:fade-out-music
   #:pause-channel
   #:resume-channel
   #:paused-channel-p
   #:pause-music
   #:resume-music
   #:paused-music-p
   #:volume-music))
