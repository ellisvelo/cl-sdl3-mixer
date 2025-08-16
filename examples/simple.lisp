(in-package #:sdl3-mixer-examples)

;;Not using shadowing-imports for the mixer init and quit functions for
;;illustrative purposes
(require 'sdl3-mixer)

;; Every channel's volume starts out at maximum according to the manual
(defparameter *current-volume* 128)

(defun simple ()
  (with-init (:everything)
    (sdl3-mixer:init :ogg)
    ;; These values are taken from the manual
    ;; https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC11
      #+nil(sdl3-mixer:open-audio 22050 :s16sys 1 1024)
    (sdl3-mixer:open-audio 0 :frequency 22050 :format :s16 :channels 1)
    (with-window (my-window :title "Mixer Example"
                            :w 100
                            :h 100)
      (with-renderer (my-renderer my-window)
        (flet ((clear-renderer (renderer)
                 (progn (set-render-draw-color renderer 0 0 0 255)
                        (render-clear renderer))))
          (let ((sound-effect (sdl3-mixer:load-wav
                               (asdf:system-relative-pathname
                                'sdl3-mixer-examples "examples/sample.ogg"))))
            (with-event-loop (:method :poll)
              (:key-down (:scancode scancode)
                 ;; Channels are 0 indexed
                 (cond ((scancode= scancode :space)
                        (sdl3-mixer:play-channel 0 sound-effect 0))
                       ((scancode= scancode :up)
                        (when (< (+ *current-volume* 20) 128)
                          (incf *current-volume* 20)
                          (format t "Current Volume: ~a~%" *current-volume*)
                          (sdl3-mixer:volume 0 *current-volume*)))
                       ((scancode= scancode :down)
                        (when (> (- *current-volume* 20) 0)
                          (decf *current-volume* 20)
                          (format t "Current Volume: ~a~%" *current-volume*)
                          (sdl3-mixer:volume 0 *current-volume*)))
		       ((scancode= scancode :escape)
			(sdl3:push-event :quit))))
              (:idle ()
                     (clear-renderer my-renderer)
                     (render-present my-renderer))
              (:quit ()
                     ;; Ensure all channels have been halted so to safely free
                     ;; any sound chunks we created
                     (sdl3-mixer:halt-channel -1)
                     (sdl3-mixer:close-audio)
                     (sdl3-mixer:free-chunk sound-effect)
                     (sdl3-mixer:quit)
                     t))))))))
