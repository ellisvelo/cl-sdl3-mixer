(in-package #:sdl3-mixer-examples)

(defun simple ()
  (with-init (:everything)
    (sdl3-mixer:init)
    (let ((mixer (sdl3-mixer:create-mixer-device))
          (sample-ogg (asdf:system-relative-pathname 'sdl3-mixer-examples "examples/sample.ogg")))
      (with-window (my-window :title "Mixer Example"
                              :w 100
                              :h 100)
        (with-renderer (my-renderer my-window)
          (flet ((clear-renderer (renderer)
                   (set-render-draw-color renderer 0 0 0 255)
                   (render-clear renderer)))

            ;; Load the audio and create the track
            (let ((sound-audio (sdl3-mixer:load-audio mixer sample-ogg))
                  (sound-track (sdl3-mixer:create-track mixer))
                  (current-volume 1.0)
                  (volume-increment 0.3))
              ;; Set the the audio for the track
              (sdl3-mixer:set-track-audio sound-track sound-audio)

              (with-event-loop (:method :poll)
                (:key-down (:scancode scancode)
                           (cond ((scancode= scancode :space)
                                  (sdl3-mixer:play-track sound-track))
                                 ((scancode= scancode :up)
                                  (when (< (+ current-volume volume-increment) 1.8)
                                    (incf current-volume volume-increment)
                                    (format t "Current Volume: ~a~%" current-volume)
                                    (sdl3-mixer:set-track-gain sound-track current-volume)))
                                 ((scancode= scancode :down)
                                  (when (> (- current-volume volume-increment) 0.0)
                                    (decf current-volume volume-increment)
                                    (format t "Current Volume: ~a~%" current-volume)
                                    (sdl3-mixer:set-track-gain sound-track current-volume)))
		                 ((scancode= scancode :escape)
			          (sdl3:push-event :quit))))
                (:idle ()
                       (clear-renderer my-renderer)
                       (render-present my-renderer))
                (:quit ()
                       ;; Not really needed because quit will destory any
                       ;; created objects
                       (sdl3-mixer:destroy-track sound-track)
                       (sdl3-mixer:destroy-audio sound-audio)
                       (sdl3-mixer:destroy-mixer mixer)

                       (sdl3-mixer:quit)
                       t)))))))))
