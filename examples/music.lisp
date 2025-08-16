(in-package #:sdl3-mixer-examples)

(require 'sdl3-mixer)

(defparameter *current-volume* 128)

(defun music ()
  (with-init (:everything)
    (sdl3-mixer:init :ogg)
    (sdl3-mixer:open-audio 0 :frequency 22050 :format :s16 :channels 1)
    (with-window (my-window :title "Mixer Example"
                            :w 100
                            :h 100)
      (with-renderer (my-renderer my-window)
        (flet ((clear-renderer (renderer)
                 (progn (set-render-draw-color renderer 0 0 0 255)
                        (render-clear renderer))))
          (let ((music (sdl3-mixer:load-music
                        (asdf:system-relative-pathname
                         'sdl3-mixer-examples
                         "examples/example_song.ogg"))))
            (with-event-loop (:method :poll)
              (:key-down (:scancode scancode)
			 (cond ((scancode= scancode :space)
				(format t "Playing Song~%")
				(sdl3-mixer:play-music music 1))
			       ((scancode= scancode :up)
				(when (< (+ *current-volume* 20) 128)
				  (incf *current-volume* 20)
				  (format t "Current Volume: ~a~%" *current-volume*)
				  (sdl3-mixer:volume-music *current-volume*)))
			       ((scancode= scancode :down)
				(when (> (- *current-volume* 20) 0)
				  (decf *current-volume* 20)
				  (format t "Current Volume: ~a~%" *current-volume*)
				  (sdl3-mixer:volume-music *current-volume*)))
			       ((scancode= scancode :escape)
				(sdl3:push-event :quit))))
              (:idle ()
                     (clear-renderer my-renderer)
                     (render-present my-renderer))
              (:quit ()
                     ;; Ensure all channels have been halted so to safely free
                     ;; any sound chunks we created
                     (sdl3-mixer:halt-music)
                     (sdl3-mixer:close-audio)
                     (sdl3-mixer:free-music music)
                     (sdl3-mixer:quit)
                     t))))))))
