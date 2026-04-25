(in-package #:sdl3-mixer)

(defconstant +prop-play-fade-in-frame-number+ sdl3-ffi:+mix-prop-play-fade-in-frames-number+
  "The number of sample frames over which to fade in the newly-started track.")

(defconstant +prop-play-fade-in-milliseconds-number+ sdl3-ffi:+mix-prop-play-fade-in-milliseconds-number+
  "The number of milliseconds over which to fade in the newly-started track.")

(defconstant +prop-play-fade-in-start-gain-float+ sdl3-ffi:+mix-prop-play-fade-in-start-gain-float+
  "If fading in, start fading from this volume level. 0.0f is silence and 1.0f
 is full volume, every in between is a linear change in gain.")

(defconstant +prop-play-append-silence-frames-number+ sdl3-ffi:+mix-prop-play-append-silence-frames-number+
  "At the end of mixing this track, after all loops are complete, append this
 many sample frames of silence as if it were part of the audio file")

(defconstant +prop-play-append-silence-milliseconds-number+ sdl3-ffi:+mix-prop-play-append-silence-milliseconds-number+
  "At the end of mixing this track, after all loops are complete, append this
 many sample milliseconds of silence as if it were part of the audio file")

(defconstant +prop-play-halt-when-exhausted-boolean+ sdl3-ffi:+mix-prop-play-halt-when-exhausted-boolean+
  "When true and the input is completely consumed for the track, then the mixer
will mark the track as stopped and call any track stopped callbacks.")

(defun mixer-equal-p (x y)
  "Return T when the mixers are equivalent."
  (cffi:pointer-eq (autowrap:ptr x) (autowrap:ptr y)))

(defun mixer-hash (x)
  "Use the mixer's pointer for hashing."
  (cffi:pointer-address (autowrap:ptr x)))

(define-custom-hash-table-constructor make-mixer-hash-table
  :test mixer-equal-p :hash-function mixer-hash)

(defvar *mixer-tracks* (make-mixer-hash-table)
  "Maps the mixer with the tracks created for it.")

(defvar *mixer-audio* nil
  "List of audio created.")

(defun %add-mixer-track (mixer track)
  "Adds a track for tracking."
  (with-custom-hash-table
    (let* ((value (gethash mixer *mixer-tracks*))
           (new-value (push track value)))
      (setf (gethash mixer *mixer-tracks*) new-value))))

(defun %remove-mixer-track (mixer track)
  "Removes a track when destroyed."
  (with-custom-hash-table
    (let ((tracks (gethash mixer *mixer-tracks*)))
      (when tracks
        (setf (gethash mixer *mixer-tracks*) (remove track tracks))))))

(defmacro create-sdl-destroy-function (destroy-function sdl-object)
  "A macro to destroy and invalidate the SDL-OBJECT."
  `(progn (tg:cancel-finalization ,sdl-object)
          (,destroy-function ,sdl-object)
          (autowrap:invalidate ,sdl-object)))

(defun version ()
  "Get the version of SDL_mixer that is linked against your program and return
the major, minor, and micro version."
  (let* ((version (mix-version))
         (major (floor version 1000000))
         (minor (floor (mod (/ version 1000) 1000)))
         (micro (mod version 1000)))
    (values major minor micro)))

(defun sdl-mixer-true-p (integer-bool)
  "This function converts an integer into CL's boolean type system."
  (= 1 integer-bool))

(defun init ()
  "Initialize the SDL mixer."
  (mix-init))

(defun create-mixer-device (&key (audio-device-id sdl3:+audio-device-default-playback+) audio-spec)
  "Create a mixer that plays sound directly to an audio device."
  (check-null (mix-create-mixer-device audio-device-id audio-spec)))

(defun get-mixer-format (mixer)
  "Get the audio format a mixer is generating."
  (c-with ((audio-spec sdl3-ffi:sdl-audio-spec))
    (check-true (mix-get-mixer-format mixer (audio-spec &)))
    audio-spec))

(defun get-mixer-gain (mixer)
  "Get a mixer's master gain control."
  (mix-get-mixer-gain mixer))

(defun set-mixer-gain (mixer gain)
  "Set a mixer's master gain control."
  (check-type gain float)
  (check-true (mix-set-mixer-gain mixer gain)))

(defun destroy-mixer (mixer)
  "Closes the mixer"
  (when (autowrap:valid-p mixer)
    (with-custom-hash-table
      (remhash mixer *mixer-tracks*))
    (create-sdl-destroy-function mix-destroy-mixer mixer)))

(defun load-audio (mixer file-path &optional (predecode 0))
  "Load audio for playback from a file using the MIXER, FILE-PATH, and
 optionally whether to return fully uncompressed data."
  (let ((audio (check-null (mix-load-audio mixer (namestring file-path) predecode))))
    (setf *mixer-audio* (push audio *mixer-audio*))
    audio))

(defun destroy-audio (audio)
  "Destroy the specified audio."
  (when (autowrap:valid-p audio)
    (setf *mixer-audio* (remove audio *mixer-audio*))
    (create-sdl-destroy-function mix-destroy-audio audio)))

(defun create-track (mixer)
  "Create a new track on a mixer."
  (let ((track (check-null (mix-create-track mixer))))
    (%add-mixer-track mixer track)
    track))

(defun get-track-audio (track)
  "Return Query the audio assigned to a track."
  (mix-get-track-audio track))

(defun set-track-audio (track audio)
  "Set a track's input to the audio."
  (check-true (mix-set-track-audio track audio)))

(defun get-track-gain (track)
  "Get a track's gain control."
  (check-true (mix-get-track-gain track)))

(defun set-track-gain (track gain)
  "Set a track's gain control."
  (check-type gain float)
  (check-true (mix-set-track-gain track gain)))

(defun get-track-loops (track)
  "Query how many loops remain for a given track."
  (mix-get-track-loops track))

(defun set-track-loops (track num-loops)
  "Change the number of times a currently-playing track will loop. This
 replaces any previously-set remaining loops. A value of 1 will loop to the
start of playback one time. Zero will not loop at all. A value of -1 requests
infinite loops. If the input is not seekable and NUM-LOOPS isn't zero, this
function will report success but the track will stop at the point it should
loop."
  (check-true (mix-set-track-loops track num-loops)))

(defun play-track (track &optional options)
  "Start (or restart) mixing a track for playback."
  (check-true (mix-play-track track (or options 0))))

(defun set-track-stopped-callback (track cffi-callback-track-stopped-fn &optional user-data)
  "Set a callback that fires when a MIX_Track is stopped."
  (check-true (mix-set-track-stopped-callback track cffi-callback-track-stopped-fn user-data)))

(defun get-track-mixer (track)
  "Get the MIX_Mixer that owns a MIX_Track."
  (check-null (mix-get-track-mixer track)))

(defun destroy-track (track)
  "Destroy the specified track."
  (when (autowrap:valid-p track)
    (%remove-mixer-track (get-track-mixer track) track)
    (create-sdl-destroy-function mix-destroy-track track)))

(defun track-playing-p (track)
  "Query if a track is currently playing."
  (sdl-mixer-true-p (mix-track-playing track)))

(defun pause-track (track)
  "Pause a currently-playing track."
  (mix-pause-track track))

(defun resume-track (track)
  "Resume a currently-paused track."
  (mix-resume-track track))

(defun paused-track-p (track)
  "Returns T when the track is paused."
  (sdl-mixer-true-p (mix-track-paused track)))

(defun stop-track (track &optional (fade-out-frames 0))
  "Halt a currently-playing track, possibly fading out over time."
  (check-true (mix-stop-track track fade-out-frames)))

(defun convert-track-ms-to-frames (track ms)
  "Convert milliseconds to sample frames for a track's current format."
  (check-rc (mix-track-ms-to-frames track ms)))

(defun convert-track-frames-to-ms (track frames)
  "Convert sample frames for a track's current format to milliseconds."
  (check-rc (mix-track-frames-to-ms track frames)))

(defun quit ()
  "Cleans up SDL Mixer by destroying mixers, tracks, and audio."
  ;; destroys audio
  (mapc #'(lambda (audio) (destroy-audio audio)) *mixer-audio*)

  ;; destroy tracks and mixers
  (with-custom-hash-table
    (maphash #'(lambda (mixer tracks)
                 (mapc #'(lambda (track) (destroy-track track)) tracks)
                 (destroy-mixer mixer)) *mixer-tracks*))
  (mix-quit))
