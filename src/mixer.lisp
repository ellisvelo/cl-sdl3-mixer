(in-package #:sdl3-mixer)

(defconstant +prop-play-fade-in-milliseconds-number+ sdl3-ffi:+mix-prop-play-fade-in-milliseconds-number+)
(defconstant +prop-play-fade-in-milliseconds-number+ sdl3-ffi:+mix-prop-play-fade-in-milliseconds-number+)
(defconstant +prop-play-fade-in-start-gain-float+ sdl3-ffi:+mix-prop-play-fade-in-start-gain-float+)

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

(defun quit ()
  "Cleans up SDL Mixer."
  (mix-quit))

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
  (declare (float gain))
  (check-true (mix-set-mixer-gain mixer gain)))

(defun destroy-mixer (mixer)
  "Closes the mixer"
  (mix-destroy-mixer mixer))

(defun load-audio (mixer file-path &optional (predecode 0))
  "Load audio for playback from a file using the MIXER, FILE-PATH, and
 optionally whether to return fully uncompressed data."
  (check-null (mix-load-audio mixer (namestring file-path) predecode)))

(defun destroy-audio (audio)
  "Destroy the specified audio."
  (mix-destroy-audio audio))

(defun create-track (mixer)
  "Create a new track on a mixer."
  (check-null (mix-create-track mixer)))

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
  (declare (float gain))
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

(defun destroy-track (track)
  "Destroy the specified track."
  (mix-destroy-track track))

(defun track-playing-p (track)
  "Query if a track is currently playing."
  ( sdl-mixer-true-p (mix-track-playing track)))

(defun pause-track (track)
  "Pause a currently-playing track."
  (mix-pause-track track))

(defun resume-track (track)
  "Resume a currently-paused track."
  (mix-resume-track track))

(defun paused-track-p (track)
  "Returns T when the track is paused."
  (sdl-mixer-true-p (mix-track-paused track)))

(defun stop-track (track fade-out-frames)
  "Halt a currently-playing track, possibly fading out over time."
  (check-true (mix-stop-track track fade-out-frames)))

(defun convert-track-ms-to-frames (track ms)
  "Convert milliseconds to sample frames for a track's current format."
  (check-rc (mix-track-ms-to-frames track ms)))

(defun convert-track-frames-to-ms (track frames)
  "Convert sample frames for a track's current format to milliseconds."
  (check-rc (mix-track-frames-to-ms track frames)))
