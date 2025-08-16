(in-package #:sdl3-mixer)

(defmacro create-sdl-free-function (free-function sdl-object)
  `(progn (tg:cancel-finalization ,sdl-object)
          (,free-function ,sdl-object)
          (autowrap:invalidate ,sdl-object)))

(defun version ()
  "Get the version of SDL_mixer that is linked against your program and return
the major, minor, and micro version."
  (let* ((version (mix-version))
	 (major (floor (/ version 1000000)))
	 (minor (floor (mod (/ version 1000) 1000)))
	 (micro (mod version 1000)))
    (values major minor micro)))

#+nil(autowrap:define-bitmask-from-enum (init-flags sdl3-ffi:mix-init-flags))
(autowrap:define-bitmask-from-constants (init-flags)
  sdl3-ffi:+mix-init-flac+
  sdl3-ffi:+mix-init-mod+
  sdl3-ffi:+mix-init-mp3+
  sdl3-ffi:+mix-init-ogg+
  sdl3-ffi:+mix-init-mid+
  sdl3-ffi:+mix-init-opus+
  sdl3-ffi:+mix-init-wavpack+)

(defun sdl-mixer-true-p (integer-bool)
  "Use this function to convert from a low level wrapped SDL_Mixer function
returning an SDL_true into CL's boolean type system."
  (= 1 integer-bool))

(defun init (&rest flags)
  "Initialize the SDL mixer specifying the formats you wish to use. Must be one
of these values or a combination thereof :ogg, :wave, :mod, :mp3"
  (mix-init (mask-apply 'init-flags flags)))

(defun quit ()
  "Cleans up SDL Mixer"
  (mix-quit))

(defun open-audio (device-id &key (frequency sdl3-ffi:+mix-default-frequency+)
			       (format sdl3-ffi:+mix-default-format+)
			       (channels sdl3-ffi:+mix-default-channels+))
  "Open an audio device for playback."
  (c-let ((audio-spec sdl3-ffi:sdl-audio-spec))
    (setf (audio-spec :freq) frequency)
    (setf (audio-spec :format) (enum-value '(:enum (sdl3-ffi:sdl-audio-format)) format))
    (setf (audio-spec :channels) channels)
    (check-true (mix-open-audio device-id (audio-spec &)))))

(defun close-audio ()
  "Closes the mixer"
  (mix-close-audio))

(defun query-spec ()
  "Gets the output format in use by the opened audio device"
  (c-with ((freq :int)
           (fmt sdl3-ffi:uint16)
           (chans :int))
    (check-non-zero (mix-query-spec (freq &) (fmt &) (chans &)))
    (values freq (enum-key '(:enum (audio-format)) fmt) chans)))

(defun load-wav (sample-file-name)
  "Loads the sample specified by the sample-file-name. Returns a mix-chunk.
sdl3-mixer must be initialized and open-audio must be called prior to."
  ;;Note the original Mix_LoadWAV function is actually a C preprocessor function
  ;;macro that was not collected by c2ffi. However the manual does state that
  ;;Mix_LoadWAV is equivalent to calling
  ;;Mix_LoadWAV_RW(SDL_RWFromFile(file,"rb"), 1) where file is a character array
  ;;representing the file
  ;;https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC19
  (autocollect (ptr)
               (check-null (mix-load-wav-io
                            (sdl-io-from-file
                             (namestring sample-file-name) "rb") 1))
    (mix-free-chunk ptr)))

(defun free-chunk (chunk)
  "Free the memory used in the chunk and then free the chunk itself. Do not free
the chunk while it is playing; halt the channel it's playing on using
halt-channel prior to freeing the chunk."
  (create-sdl-free-function mix-free-chunk chunk))

(defun allocate-channels (channels)
  "Set the number of channels to be mixed. Opening too many channels may result
in a segfault. This can be called at any time even while samples are playing.
Passing a number lower than previous calls will close unused channels. It
returns the number of channels allocated. NOTE: Channels are 0 indexed!"
  ;;This supposedly never fails so no check is in place
  (mix-allocate-channels channels))

(defun volume (channel volume)
  "Set the volume on a given channel, pass -1 to set the volume for all
channels. The volume may range from 0 to 128. Passing in a number higher than
the maximum will automatically set it to the maximum while passing in a negatiev
will automatically set it to 0. Returns the current volume of the channel. NOTE:
Channels are 0 indexed!"
  (mix-volume channel volume))

(defun play-channel (channel mix-chunk loops)
  "Plays the mix-chunk (sound effect) loops+1 times on a given channel. Passing
-1 for the channel will play it on the first unreserved channel. Returns the
channel the sample is played on. NOTE: Channels are 0 indexed!"
  ;; The original Mix_PlayChannel function is just a function-like C
  ;; preprocessor macro much like Mix_LoadWAV which was not in the spec.
  ;; According to the docs Mix_PlayChannel is simply Mix_PlayChannelTimed with
  ;; ticks set to -1
  ;; https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer_frame.html
  (check-rc (mix-play-channel-timed channel mix-chunk loops -1)))

(defun set-channel-finished-callback (cffi-callback-fn)
  "Sets a callback that will be invoked after the channel has finished
playing. CFFI-CALLBACK-FN is defined with CFFI:DEFCALLBACK. Using a value of NIL
will disable the callback."
  (mix-channel-finished cffi-callback-fn))

(defun playing (channel)
  "Checks whether or not a channel is currently playing. It will return a 1 for
playing and 0 otherwise. Passing -1 for the channel will specify how many
channels are playing."
  (mix-playing channel))

(defun pause-channel (channel)
  "Pauses the CHANNEL. A value of -1 will pause all channels."
  (mix-pause channel))

(defun resume-channel (channel)
  "Resumes a paused CHANNEL. A value of -1 will resume all channels."
  (mix-resume channel))

(defun paused-channel-p (channel)
  "Returns T when the CHANNEL is paused."
  (sdl-mixer-true-p (mix-paused channel)))

(defun halt-channel (channel)
  "Halt the channel or pass -1 to halt all channels. Always returns 0. NOTE:
Channels are 0 indexed!"
  (mix-halt-channel channel))

(defun load-music (music-file-name)
  "Loads music from a file. Returns a mix-music object"
  (autocollect (ptr)
      (check-null (mix-load-mus (namestring music-file-name)))
    (mix-free-music ptr)))

(defun free-music (mix-music-object)
  (create-sdl-free-function mix-free-music mix-music-object))

(defun play-music (mix-music-object &optional (loops -1))
  "Play the music as many times as specified by the optional loops argument. By
default loops is -1 which makes the music loop indefinitely. Returns 0 on
success -1 on error"
  (check-rc (mix-play-music mix-music-object
                            loops)))

(defun fade-in-music (mix-music-object &optional (loops -1) (ms 1000))
  "Fade in music over MS milliseconds and repeat as specified by LOOPS. The
default number of milliseconds for fade in is 1000."
  (check-rc (mix-fade-in-music mix-music-object loops ms)))

(defun fade-out-music (ms)
  "Fade out the music over MS milliseconds and then halt it."
  (check-zero (mix-fade-out-music ms)))

(defun pause-music ()
  "Pause the music stream"
  (mix-pause-music))

(defun resume-music ()
  "Resume the music stream"
  (mix-resume-music))

(defun paused-music-p ()
  "Return T when the music stream is paused"
  (sdl-mixer-true-p (mix-paused-music)))

(defun halt-music ()
  "Halts the playback of all music"
  (mix-halt-music))

(defun volume-music (music-volume)
  "Adjust the volume of the music. Volume ranges from 0 to 128. The return value
is an integer that usually represents the previous volume setting. Passing -1 as
the music volume does not change the volume but instead returns the current
volume setting"
  (mix-volume-music music-volume))
