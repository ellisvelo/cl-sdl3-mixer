# cl-sdl3-mixer

A wrapper for SDL3_Mixer.

## Examples
A simple example program has been provided. Ogg support is required to run it however. Press the space bar to play the sound effect/song, the up directional key to increase the volume by 0.3 and the down directional key to decrease the volume by 0.3. The current volume is displayed in standard-output.

```lisp
(asdf:load-system :sdl3-mixer-examples)

;; Running an example
#-sbcl (sdl3-mixer-examples:simple)

;; MacOS requires that we initialise in the main thread
#+sbcl (sdl3:make-this-thread-main #'sdl3-mixer-examples:simple)
```

## Regenerating CFFI Bindings

This library uses [cl-autowrap](https://github.com/rpav/cl-autowrap) to generate CFFI bindings. If you need to regenerate the bindings, follow these steps:

1. Delete the existing bindings:

```
$ rm -f src/spec/SDL_mixer.*.spec
```

2. Reload the system in a REPL. This action will automatically regenerate the bindings:

```
${LISP-sbcl} \
    --load "sdl3-mixer.asd" \
    --eval "(asdf:load-sysm :sdl3-mixer)" \
    --eval "(uiop:quit)"
```

In most cases, this process should work without issues. However, if you encounter problems (usually due to environment-specific factors like missing include headers), you can use the `EXTRA_INCLUDES` environment variable to specify additional include paths:

```
EXTRA_INCLUDES=/data1/include:/data1/lib/include \
    ${LISP-sbcl} \
        --load "sdl3-mixer.asd" \
        --eval "(asdf:load-system :sdl3-mixer)" \
        --eval "(uiop:quit)"
```

This approach allows you to provide the necessary include paths without modifying the source code.

## Issues

If you cannot load `libSDL3_mixer`, please ensure that you have SDL_mixer 3.0,
installed and not just 2.0. If you receive errors concerning unknown file types,
please ensure that libSDL3_mixer is linked against the appropriate sound
library, *e.g. libVorbis for ogg support*.

If you are sure all of this is correct, and it still will not load, please [file an issue](https://github.com/ellisvelo/cl-sdl3-mixer/issues/new) and specify:

* Your platform and architecture
* Your lisp
* The absolute path to your installed `.so`, `.dll`, or the appropriate MacOS framework

