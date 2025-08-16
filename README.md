# cl-sdl3-mixer

This is a brief but usuable wrapper for SDL3_Mixer.

## Usage
The following functions are currently available to the users
* `(sdl3-mixer:linked-version)`: Returns the version number for SDL Mixer 2. Calls [Mix_Linked_Version](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC8)
* `(sdl3-mixer:init &rest formats)`: Initialize the SDL mixer specifying the formats you wish to use. Must be one of these values or a combination thereof `:ogg`, `:wave`, `:mod`, `:mp3`. Calls [Mix_Init](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC9).
* `(sdl3-mixer:quit)` Cleans up SDL Mixer. Calls [Mix_Quit](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC10).
* `(sdl3-mixer:open-audio frequency format channels chunksize)`: Initialize the mixer specifiying the output sample format, number of output channels (1 mono or 2 for stereo), and bytes used per output sample. format must be one of the following values, `:u8`, `:s8`, `:u16lsb`, `:s16lsb`, `:u16msb`, `:s16msb`, `:u16`, `:s16`, `:u16sys`, `:s16sys`, `:s32lsb`, `:s32msb`, `:s32sys`, `:s32`, `:f32lsb`, `:f32msb`, `:f32sys`, `:f32`. Calls [Mix_OpenAudio](https://wiki.libsdl.org/SDL_mixer/Mix_OpenAudio)
* `(sdl3-mixer:close-audio)`: Closes the mixer. Calls [Mix_CloseAudio](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC12)
* `(sdl3-mixer:query-format)`: Gets the output format in use by the opened audio device. Calls [Mix_QuerySpec](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC15)
* `(sdl3-mixer:load-wav sample-file-name)`: Loads the sample specified by the sample-file-name. Returns a mix-chunk. sdl3-mixer must be initialized and open-audio must be called prior to. Calls  [Mix_LoadWav_RW](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC20). Please refer to the source comments for more details if you are interested in why it calls Mix_LoadWav_RW as opposed to Mix_LoadWav
* `(sdl3-mixer:allocate-channels number-of-channels)`: Set the number of channels to be mixed. Opening too many channels may result in a segfault. This can be called at any time even while samples are playing. Passing a number lower than previous calls will close unused channels. It returns the number of channels allocated. NOTE: Channels are 0 indexed! Calls [Mix_AllocateChannels](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC26)
* `(sdl3-mixer:volume channel volume)`: Set the volume on a given channel, pass -1 to set the volume for all channels. The volume may range from 0 to 128. Passing in a number higher than the maximum will automatically set it to the maximum while passing in a negatiev will automatically set it to 0. Returns the current volume of the channel. NOTE: Channels are 0 indexed! Calls [Mix_Volume](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC27)
* `(sdl3-mixer:play-channel channel mix-chunk loops)`: Plays the mix-chunk (sound effect) loops+1 times on a given channel. Passing -1 for the channel will play it on the first unreserved channel. Returns the channel the sample is played on. NOTE: Channels are 0 indexed! Calls [Mix_PlayChannel](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC28)
* `(sdl3-mixer:halt-channel)` Halt the channel or pass -1 to halt all channels. Always returns 0. NOTE: Channels are 0 indexed! Calls [Mix_HaltChannel](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC34)
* `(sdl3-mixer:free-chunk chunk)` Free memory used for a given chunk. Do not free the chunk while it's being called. Calls[Mix_FreeChunk](http://jcatki.no-ip.org:8080/SDL_mixer/SDL_mixer_frame.html)
* `(sdl3-mixer:load-music music-file-name)` Loads music from a file. Returns a mix-music object. Calls [Mix_LoadMUS](http://jcatki.no-ip.org:8080/SDL_mixer/SDL_mixer_55.html#SEC55)
* `(sdl3-mixer:free-music mix-music-object)` Free memory used by a music object. Calls [Mix_FreeMusic](http://jcatki.no-ip.org:8080/SDL_mixer/SDL_mixer_56.html#SEC56)
* `(sdl3-mixer:play-music mix-music-object loops)` Play music object looping the number of times specified by the loops argument. -1 plays indefinitely. Calls [Mix_PlayMusic](http://jcatki.no-ip.org:8080/SDL_mixer/SDL_mixer_57.html#SEC57)
* `(sdl3-mixer:halt-music)` Halts all playing music. Calls [Mix_HaltMusic](http://jcatki.no-ip.org:8080/SDL_mixer/SDL_mixer_67.html#SEC67)
* `(sdl3-mixer:volume-music volume)` Sets the music volume to the volume specified. Calls [Mix_VolumeMusic](http://jcatki.no-ip.org:8080/SDL_mixer/SDL_mixer_frame.html)

## Examples
A simple example program has been provided. Ogg support is required to run it however. Press the space bar to play the sound effect/song, the up directional key to increase the volume by 20 and the down directional key to decrease the volume by 20. The current volume is displayed in standard-output.

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
    --eval "(ql:quickload '(:sdl3-mixer))" \
    --eval "(uiop:quit)"
```

In most cases, this process should work without issues. However, if you encounter problems (usually due to environment-specific factors like missing include headers), you can use the `EXTRA_INCLUDES` environment variable to specify additional include paths:

```
EXTRA_INCLUDES=/data1/include:/data1/lib/include \
    ${LISP-sbcl} \
        --load "sdl3-mixer.asd" \
        --eval "(ql:quickload '(:sdl3-mixer))" \
        --eval "(uiop:quit)"
```

This approach allows you to provide the necessary include paths without modifying the source code.

## Issues

If you cannot load `libSDL3_mixer`, please ensure that you have SDL_mixer 2.0,
installed and not just 1.2. If you receive errors concerning unknown file types,
please ensure that libSDL3_mixer is linked against the appropriate sound
library, *e.g. libVorbis for ogg support*. As of writing (05-31-2015) the
SDL_mixer 2.0 provided by brew on OSX does not link libVorbis correctly, please
build it from source.


If you are sure all of this is correct, and it still will not load, please [file an issue](https://github.com/lispgames/cl-sdl3-mixer/issues/new) and specify:

* Your platform and architecture
* Your lisp
* The absolute path to your installed `.so`, `.dll`, or the appropriate OSX framework

