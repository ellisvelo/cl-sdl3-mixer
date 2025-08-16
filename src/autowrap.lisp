(cl:in-package #:sdl3-ffi)

(autowrap:c-include
 `(sdl3-mixer autowrap-spec "SDL3_mixer.h")
  :function-package :sdl3-ffi.functions
  :spec-path '(sdl3-mixer autowrap-spec)
  :exclude-sources ("/usr/local/lib/clang/([^/]*)/include/(?!stddef.h)"
                    "/usr/include/"
                    "/usr/include/arm-linux-gnueabihf"
                    "/usr/local/include/SDL3"
		    ".*/SDL3/SDL_.*.h"
                    #+darwin "/opt/homebrew/include/SDL3")
  :sysincludes (cl:append
                 #+darwin '("/opt/homebrew/include")
                 (cl:when (uiop:getenv "EXTRA_INCLUDES")
                   (uiop:split-string (uiop:getenv "EXTRA_INCLUDES") :separator " ")))
  :include-sources ("SDL3_mixer.h")
  :exclude-constants ("^(?!MIX)")
  :symbol-exceptions (("SDL_RWops" . "SDL-RWOPS"))
  :no-accessors cl:t
  :release-p cl:t)
