(in-package #:sdl3-mixer)

(define-condition sdl-mixer-error (sdl3::sdl-rc-error) ())

;;; Note, Mix_GetError doesn't exist, it's a #define for SDL_GetError

(defmacro check-rc (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (< ,rc 0)
         (error 'sdl-mixer-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-zero (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (zerop ,rc)
         (error 'sdl-mixer-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-non-zero (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (unless (/= ,rc 0)
         (error 'sdl-mixer-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-true (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (unless (sdl-mixer-true-p ,rc)
         (error 'sdl-mixer-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-null (form)
  (with-gensyms (wrapper)
    `(let ((,wrapper ,form))
       (if (cffi:null-pointer-p (autowrap:ptr ,wrapper))
           (error 'sdl-mixer-error :rc ,wrapper :string (sdl-get-error))
           ,wrapper))))
