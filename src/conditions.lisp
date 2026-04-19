(in-package #:sdl3-mixer)

(define-condition sdl-mixer-error (sdl3::sdl-rc-error) ())

(defmacro check-rc (form)
  "Signal an error when the FORM has a return code less than zero."
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (< ,rc 0)
         (error 'sdl-mixer-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-zero (form)
  "Signal an error when the FORM has a return code of zero."
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (zerop ,rc)
         (error 'sdl-mixer-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-non-zero (form)
  "Signal an error when the FORM does not return zero."
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (unless (/= ,rc 0)
         (error 'sdl-mixer-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-true (form)
  "Signal an error when the FORM returns false."
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (unless (sdl-mixer-true-p ,rc)
         (error 'sdl-mixer-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-null (form)
  "Signal an error when the FORM returns nil."
  (with-gensyms (wrapper)
    `(let ((,wrapper ,form))
       (if (cffi:null-pointer-p (autowrap:ptr ,wrapper))
           (error 'sdl-mixer-error :rc ,wrapper :string (sdl-get-error))
           ,wrapper))))
