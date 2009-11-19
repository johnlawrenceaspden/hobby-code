(require (lib "foreign.ss"))

(define cmath (ffi-lib "libm"))

(unsafe!)

(define csine (get-ffi-obj "sin" (ffi-lib "libm") (_fun _double -> _double)))

(csine 1.0)
(sin 1)