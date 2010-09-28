#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         "../../vortex.rkt")
(provide (all-defined-out))

(define-ffi-definer def-simple (ffi-lib "/usr/local/lib/libracket-vortex-simpletest" "1.0"))

(def-simple register_plain_profile (_fun _VortexCtx-pointer -> _int))
(def-simple set_on_accepted (_fun _VortexCtx-pointer -> _void))