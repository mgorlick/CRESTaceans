#lang racket

(require ffi/unsafe ffi/unsafe/define
         "../../vortex.rkt")

(provide (all-defined-out)
         (rename-out [register_nocbs_profile register-no-cbs-profile]))

(define-ffi-definer def-no-cbs (ffi-lib "/usr/local/lib/libracket-vortex-no-cbs" "1.0"))

(def-no-cbs register_nocbs_profile (_fun _VortexCtx-pointer _VortexAsyncQueue-pointer -> _void))