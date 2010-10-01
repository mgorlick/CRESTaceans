#lang racket

(require ffi/unsafe ffi/unsafe/define
         "../../vortex.rkt")

(provide (all-defined-out)
         )

(define-ffi-definer def-no-cbs (ffi-lib "libracket-vortex-no-cbs" "1.0"))

(def-no-cbs server_register_nocbs_profile (_fun _VortexCtx-pointer _VortexAsyncQueue-pointer -> _void))
(def-no-cbs client_register_nocbs_profile (_fun _VortexCtx-pointer _VortexAsyncQueue-pointer -> _void))

(def-no-cbs set_close_notify (_fun _VortexChannel-pointer _VortexAsyncQueue-pointer -> _void))

(def-no-cbs set_frame_received_handler (_fun _VortexChannel-pointer _axlPointer -> _void))

(def-no-cbs get_frame (_fun _int -> _int))
(def-no-cbs get_close (_fun _int -> _int))
(def-no-cbs frameaddr (_fun -> (_vector i _int)))
(def-no-cbs closeaddr (_fun -> (_vector i _int)))

(def-no-cbs make_frame_pipe (_fun -> _int))
(def-no-cbs make_close_pipe (_fun -> _int))
