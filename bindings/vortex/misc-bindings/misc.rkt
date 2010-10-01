#lang racket

(require ffi/unsafe ffi/unsafe/define
         "../vtx/libvortex.rkt")
(provide (all-defined-out))

; bindings for miscellaneous *nix utilies
; pipes
; file descriptors
; timevals

(define-ffi-definer def-misc (ffi-lib "libracket-vortex-misc" "1.0"))

(define (rkt_pipe fd)
  (def-misc rkt_pipe (_fun (_vector i _int) -> _int))
  (let ([res (rkt_pipe fd)])
    (if (eq? -1 res)
        (raise 'pipe-creation-error)
        fd)))
(def-misc rkt_read (_fun _int _string _int -> (result : _int) -> result))
(def-misc rkt_write (_fun _int _string _int -> _int))

(define-cpointer-type _fd-set-pointer)

(def-misc rkt_fd_make (_fun -> (_or-null _fd-set-pointer)))
(def-misc rkt_fd_free (_fun (_or-null _fd-set-pointer) -> _void))
(def-misc rkt_fd_zero (_fun (_or-null _fd-set-pointer) -> _void))
(def-misc rkt_fd_set (_fun _int (_or-null _fd-set-pointer) -> _void))
(def-misc rkt_fd_clr (_fun _int (_or-null _fd-set-pointer) -> _void))
(def-misc rkt_fd_isset (_fun _int (_or-null _fd-set-pointer) -> _int))

(def-misc rkt_timeval_make (_fun _long _long -> _timeval-pointer))
(def-misc rkt_timeval_free (_fun _timeval-pointer -> _void))

(def-misc rkt_select (_fun _int (_or-null _fd-set-pointer) (_or-null _fd-set-pointer)
                           (_or-null _fd-set-pointer) _timeval-pointer -> _int))
