#lang racket

(require (except-in ffi/unsafe ->)
         "../vtx/module.rkt")
(provide (all-defined-out))

;; partial thread replacement for vortex library

; original vortex used extra pthreads to perform callbacks, which corrupted the
; racket stack upon callback attempt (if the pthread was not the main one, i.e.,
; the one driving the racket evaluator itself.) here we just replace the default
; vortex thread creation function with one that creates green threads.

; caveat: the ffi cannot send pointers to green threads across the racket <-> C
; boundary. vortex holds on to thread references but doesn't do anything with them;
; if this changes, another solution will have to be found.

;; VortexThreadCreateFunc: VortexThread* VortexThreadFunc pointer -> axl_bool
(define/contract (rkt:vortex-thread-create thread* func user-data)
  (cpointer? procedure? cpointer? . -> . integer?)
  (thread (lambda () 
            (vortex-thread-set-reference thread*)
            (func user-data)))
  axl-true)