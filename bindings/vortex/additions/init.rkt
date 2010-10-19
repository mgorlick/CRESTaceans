#lang racket

(require (except-in ffi/unsafe ->)
         "../vtx/module.rkt")
(provide (all-defined-out))

(define/contract (rkt:vortex-init-ctx ctx)
  (VortexCtx*? . -> . integer?)
  (define s&r-tg (make-thread-group)) ; for reader and sequencer
  (define tp-tg (make-thread-group)) ; for thread pool  
  (vortex-thread-set-create rkt:vortex-thread-create)  
  (printf "log (debug level): Doing alternate vortex ctx initialization in Racket~n")
  (rkt:preinitialize-ctx ctx)

  (parameterize ([current-thread-group s&r-tg]) ; launch the reader and sequencer in one thread group
    (printf "log (debug level): starting vortex reader...~n")
    (vortex-reader-run ctx)
    (printf "log (debug level): starting vortex sequencer...~n")
    (vortex-sequencer-run ctx)
    )
  
  (parameterize ([current-thread-group tp-tg]) ; launch the thread pool members in one thread group
    (printf "log (debug level): starting vortex thread pool (threadcount: ~s)~n" 
            (vortex-thread-pool-get-num))
    (vortex-thread-pool-init ctx (vortex-thread-pool-get-num))
    )
  
  (printf "log (debug level): ctx initialized in Racket~n")
  (vortex-ctx-mark-initialized ctx)

  ; digusting unprincipled hack: make the main thread sleep (for as little as possible)
  ; to schedule the reader, sequencer and thread pool members in first
  ;(sleep 0)
  axl-true)

(define (rkt:preinitialize-ctx ctx)
  (vortex-preinit-ctx ctx)
  (vortex-reader-prep-to-run ctx)
  (vortex-sequencer-prep-for-run ctx))

;; VortexThreadCreateFunc: VortexThread* VortexThreadFunc pointer -> axl_bool
(define/contract (rkt:vortex-thread-create thread* func user-data)
  (VortexThread*? procedure? cpointer? . -> . any)
  (let ([t (thread (lambda () (func user-data)))])
    ;(ptr-set! thread* _pointer t)
    axl-true))