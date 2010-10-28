#lang racket

(require (except-in ffi/unsafe ->)
         "../vtx/module.rkt"
         "thread.rkt"
         "thread-pool.rkt"
         "connection.rkt")
(provide (all-defined-out))

(define/contract (rkt:vortex-init-ctx ctx)
  (VortexCtx*? . -> . integer?) ; the integer must always be `axl_true' or `axl_false'
  
  (define sequencer-group (make-thread-group)) ; for sequencer
  (define reader-group (make-thread-group)) ; for reader
  (define tp-tg (make-thread-group)) ; for thread pool
  
  ;; replacement of vortex C components with custom components written in racket
  (vortex-thread-set-create rkt:vortex-thread-create)  
  (vortex-thread-pool-set-new-task rkt:vortex-thread-pool-new-task)
  (vortex-thread-pool-set-new-event rkt:vortex-thread-pool-new-event)
  (vortex-connection-set-listener-closures-setter rkt:vortex-connection-set-listener-mode-closures)
  (vortex-connection-set-client-closures-setter rkt:vortex-connection-set-client-mode-closures)
  
  (printf "log (debug level): Doing alternate vortex ctx initialization in Racket~n")
  (rkt:preinitialize-ctx ctx) ; performs all the steps of vortex_ctx_init except for what follows here...

  (parameterize ([current-thread-group reader-group])
    (printf "log (debug level): starting vortex reader...~n")
    (vortex-reader-run ctx)
    )
  
  (parameterize ([current-thread-group sequencer-group])
    (printf "log (debug level): starting vortex sequencer...~n")
    (vortex-sequencer-run ctx)
    )
  
  (parameterize ([current-thread-group tp-tg])
    (printf "log (debug level): starting vortex thread pool (threadcount: ~s)~n" 
            (vortex-thread-pool-get-num))
    (vortex-thread-pool-init ctx (vortex-thread-pool-get-num))
    )
  
  (printf "log (debug level): ctx initialized in Racket~n")
  (vortex-ctx-mark-initialized ctx)
  
  axl-true)

(define/contract (rkt:preinitialize-ctx ctx)
  (VortexCtx*? . -> . any)
  (vortex-preinit-ctx ctx)
  (vortex-reader-prep-to-run ctx)
  (vortex-sequencer-prep-for-run ctx))