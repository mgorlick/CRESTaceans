#lang racket

(require (except-in ffi/unsafe ->)
         "../vtx/module.rkt"
         "thread.rkt"
         "thread-pool.rkt"
         "connection.rkt")
(provide (all-defined-out))

(define/contract (rkt:vortex-init-ctx ctx use-logging? use-ssl? ssl-cert-path)
  (VortexCtx*? boolean? boolean? (or/c string? false?) . -> . integer?) ; return must always be `axl_true' or `axl_false'
  
  (define sequencer-group (make-thread-group)) ; for sequencer
  (define reader-group (make-thread-group)) ; for reader
  (define tp-tg (make-thread-group)) ; for thread pool
  
  ;; replacement of vortex C components with custom components written in racket
  (vortex-thread-set-create rkt:vortex-thread-create)  
  (vortex-thread-pool-set-new-task rkt:vortex-thread-pool-new-task)
  (vortex-thread-pool-set-new-event rkt:vortex-thread-pool-new-event)
  (vortex-connection-set-listener-closures-setter rkt:vortex-connection-set-listener-mode-closures)
  (vortex-connection-set-client-closures-setter rkt:vortex-connection-set-client-mode-closures)
  
  (rkt:preinitialize-ctx ctx) ; performs all the steps of vortex_ctx_init except for what follows here...
  
  (parameterize ([current-thread-group reader-group])
    (vortex-reader-run ctx))
  (parameterize ([current-thread-group sequencer-group])
    (vortex-sequencer-run ctx))
  (parameterize ([current-thread-group tp-tg])
    (vortex-thread-pool-init ctx (vortex-thread-pool-get-num)))
  
  (vortex-ctx-mark-initialized ctx)
  (cond [use-logging? (vortex-log-enable ctx axl-true)])
  (cond [use-ssl? (vortex-ctx-set-ssl ctx axl-true ssl-cert-path)])
  axl-true)

(define/contract (rkt:preinitialize-ctx ctx)
  (VortexCtx*? . -> . any)
  (vortex-preinit-ctx ctx)
  (vortex-reader-prep-to-run ctx)
  (vortex-sequencer-prep-for-run ctx))