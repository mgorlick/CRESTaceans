#lang racket

(require (except-in ffi/unsafe ->)
         "../vtx/module.rkt"
         "thread.rkt"
         "thread-pool.rkt"
         "connection.rkt")
(provide (all-defined-out))

;; replace default vortex implementations of i/o and threading
;; (which don't do anything) with Racket closures
(define (rkt:vortex-setup)
  ;; replacement of vortex C components with custom components written in racket
  (vortex-mutex-set-setup rkt:vortex-mutex-setup)
  (vortex-thread-set-create rkt:vortex-thread-create)
  (vortex-cond-set-setup rkt:vortex-cond-setup)
  (vortex-connection-set-listener-closures-setter rkt:vortex-connection-set-listener-mode-closures)
  (vortex-connection-set-client-closures-setter rkt:vortex-connection-set-client-mode-closures))

;; this function initializes a Vortex context in a way that glues all of the
;; Racket compatibility modifications together. Here we make separate thread groups
;; for different Vortex components, potentially turn on logging and SSL, and initialize all
;; the components. Most initialization is done on the C side but we need to initialize the
;; reader, sequencer and thread pool here so that we can assign them to new thread groups.
(define/contract (rkt:vortex-init-ctx ctx use-logging?)
  (VortexCtx*? boolean? . -> . integer?) ; return must always be `axl_true' or `axl_false'
  
  (define sequencer-group (make-thread-group)) ; for sequencer
  (define reader-group (make-thread-group)) ; for reader
  (define tp-tg (make-thread-group)) ; for thread pool
  
  (rkt:preinitialize-ctx ctx) ; performs all the steps of vortex_ctx_init except for what follows here...
  
  (parameterize ([current-thread-group reader-group])
    (vortex-reader-run ctx))
  (parameterize ([current-thread-group sequencer-group])
    (vortex-sequencer-run ctx))
  (parameterize ([current-thread-group tp-tg])
    (vortex-thread-pool-init ctx 5)
    )
  
  (vortex-ctx-mark-initialized ctx)
  (cond [use-logging? (vortex-log-enable ctx axl-true)])
  axl-true)

(define/contract (rkt:preinitialize-ctx ctx)
  (VortexCtx*? . -> . any)
  (vortex-preinit-ctx ctx)
  (vortex-reader-prep-to-run ctx)
  (vortex-sequencer-prep-for-run ctx))