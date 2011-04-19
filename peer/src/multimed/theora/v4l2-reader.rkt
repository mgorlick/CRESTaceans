#! /usr/bin/env racket
#lang racket

(require "../util.rkt"
         "../../../../bindings/theora/theora.rkt"
         "structs.rkt")

(provide make-v4l2-reader)

(define (make-v4l2-reader signaller receiver)
  (λ ()
    (define v (v4l2-reader-setup))
    (define v-sema (make-semaphore 1)) ; protect v4l2 reader and pool representation
    (define-values (w h fn fd buffer-ct) (v4l2-reader-get-params v))
    
    (define pool (for/set ([i (in-range buffer-ct)]) i))
    (define (pool-add! i) (set! pool (set-add pool i)))
    (define (pool-remove! i) (set! pool (set-remove pool i)))
    (define (pool-has? i) (set-member? pool i))
    (define-thread pool-helper
      (let loop ()
        (let ([index (thread-receive)])
          (semaphore-wait v-sema)
          (unless (pool-has? index)
            (if (v4l2-reader-enqueue-buffer v index)
                (pool-add! index)
                (printf "error: couldn't requeue buffer #~a~n" index)))
          (semaphore-post v-sema))
        (loop)))
    
    (define (make-frame data num i)
      (make-VideoFrameBuffer data num (λ () (requeue i))))
    
    (define (requeue i)
      (with-handlers ([exn:fail? (λ (e) (void))]) 
        (thread-send pool-helper i)
        (void)))
    
    (define is-signaller? (make-thread-id-verifier signaller))
    
    (let loop ()
      (match (receive-killswitch/whatever is-signaller? #:block? #f)
        [(? die? _) (semaphore-wait v-sema)
                    (kill-thread pool-helper)
                    (v4l2-reader-delete v)
                    (command/killswitch signaller receiver)
                    (reply/state-report signaller #f)]
        [(? no-message? _) (semaphore-wait v-sema)
                           (let-values ([(d f i) (v4l2-reader-get-frame v)])
                             (when d (thread-send receiver (make-frame d f i)))
                             (pool-remove! i))
                           (semaphore-post v-sema)
                           (loop)]))))