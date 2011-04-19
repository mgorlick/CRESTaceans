#! /usr/bin/env racket
#lang racket

(require "../util.rkt"
         "../../../../bindings/theora/theora.rkt")

(provide make-reader)

(define-thread some-downstream-consumer
  (let loop ()
    (match-let ([(V4L2Frame data framenum λdisposal) (thread-receive)])
      ;; ... do something with data ...
      (when (= (modulo framenum 30) 0)
        (printf "got ~a bytes for frame #~a~n" (bytes-length data) framenum))
      ;; assume this is the last consumer to use the buffer in this address space -
      ;; ok to dispose of it by calling the disposal thunk
      (λdisposal))
    (loop)))

(define (make-reader signaller receiver)
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
  
  (define (make-frame d f i)
    (V4L2Frame d f (λ () 
                     (with-handlers ([exn:fail? (λ (e) #f)]) 
                       (thread-send pool-helper i)))))
  
  (define (cleanup)
    (semaphore-wait v-sema)
    (kill-thread pool-helper)
    (v4l2-reader-delete v))
  
  (define is-signaller? (make-thread-id-verifier signaller))
  
  (let loop ()
    (match (receive-killswitch/whatever is-signaller? #:block? #f)
      [(? die? _) (cleanup)
                  (command/killswitch signaller receiver)
                  (reply/state-report signaller #f)]
      [else (semaphore-wait v-sema)
            (let-values ([(d f i) (v4l2-reader-get-frame v)])
              (cond 
                [d (thread-send receiver (make-frame d f i))
                   (pool-remove! i)
                   (semaphore-post v-sema)
                   (loop)]
                [else (semaphore-post v-sema)
                      (printf "Error: null reference from v4l2 API. ")
                      (printf "Check that buffers are being requeued after use.~n")
                      ]))])))

(make-reader (current-thread) some-downstream-consumer)