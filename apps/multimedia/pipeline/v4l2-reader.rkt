#lang racket/base

(require "util.rkt"
         "structs.rkt"
         "../bindings/vp8/vp8.rkt"
         (rename-in racket/contract (-> c/->))
         racket/set
         racket/match)

(provide make-v4l2-reader)

(define/contract (make-v4l2-reader signaller receiver)
  (thread? thread? . c/-> . (c/-> void))
  (λ ()
    (define v (v4l2-reader-setup))
    (define-values (w h fn fd buffer-ct) (v4l2-reader-get-params v))
    (thread-send receiver (make-VideoParams w h fn fd))
    (define v-sema (make-semaphore 1)) ; protect v4l2 reader and pool representation
    
    (define in-pool-range/c (between/c 0 (sub1 buffer-ct)))
    ; FIXME: turn contracts back on here once performance is tuned
    (define pool (for/set ([i (in-range buffer-ct)]) i))
    (define (pool-has? i) (set-member? pool i))
    (define (pool-add! i) (set! pool (set-add pool i)))
    (define (pool-remove! i) (set! pool (set-remove pool i)))
    
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
    
    (define (make-frame data size i ts)
      (define (requeue)
        (thread-send pool-helper i #f))
      (make-FrameBuffer data size requeue ts))
    
    (define is-signaller? (make-thread-id-verifier signaller))
    
    (define sleep-time (/ (/ fn fd) 3))
    
    (define (grab-frame ts)
      (semaphore-wait v-sema)
      (cond
        [(v4l2-reader-is-ready v) ; we have to do busy waiting for now - can't select() at a lower level -or- here
         (let-values ([(d f s i) (v4l2-reader-get-frame v)])
           (when d
             (thread-send receiver (make-frame d s i ts)) ; throw away framenum for now
             (pool-remove! i))
           (semaphore-post v-sema))
         #t]
        [else
         (semaphore-post v-sema)
         (sleep sleep-time) ; sleep for a quarter of the frame duration
         #f]))
        
    (define (cleanup)
      (command/killswitch signaller receiver)
      (semaphore-wait v-sema)
      (kill-thread pool-helper)
      (semaphore-post v-sema)
      ;; HACK: we need to wait for the next elements
      ;; of the pipeline to free themselves before we kill
      ;; the buffer so they're not writing to freed mmapped memory
      ;; since this is video one second should be enough.
      (sleep 1)
      (v4l2-reader-delete v)
      (reply/state-report signaller #f))
    
    (define (loop)
      ;; need an inner loop to trampoline back from busy-wait-triggering grab-frame calls
      ;; in order to keep accurate timestamps
      (define ts (current-inexact-milliseconds))
      (let loop2 ()
        (match (receive-killswitch/whatever is-signaller? #:block? #f)
          [(? no-message? _) (if (grab-frame ts)
                                 (loop2)
                                 (loop))]
          [(? die? _) (cleanup)])))
    
    (loop)))