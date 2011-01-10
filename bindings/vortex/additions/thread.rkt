#lang racket

(require (except-in ffi/unsafe ->)
         "../vtx/module.rkt")
(provide rkt:vortex-thread-create
         rkt:vortex-mutex-setup
         rkt:vortex-cond-setup)

;; partial thread replacement for vortex library

; original vortex used extra pthreads to perform callbacks, which corrupted the
; racket stack upon callback attempt (if the pthread was not the main one, i.e.,
; the one driving the racket evaluator itself.) here we just replace the default
; vortex thread creation function with one that creates green threads.

; caveat: the ffi cannot send pointers to green threads across the racket <-> C
; boundary. instead, send the VortexThread** back to vortex C side where Vortex
; can grab it from a conveniently-placed global variable `current_scheme_thread'

;; VortexThreadCreateFunc: VortexThread** VortexThreadFunc pointer -> axl_bool
(define/contract (rkt:vortex-thread-create thread* func user-data)
  (cpointer? procedure? cpointer? . -> . integer?)
  (thread (lambda () 
            (vortex-thread-set-reference thread*)
            (func user-data)))
  axl-true)

;; Condition variable implementation from (Birrell, 2003)
(struct condvar (s x h
                   [waiters #:mutable]))

(define (cond-create)
  (condvar (make-semaphore 0) ; s signals a thread to resume
           (make-semaphore 1) ; x protects waiters count
           (make-semaphore 0) ; h is handshake to ensure that correct threads drop through 
           0))

(define (cvcount cv)
  (condvar-waiters cv))

(define (cvinc cv)
  (set-condvar-waiters! cv (add1 (condvar-waiters cv))))

(define (cvdec cv)
  (set-condvar-waiters! cv (sub1 (condvar-waiters cv))))

(define (cvlock cv) ; P
  (semaphore-wait (condvar-x cv)))

(define (cvunlock cv) ; V
  (semaphore-post (condvar-x cv)))

(define (handshake-init cv) ; P
  (semaphore-wait (condvar-h cv)))

(define (handshake-end cv) ; V
  (semaphore-post (condvar-h cv)))

(define (signal-init cv) ; P
  (semaphore-wait (condvar-s cv)))

(define (signal-end cv) ; V
  (semaphore-post (condvar-s cv)))

(define (rkt:vortex-cond-setup vtx-cond-var*)
  
  (define cv (cond-create))
  
  ;; precondition: none
  ;; signal one thread waiting on the condition variable
  (define (cond-signal)
    (cvlock cv)
    (cond [(> (cvcount cv) 0)
           (cvdec cv)
           (signal-end cv)
           (handshake-init cv)])
    (cvunlock cv))
  
  ;; precondition: none
  ;; signal all threads waiting on the condition variable
  ;; (i.e., those that enqueued on `waiters' before (1) executed
  (define (cond-broadcast)
    (cvlock cv) ; (1)
    (for/list ([i (in-range (cvcount cv))])
      (signal-end cv))
    (let loop ()
      (when (> (cvcount cv) 0)
        (cvdec cv)
        (handshake-init cv)
        (loop)))
    (cvunlock cv))
  
  ;; precondition: this thread holds `mutex'
  ;; wait for a signal on the condition variable
  (define (cond-wait mutex)
    (cvlock cv)
    (cvinc cv)
    (cvunlock cv)
    (vortex-mutex-unlock mutex)
    (signal-init cv)
    (handshake-end cv)
    (vortex-mutex-lock mutex)
    axl-true
    )
  
  ;; precondition: this thread holds `mutex'
  ;; wait for a signal on the condition variable
  ;; for `usec' microseconds
  ;; DANGER!! 
  ;; you might think to redo this in the form
  ;; (if (semaphore-try-wait? cv)
  ;;     (... do something ...)
  ;;     (... do something else ...))
  ;; do not do that!!
  ;; it makes things much much slower (30-40x or more) than syncing on an event directly
  ;; there may be a spurious negative result here
  ;; (i.e., a thread is told that it did not successfully
  ;; wait on the cond when it actually did.)
  ;; if that is the case, it is not a problem since
  ;; it'll be successful on the next round through.
  (define (cond-timedwait mutex usecs)
    (cvlock cv)
    (cvinc cv)
    (cvunlock cv)
    (vortex-mutex-unlock mutex)
    (let ([ready-or-not (sync/timeout (/ usecs 1000000) (semaphore-peek-evt (condvar-s cv)))])
      (if (evt? ready-or-not)
          (begin
            (semaphore-wait (condvar-s cv))
            (handshake-end cv)
            (vortex-mutex-lock mutex)
            axl-true)
          (begin
            (cvlock cv)
            (cvdec cv)
            (cvunlock cv)
            (vortex-mutex-lock mutex)
            axl-false))))
  
  (vortex-cond-set-closures vtx-cond-var* cond-signal cond-broadcast cond-wait cond-timedwait))

;; Mutex replacement
;; very important: ensure that only one thread posts to a mutex between calls to lock it
(define (m-lock m)
  (semaphore-wait m))

(define (m-unlock m)
  (semaphore-post m))

(define (rkt:vortex-mutex-setup mutex*)
  (define m (make-semaphore 1))
  (define (unlock) (m-unlock m))
  (define (lock) (m-lock m))
  (vortex-mutex-set-closures mutex* lock unlock))