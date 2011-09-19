#lang racket

(require (planet bzlib/thread)
         (prefix-in gfx- "gfx.rkt")
         (prefix-in sim- "simulation.rkt"))
(provide/contract [make-game (flonum? flonum? exact-integer? . -> . void?)])

(define gfx-parent-thread (thread gfx-start-peer))
(define sim-parent-thread (thread sim-start-peer))

(define (number->integer n)
  (inexact->exact (round n)))

; make-game: double double integer -> void
; spawn a new instance of the game by spawning individual computations
; from the binding environments that hold their respective services
; and then link them together in a source/sink pattern, then start them
; (from the last sink, backward, to the first source)
(define (make-game width height depth)
  (let ([gfx-instance (computation-spawn gfx-parent-thread (number->integer width) (number->integer height) depth)]
        [sim-instance (computation-spawn sim-parent-thread width height)])
    (start/link gfx-instance sim-instance)
    (start/link sim-instance (list gfx-instance))
    (wait-for-shutdown-signal gfx-instance sim-instance)
    (void)))

; wait-for-shutdown-signal : thread thread ... -> void
; wait for a shutdown signal from the first thread.
; then, relay the shutdown signal to all the other threads.
(define (wait-for-shutdown-signal expected-sender . rest)
  (receive/match
   [(list expected-sender 'shutdown)
    (for ([t rest])
      (thread-cast t 'shutdown))]))

; computation-spawn: thread any ... -> thread
; spawn a computation off from the parent computation, using the common spawn interface
; any additional parent-specific arguments are appended to the end in order
(define (computation-spawn parent-thread . args)
  (let ([msg-args (append (list (current-thread) 'spawn-request) args)])
    (thread-send parent-thread msg-args)
    (receive/match
     [(list parent-thread 'spawn-notification (? thread? spawned-thread))
      spawned-thread])))

; start/link: thread any ... -> void
; start a computation using the common start interface.
; optionally, append extra arguments to the message in order to link
; one computation to another, or to multiple others, in a source/sink pattern
; for one: second argument is a thread
; for multiple: second argument is a (listof thread)
(define (start/link source . args)
  (let ([msg-args (append (list (current-thread) 'start-request) args)])
    (thread-send source msg-args)))