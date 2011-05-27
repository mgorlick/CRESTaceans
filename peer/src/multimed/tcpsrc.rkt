#lang racket

(require "util.rkt"
         "structs.rkt"
         "bufferpool.rkt")

(provide make-tcp-src)

(define BUFFSIZE (* 1024 1024))

(define/contract (make-tcp-src signaller inbound-host inbound-port receiver)
  (thread? (or/c string? #f) exact-nonnegative-integer? thread? . -> . (-> void))
  (define is-signaller? (make-thread-id-verifier signaller))
  (define listener (tcp-listen inbound-port))
  (define-values (i o) (tcp-accept listener))
  (define-values (handler λrequest) (make-bufferpool-handler 20 BUFFSIZE))
  
  (define mailbox-evt (thread-receive-evt))
  (define socket-evt (read-bytes-evt 4 i))
  
  (λ ()
    (let loop ()
      (define-values (buffer λdisposal) (λrequest))
      
      (match (sync socket-evt mailbox-evt)
        [(? bytes? b) (let* ([s (integer-bytes->integer b #f #t)]
                             [s* (read-bytes! buffer i 0 s)])
                        (thread-send receiver (make-FrameBuffer buffer s* λdisposal)))
                      (loop)]
        [(? eof-object? n) (loop)]
        [(? evt? _) (match (receive-killswitch/whatever is-signaller?)
                      [(? die? sig) (close-input-port i)
                                    (close-output-port o)
                                    (tcp-close listener)
                                    (command/killswitch signaller receiver)
                                    (reply/state-report signaller #f)]
                      [_ (loop)])]))))