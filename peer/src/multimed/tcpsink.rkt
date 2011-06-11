#lang typed/racket/base

(require "util-types.rkt"
         "structs.rkt"
         (only-in typed/racket
                  tcp-connect
                  tcp-accept
                  match))

(require/typed "util.rkt"
               [receive-killswitch/whatever ((Any -> Boolean) -> (U FrameBuffer Bytes Symbol))])

(provide make-tcp-sink)

(: make-tcp-sink (Thread String Exact-Nonnegative-Integer -> (-> Void)))
(define (make-tcp-sink signaller remote-host remote-port)
  (define is-signaller? (make-thread-id-verifier signaller))
  (define-values (i o) (tcp-connect remote-host remote-port))
  (λ ()
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? sig)
         (reply/state-report signaller #f)
         (close-input-port i)
         (close-output-port o)]
        
        [(FrameBuffer buffer size λdisposal)
         (with-handlers ([exn:fail? (λ (e) (printf "Error ~a: packet size is ~a~n" e size))])
           (write-length o size)
           (write-bytes buffer o 0 size)
           (λdisposal)
           (loop))]
        
        ;        [(? bytes? buffer)
        ;           (with-handlers ([exn:fail? (λ (e) (printf "Error ~a: packet size is ~a~n" e (bytes-length buffer)))])
        ;             (write-length o (bytes-length buffer))
        ;             (write-bytes buffer o)
        ;             (loop))]
        ))))

(: write-length (Output-Port Natural -> Natural))
(define (write-length o size)
  (write-bytes (integer->integer-bytes size 4 #f #t) o))