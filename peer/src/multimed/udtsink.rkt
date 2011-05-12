#lang typed/racket

(require "util-types.rkt"
         "udp-types.rkt"
         "structs.rkt"
         "../../../libs/rudt/packets/module.rkt")

(require/typed "util.rkt"
               [receive-killswitch/whatever ((Any -> Boolean) -> (U FrameBuffer Bytes Symbol))])

(provide make-udt-writer)

(: make-udt-writer (Thread String Exact-Nonnegative-Integer -> (-> Void)))
(define (make-udt-writer signaller remote-host remote-port)
  (let ([socket (udp-open-socket #f #f)]
        [is-signaller? (make-thread-id-verifier signaller)]
        [storage (make-bytes (* 1024 1024))])
    (λ ()
      (let: loop : Void ([lstSeqNo : Natural 0] [lstMsgNo : Natural 0])
        (match (receive-killswitch/whatever is-signaller?)
          [(? die? sig)
           (reply/state-report signaller #f)
           (udp-close socket)]
          
          [(FrameBuffer buffer size λdisposal)
           (let-values ([(pkt seqNo msgNo) (makeMessage lstSeqNo lstMsgNo 0 #"")])
             (define written (packet->bytes pkt storage)) ; write the header
             (bytes-copy! storage 16 buffer 0 size) ; write the body
             (λdisposal)
             (with-handlers ([exn:fail? (λ (e) (printf "Error ~a: packet size is ~a~n" e (bytes-length buffer)))])
               (udp-send-to socket remote-host remote-port storage 0 (+ size written)))
             (loop seqNo msgNo))]
          
          [(? bytes? buffer)
           (let-values ([(pkt seqNo msgNo) (makeMessage lstSeqNo lstMsgNo 0 buffer)])
             (with-handlers ([exn:fail? (λ (e) (printf "Error ~a: packet size is ~a~n" e (bytes-length buffer)))])
               (define written (packet->bytes pkt storage))
               (udp-send-to socket remote-host remote-port storage 0 written))
             (loop seqNo msgNo))]
          )))))