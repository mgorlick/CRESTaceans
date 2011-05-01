#lang typed/racket

(require "util.rkt"
         "packets.rkt"
         "data.rkt")

(provide makeMessage
         makeMultipart)

(define-type ContinuablePacket (U FstPacket MidPacket))
(define-type EndMultipart (Bytes -> (values LstPacket Natural Natural)))
(define-type ContinueMultipart (Rec f (Bytes -> (values ContinuablePacket f EndMultipart))))

; Q: are senders required to end a multipart before starting a new multipart or sending a singlepart?
; A: yes

;; given the last sequence number and message number used, make a packet corresponding to
;; a single (i.e., non-multipart) message. return the packet and the new last sequence and message numbers
(: makeMessage (Natural Natural Natural Bytes -> (values SinglePacket Natural Natural)))
(define (makeMessage lastSeqNo lastMsgNo destID data)
  (define thisSeq (wrappedSucc lastSeqNo 31))
  (define thisMsg (wrappedSucc lastMsgNo 29))
  (values (SinglePacket (make-timestamp) destID thisSeq thisMsg #t data) thisSeq thisMsg))

;; makeMultipart: like makeMessage, but takes a list of messages to send instead of a single message,
;; and returns a list of packets instead of a single packet. also returns new last sequence and message numbers
(: makeMultipart (Natural Natural Natural (Listof Bytes) -> (values (Listof DataPacket) Natural Natural)))
(define (makeMultipart lastSeqNo lastMsgNo destID datas)
  (define thisSeq (wrappedSucc lastSeqNo 31))
  (define thisMsg (wrappedSucc lastMsgNo 29))
  (define ordered? #t)
  
  ;; startMultipart: given the first part of a multipart message,
  ;; produces the first packet of the message plus a 
  ;; ContinueMultipart and an EndMultipart
  (: startMultipart (Bytes -> (values FstPacket ContinueMultipart EndMultipart)))
  (define (startMultipart data)
    (values (FstPacket (make-timestamp) destID thisSeq thisMsg ordered? data)
            (make-Continuable thisSeq) (make-Endable thisSeq)))
  
  ;; make-Continuable: given the last sequence number used, produces a continuation that
  ;; will produce the next packet in the middle of a multipart message,
  ;; along with a new ContinueMultipart and an EndMultipart
  (: make-Continuable (Natural -> ContinueMultipart))
  (define (make-Continuable lastSeq)
    (λ: ([next-data : Bytes])
        (define nextSeq (wrappedSucc lastSeq 31))
        (values (MidPacket (make-timestamp) destID nextSeq thisMsg ordered? next-data)
                (make-Continuable nextSeq) (make-Endable nextSeq))))
  
  ;; make-Endable: given the last sequence number, produces a continuation that
  ;; will produce the last packet of a multipart message, along with the
  ;; updated sequence and message numbers for the mutlipart message
  (: make-Endable (Natural -> EndMultipart))
  (define (make-Endable lastSeq)
    (λ: ([next-data : Bytes])
        (define nextSeq (wrappedSucc lastSeq 31))
        (values (LstPacket (make-timestamp) destID nextSeq thisMsg ordered? next-data) nextSeq thisMsg)))
  
  ;; make the first packet of the multipart message. 
  ;; then make all the middle packet, following the 
  ;; ContinueMultipart. finally, make the last packet,
  ;; following the EndMultipart.
  (let-values ([(fst cc1 ec1) (startMultipart (first datas))])
    (let: ([pkts : (Listof DataPacket) (list fst)])
    (let loop ([pkts pkts] [remaining (rest datas)] [cc cc1] [ec ec1])
      (if (empty? (rest remaining))
          (let-values ([(lst newSeqNo newMsgNo) (ec (first remaining))])
            (values (reverse (cons lst pkts)) newSeqNo newMsgNo))
          (let-values ([(mid ccn ecn) (cc (first remaining))])
            (loop (cons mid pkts) (rest remaining) ccn ecn)))))))