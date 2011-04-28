#lang racket

(require rackunit
         rackunit/text-ui
         "../packets/packets.rkt"
         "../packets/control.rkt")

(define top32 (abs (sub1 (expt 2 32))))
(define roundtrip (compose bytes->packet packet->bytes))

(define-test-suite ctrl-serialization-tests
  "Round-tripping of control packet serialization and deserialization"
  
  (test-case
   "Shutdown: No Control fields, no Additional field"
   (define sp (make-Shutdown 5 5))
   (check-equal? sp (roundtrip sp)))
  
  (test-case
   "KeepAlive: No Control fields, no Additional field"
   (define kap (make-KeepAlive top32 55555))
   (check-equal? kap (roundtrip kap)))
  
  (test-case
   "ACK2: No Control fields, present Additional field"
   (define ak2p (make-ACK2 0 33321 6821976))
   (check-equal? ak2p (roundtrip ak2p)))
  
  (test-case
   "Light ACK: No Control fields, present Additional field"
   (define lakp (make-LightACK 6553 top32 29))
   (check-equal? lakp (roundtrip lakp)))
  
  (test-case
   "NAK: Present Control fields, no Additional field"
   (define nakp (make-NAK top32 top32 top32))
   (check-equal? nakp (roundtrip nakp)))
  
  (test-case
   "Handshake: Present Control fields, no Additional field"
   (define hp1 (make-Handshake 55 0 4 'Dgram 0 top32 top32 1 55 67))
   (define hp2 (make-Handshake 0 123 0 'Stream 592395 0 1 -1 top32 top32))
   (check-equal? hp1 (roundtrip hp1))
   (check-equal? hp2 (roundtrip hp2)))
  
  (test-case
   "Medium ACK: Present Control fields, present Additional field"
   (define mackp (make-MedACK 0 2223 top32 5552 000 1123))
   (check-equal? mackp (roundtrip mackp)))
  
  (test-case
   "Full ACK: Present Control fields, present Additional field"
   (define fackp (make-FullACK 66858 5929582 124144 52828 347 23552 569691 top32 0))
   (check-equal? fackp (roundtrip fackp)))
  
  (test-case
   "Present Control fields, present Additional field"
   (define drp (make-DropReq 0 100 500 top32 65535))
   (check-equal? drp (roundtrip drp))))

(run-tests ctrl-serialization-tests)