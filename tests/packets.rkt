#! /usr/bin/env racket
#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "../packets/util.rkt"
         "../packets/packets.rkt"
         "../packets/control.rkt"
         "../packets/data.rkt")

(define ctrl-serialization-tests
  (let ([roundtrip (compose bytes->cpacket cpacket->bytes)]
        [has-min-header? (compose (negate lacks-full-header?) cpacket->bytes)])
    (test-suite
     "Round-tripping of control packet serialization and deserialization"
     
     (test-case
      "Shutdown: No Control fields, no Additional field"
      (define sp (make-Shutdown 5 5))
      (check-pred has-min-header? sp)
      (check-equal? sp (roundtrip sp)))
     
     (test-case
      "KeepAlive: No Control fields, no Additional field"
      (define kap (make-KeepAlive top32 55555))
      (check-pred has-min-header? kap)
      (check-equal? kap (roundtrip kap)))
     
     (test-case
      "ACK2: No Control fields, present Additional field"
      (define ak2p (make-ACK2 0 33321 6821976))
      (check-pred has-min-header? ak2p)
      (check-equal? ak2p (roundtrip ak2p)))
     
     (test-case
      "Light ACK: No Control fields, present Additional field"
      (define lakp (make-LightACK 6553 top32 29))
      (check-pred has-min-header? lakp)
      (check-equal? lakp (roundtrip lakp)))
     
     (test-case
      "NAK: Present Control fields, no Additional field"
      (define nakp (make-NAK top32 top32 top32))
      (check-pred has-min-header? nakp)
      (check-equal? nakp (roundtrip nakp)))
     
     (test-case
      "Handshake: Present Control fields, no Additional field"
      (define hp1 (make-Handshake 55 0 4 'Dgram 0 top32 top32 'CSReq 55 67))
      (define hp2 (make-Handshake 0 123 0 'Stream 592395 0 1 'RDVReq top32 top32))
      (define hp3 (make-Handshake 55 0 4 'Stream 0 top32 top32 'Deny 55 67))
      (define hp4 (make-Handshake 0 123 0 'Dgram 592395 0 1 'Accept top32 top32))
      (check-pred has-min-header? hp1)
      (check-pred has-min-header? hp2)
      (check-pred has-min-header? hp3)
      (check-pred has-min-header? hp4)
      (check-equal? hp1 (roundtrip hp1))
      (check-equal? hp2 (roundtrip hp2))
      (check-equal? hp3 (roundtrip hp3))
      (check-equal? hp4 (roundtrip hp4)))
     
     (test-case
      "Medium ACK: Present Control fields, present Additional field"
      (define mackp (make-MedACK 0 2223 top32 5552 000 1123))
      (check-pred has-min-header? mackp)
      (check-equal? mackp (roundtrip mackp)))
     
     (test-case
      "Full ACK: Present Control fields, present Additional field"
      (define fackp (make-FullACK 66858 5929582 124144 52828 347 23552 569691 top32 0))
      (check-pred has-min-header? fackp)
      (check-equal? fackp (roundtrip fackp)))
     
     (test-case
      "Drop Request: Present Control fields, present Additional field"
      (define drp (make-DropReq 0 100 500 top32 65535))
      (check-pred has-min-header? drp)
      (check-equal? drp (roundtrip drp)))
     )))

(define msg-serialization-tests
  (let ([roundtrip (compose bytes->dpacket dpacket->bytes)]
        [has-min-header? (compose (negate lacks-full-header?) dpacket->bytes)])
    (test-suite
     "Round-tripping of data message serialization and deserialization"
     (test-case
      "First message, ordered stream"
      (define first/ordered
        (make-DataPacket 2555 0 top31 top29 'First #t #"I had to let him go."))
      (check-pred has-min-header? first/ordered)
      (check-equal? first/ordered (roundtrip first/ordered)))
     
     (test-case
      "Middle message, ordered stream"
      (define mid/ordered
        (make-DataPacket 2555 0 0 top29 'Middle #t #"Don't disturb my friend. He's dead tired."))
      (check-pred has-min-header? mid/ordered)
      (check-equal? mid/ordered (roundtrip mid/ordered)))
     
     (test-case
      "Last message, ordered stream"
      (define last/ordered
        (make-DataPacket top32 0 top31 0 'Last #t #"Let off some steam, Bennett."))
      (check-pred has-min-header? last/ordered)
      (check-equal? last/ordered (roundtrip last/ordered)))
     
     (test-case
      "Only message, ordered stream"
      (define only/ordered
        (make-DataPacket 555123 top32 0 top29 'Only #t
                         #"I eat Green Berets for breakfast, and right now, I'm very hungry!"))
      (check-pred has-min-header? only/ordered)
      (check-equal? only/ordered (roundtrip only/ordered)))
     
     (test-case
      "First message, unordered stream"
      (define first/unordered (make-DataPacket 2324 513 top31 top29 'First #f #"No."))
      (check-pred has-min-header? first/unordered)
      (check-equal? first/unordered (roundtrip first/unordered)))
     
     (test-case
      "Middle message, unordered stream"
      (define mid/unordered (make-DataPacket 372582 65535 0 top29 'Middle #f #"Wrong!"))
      (check-pred has-min-header? mid/unordered)
      (check-equal? mid/unordered (roundtrip mid/unordered)))
     
     (test-case
      "Last message, unordered stream"
      (define last/unordered (make-DataPacket 32525 32768 top31 5555 'Last #f #"I lied."))
      (check-pred has-min-header? last/unordered)
      (check-equal? last/unordered (roundtrip last/unordered)))
     
     (test-case
      "Only message, unordered stream"
      (define only/unordered (make-DataPacket 0 4412468 0 top29 'Only #f #"I let him go."))
      (check-pred has-min-header? only/unordered)
      (check-equal? only/unordered (roundtrip only/unordered))))))

(time (run-tests ctrl-serialization-tests))
(time (run-tests msg-serialization-tests))