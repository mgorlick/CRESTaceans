#lang racket/base

(require racket/contract racket/list)

(require rackunit
         "depends.rkt"
         "crypto.rkt"
         
         "../peer-validation/scurl.rkt"
         "../peer-validation/scurl-test.rkt")

; Setup test state.
(define cipher-type cipher:aes-256)
(define digest-type digest:sha256)
(define ksize (cipher-key-length cipher-type))
(define hksize (/ ksize 2))
(define ivsize (cipher-iv-length cipher-type))
(define hivsize (/ ivsize 2))
(define skeys (generate-shared-keys cipher-type))
(define skeys2 (generate-shared-keys cipher-type))
(define sess1 (create-session cipher:aes-256 digest:sha256 skeys skeys2))
(define sess2 (create-session cipher:aes-256 digest:sha256 skeys skeys2))

(define 23bm (make-bytes 23 43))
(define block-size 32)
(define pad-needed (- block-size (bytes-length 23bm)))
(define 32bm (make-bytes 32 56))
(define key (random-bytes (cipher-key-length cipher-type)))
(define iv (random-bytes (cipher-iv-length cipher-type)))
(define sess (create-session cipher-type
                             digest-type
                             (generate-shared-keys cipher-type)
                             (generate-shared-keys cipher-type)))
(define msg1 #"H")
(define msg2 #"The cat is wiggling!")
(define msg3 #"Fazzle, Dazzle, Foo!")
(define msg4 #"Chupakabra...")
(define msg5 #"STANDARDS OF EXCELLENCE!")
(define nopad-msg (make-bytes (cipher-block-size cipher-type) 200))
(define pkmsg (make-bytes (pkey-size (scurl-key scurl3-full)) 5))
(define pkmsgm1 (make-bytes (- (pkey-size (scurl-key scurl3-full)) 1) 5))
(define pkmsgp1 (make-bytes (+ (pkey-size (scurl-key scurl3-full)) 1) 5))

; Define byte-string manipulation functions.
; Change first byte in message.
(define (change-1st-byte data)
  (bytes-set! data 0 1)
  data)

; append bytes on the end.
(define (append-end-bytes data)
  (bytes-append
   data
   (random-bytes (cipher-block-size cipher-type))))

; inject bytes in the middle.
(define (append-middle-bytes data)
  (bytes-append
   (subbytes data 0 5)
   (random-bytes (cipher-block-size cipher-type))
   (subbytes data 5)))

; append bytes in the beginning.
(define (append-begin-bytes data)
  (bytes-append
   (random-bytes (cipher-block-size cipher-type))
   data))

; Main crypto test
(define crypto-test
  (test-suite
   "Tests for crypto.rkt"
   
   (test-case
    "Test that the sizes of the shared-keys are correct."
    
    (check-true (shared-keys? skeys)
                "The shared-keys failed creation.")
    (check-true (and
                 (= (bytes-length (shared-keys-kcs skeys)) hksize)
                 (= (bytes-length (shared-keys-ksc skeys)) hksize))
                "kcs and ksc are not the correct sizes.")
    (check-true (and
                 (= (bytes-length (shared-keys-ivcs skeys)) hivsize)
                 (= (bytes-length (shared-keys-ivsc skeys)) hivsize))
                "ivcs and ivsc are not the correct sizes."))
   
   (test-case
    "Test that two shared-keys are different."
    
    (check-true (shared-keys? skeys2)
                "The shared-keys failed creation.")
    (check-false (or
                  (bytes=? (shared-keys-kcs skeys) (shared-keys-kcs skeys2))
                  (bytes=? (shared-keys-ksc skeys) (shared-keys-ksc skeys2))
                  (bytes=? (shared-keys-ivcs skeys) (shared-keys-ivcs skeys2))
                  (bytes=? (shared-keys-ivsc skeys) (shared-keys-ivsc skeys2)))
                 "At least one component of two different shared-keys structures is the same."))
   
   ; Test crypto.
   (test-case 
    "Testing the msg-length functions."
    
    (check-equal? (msg-length 23bm)
                  (bytes 0 0 0 23)
                  "The msg-length function failed to return a 4 byte string where the right-most byte is 23.")
    (check-equal? (bytes->msg-length (msg-length 23bm))
                  (bytes-length 23bm)
                  "The msg-length then bytes->msg-length functions do not yield the same value as the input."))
   
   (test-case
    "Testing add-padding and remove-padding"
    
    (check-equal? (bytes-length (add-padding 23bm block-size))
                  block-size
                  "The add-padding failed to add the correct amount of padding.")
    (check-equal? (add-padding 23bm block-size)
                  (bytes-append 23bm (make-bytes pad-needed pad-needed))
                  "The add-padding failed to add padding as expected.")
    (check-equal? (bytes-length (remove-padding (add-padding 23bm block-size) block-size))
                  (bytes-length 23bm) 
                  "The remove-padding failed to remove the correct amount from the given data."))
   
   (test-case
    "Testing add-padding and remove-padding on boundary cases"
    (check-equal? (bytes-length (add-padding 32bm block-size))
                  (+ block-size (bytes-length 32bm))
                  "add-padding failed to add an extra block of padding.")
    (check-equal? (bytes-length (remove-padding (add-padding 32bm block-size) block-size))
                  (bytes-length 32bm)
                  "remove-padding failed to remove the extra block of padding.")
    (check-equal? (remove-padding (add-padding 32bm block-size) block-size)
                  32bm
                  "add-padding and then remove-padding failed to yield the same input."))
   
   (test-case
    "Testing msg/encrypt! and msg/decrypt!"
    
    (check-equal? (msg/decrypt! sess (msg/encrypt! sess msg1)) msg1 "msg1 failed msg/encrypt!->msg/decrypt!.")
    (check-equal? (msg/decrypt! sess (msg/encrypt! sess msg2)) msg2 "msg2 failed msg/encrypt!->msg/decrypt!.")
    (check-equal? (msg/decrypt! sess (msg/encrypt! sess msg3)) msg3 "msg3 failed msg/encrypt!->msg/decrypt!.")
    (check-equal? (msg/decrypt! sess (msg/encrypt! sess msg4)) msg4 "msg4 failed msg/encrypt!->msg/decrypt!.")
    (check-equal? (msg/decrypt! sess (msg/encrypt! sess msg5)) msg5 "msg5 failed msg/encrypt!->msg/decrypt!.")
    (check-equal? (msg/decrypt! sess (msg/encrypt! sess nopad-msg)) nopad-msg "nopad-msg failed msg/encrypt!->msg/decrypt!."))
   
   (test-case
    "Testing modification of a byte in the message between msg/encrypt! and msg/decrypt!."
    
    (check-false (msg/decrypt! sess (change-1st-byte (msg/encrypt! sess msg1)))
                 "Changed the first byte and it passed decryption.")
    (check-false (msg/decrypt! sess (append-end-bytes (msg/encrypt! sess msg2)))
                 "Added extra bytes at the end of the message and it passed decryption.")
    (check-false (msg/decrypt! sess (append-middle-bytes (msg/encrypt! sess msg3)))
                 "Added extra bytes in the middle of the message and it passed decryption.")
    (check-false (msg/decrypt! sess (append-begin-bytes (msg/encrypt! sess msg4)))
                 "Added extra bytes at the beginning of the message and it passed decryption."))
   
   (test-case
    "Test assymetric encryption/decryption"
    
    (check-equal? (msg/decrypt/pkey scurl3-full (msg/encrypt/pkey scurl3-full msg1)) msg1 
                  "msg1 failed msg/encrypt/pkey->msg/decrypt/pkey")
    (check-equal? (msg/decrypt/pkey scurl3-full (msg/encrypt/pkey scurl3-full msg2)) msg2 
                  "msg2 failed msg/encrypt/pkey->msg/decrypt/pkey")
    (check-equal? (msg/decrypt/pkey scurl3-full (msg/encrypt/pkey scurl3-full msg3)) msg3 
                  "msg3 failed msg/encrypt/pkey->msg/decrypt/pkey")
    (check-equal? (msg/decrypt/pkey scurl3-full (msg/encrypt/pkey scurl3-full msg4)) msg4 
                  "msg4 failed msg/encrypt/pkey->msg/decrypt/pkey")
    (check-equal? (msg/decrypt/pkey scurl3-full (msg/encrypt/pkey scurl3-full msg5)) msg5 
                  "msg5 failed msg/encrypt/pkey->msg/decrypt/pkey")
    
    (check-equal? (msg/decrypt/pkey scurl3-full (msg/encrypt/pkey scurl3-full pkmsg)) pkmsg 
                  "pkmsg failed msg/encrypt/pkey->msg/decrypt/pkey")
    (check-equal? (msg/decrypt/pkey scurl3-full (msg/encrypt/pkey scurl3-full pkmsgm1)) pkmsgm1 
                  "pkmsgm1 failed msg/encrypt/pkey->msg/decrypt/pkey")
    (check-equal? (msg/decrypt/pkey scurl3-full (msg/encrypt/pkey scurl3-full pkmsgp1)) pkmsgp1 
                  "pkmsgp1 failed msg/encrypt/pkey->msg/decrypt/pkey")
    
    (check-equal? (msg/decrypt/pkey scurl3-full (msg/encrypt/pkey scurl3-full-public msg1)) msg1
                  "msg1 failed msg/encrypt/pkey with scurl3-full-public and msg/decrypt/pkey with scurl3-full")
    (check-equal? (msg/decrypt/pkey scurl3-full (msg/encrypt/pkey scurl3-full-public msg2)) msg2 
                  "msg2 failed msg/encrypt/pkey with scurl3-full-public and msg/decrypt/pkey with scurl3-full"))
   
   (test-case
    "Testing modification of a byte in the message between msg/encrypt/pkey and msg/decrypt/pkey."
    
    (check-false (msg/decrypt/pkey scurl3-full (change-1st-byte (msg/encrypt/pkey scurl3-full msg1)))
                 "Changed the first byte and it passed decryption.")
    (check-false (msg/decrypt/pkey scurl3-full (append-end-bytes (msg/encrypt/pkey scurl3-full msg2)))
                 "Added extra bytes at the end of the message and it passed decryption.")
    (check-false (msg/decrypt/pkey scurl3-full (append-middle-bytes (msg/encrypt/pkey scurl3-full msg3)))
                 "Added extra bytes in the middle of the message and it passed decryption.")
    (check-false (msg/decrypt/pkey scurl3-full (append-begin-bytes (msg/encrypt/pkey scurl3-full msg4)))
                 "Added extra bytes at the beginning of the message and it passed decryption."))
   ))

; Provide everything.
(provide (all-defined-out))

; Uncomment to run this test only.
;(require rackunit/text-ui)
;(run-tests crypto-test)
