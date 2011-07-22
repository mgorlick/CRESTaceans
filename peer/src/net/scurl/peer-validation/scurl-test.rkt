#lang racket/base

(require racket/contract racket/list)

(require rackunit
         "depends.rkt"
         
         "host-id.rkt"
         "scurl.rkt"
         ; Need crypto util for hex function
         (planet vyzo/crypto/util))

(define
  scurl1-url-text
  "http://www.amazon.com:8002/scurl/23456789abcdefghijkmnpqrstuvwxyz/index.html")

(define
  scurl1-host-id-string
  "23456789abcdefghijkmnpqrstuvwxyz")

(define
  scurl1-host-id-bytes
  (bytes 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))

(define
  scurl1-null
  (string->scurl scurl1-url-text))

(define
  scurl2-url-text
  "http://www.ebay.com:4444/scurl/vefvsv5wd4hz9isc3rb2x648ish742ha/shopping/cart.html")

(define
  scurl2-host-id-bytes
  (bytes 27 12 13 27 24 27 3 28 11 2 15 31 7 16 24 10 1 23 9 0 29 4 2 6 16 24 15 5 2 0 15 8))

(define
  scurl2-null
  (string->scurl scurl2-url-text))

(define
  scurl3-url-text
  "http://www.amazon.com:8002/scurl/sa7d8g3xunep38fvjy65r7zdhw59zua8/index.html")

(define
  scurl3-pkey
  (bytes->private-key pkey:rsa #"0\202\2[\2\1\0\2\201\201\0\241Dx\342U\230]\e\261\377I\275n\n\373,5\267\305\251R\203\242\211F\200\205\314\303\30[n\261\211\267\363\311\247\251\v\6\330>\326\21\302\355\366\210\32X\344\357\241T\226\275\263\3118\370\300mV\316\257w\233\303\356\20+\347Dz\227NG{\321L\217\334\347\24\242p\233\255\34\216\266\4\270E\355\361>J\224\vJU\360\r\203\224\234\353)i\316\337a\372\353]\5\216f\312\254\337s\317\377\5{\2\3\1\0\1\2\201\200A\f\200\2023*\201,\1\261J\3129\17\376\336\207PxOc4+\244\231\246\217\321\327D\4\345r\201m\214\315cn\201?\"\214\276\6n\rTj\363\315\265\222\237\r\242\274$?\337V\0370:\266\214]V\344\201e\230\320\301\316\321\260\305\253)\373\275\245\370\17p6\22\a\262n\24\204\274n\207\234Td\333y\201\317\264d`\250>\322\260Yf\22b\361LNX=\257\273\257.\251X5\332\251\2A\0\325\a\351\\\30\22.F!\357?\337\200\330!\360#r\236A\331\303\232T\231=\247\201b\353\302\231\321O\316=\213\231 \217\21\202pa\5\340\372\314^\214\264\354\333{b\243u*\177*.\204\277\37\2A\0\301\313\262\253\21\340\340\340\357]\236\263\251`\241\277\263\356\35g\22\301\223\212\276.Z\232)\210:Eg\375\274\204\21\234$\272\327\267\260\351\"\274\235\1\\\323\315\362\200K|:\215@\200VE\21\332%\2@\177\360+n\372\251\330\216\230\f\266\240\205!\310h\16\361\344'\212\37\326\f\336\247Ot\3762\267\1g\332t\3633fxNS\346\261\225#Q\265m\215\217\25\215vW\b\253@O\220e\205\340\205c\2@{6\n\3O\233\234\376\5\321\214\354\a\35#\331a\t[\362Co\205a\251\246\210\376\b\267\316\354\177\300G\321\343\354UA\341\222\255L\234D4\302@\5}\243\327C\246\331\a#b4\3676\361m\2@&\211a\332\274\304\313\200*kI\267\"\354\332\322\352\201Z\346\247\346\225\303hr\e\353\212\277\263\222\227\244\216Y\256\371\233\351\5n\r\bH\317\243\3n\243\225\340rd\3367\n.!_\334@\32\254"))

(define
  scurl3-expected-host-id-in-hex
  ; Calculated offline
  (subbytes #"c20ab3383dd5195099bb8f883b97eb7f067fe906ba5f4aeb06a0afe4fd76cf72"
            0 
            (* (host-id-bytes-length) 2)))

(define
  scurl3-full
  (scurl (string->url scurl3-url-text)
         (host-id-string->host-id-bytes
          "sa7d8g3xunep38fvjy65r7zdhw59zua8")
         digest:sha256
         pkey:rsa scurl3-pkey))

(define
  scurl3-full-public
  (scurl->public-scurl scurl3-full))

(define
  scurl4-url-text
  "http://www.ebay.com:4444/scurl/hr9vnr77s8phpsfeeicy4uveki6jup6r/shopping/cart.html")

(define
  scurl4-pkey
  (bytes->private-key pkey:rsa #"0\202\1<\2\1\0\2A\0\307G<\217\326!X\276t\345\30\270\376/\235!\276J5\303\324\323\330\30[\235\366\373\240\0162\207U\247\301B\2\333\362\321lk]\320W>\361VZ:\36\2262\237\a\5\":D\223\303\233\374\223\2\3\1\0\1\2A\0\200R\r\334m=q\340\312s\17\225h\20\216\365@\214\226\314p\212\255w\31\337\301?\314\223\367\322l\340u\312\370\261;\234Q\266\257\341\306\230\354\334@z\20\e}\0\354\252)\203\263M\3341\203a\2!\0\343?\17\302\27\270i\6\31\22\314$\35\366\270S(\276\377\177\343n\332\222\253\324\3\5\263y\221\3\2!\0\340~<\210\326\305\370\243\201I\n\300]\341\240\324\26N\371^\304\375\315F\34\202\337\30\324\237i1\2 {\2565\320h\202\230\276\225\344\3620r\22v\355\343\316XV\355\234\306\337o\37\273\351]n;\253\2!\0\276\262\235K\204^!k\v,\367\344\27\e\316\31\273e\224\261\253W\t\243\320\36_\323`\362\225Q\2!\0\327\342\341fp\37\334ma\363\315%E\32\331\327\253\31\363qxw\250\331#\363\207\22O\357\260x"))

(define
  scurl4-full
  (scurl (string->url scurl4-url-text)
         (host-id-string->host-id-bytes
          "hr9vnr77s8phpsfeeicy4uveki6jup6r")
         digest:sha256
         pkey:rsa scurl4-pkey))

(define
  scurl5-generated
  (generate-scurl "http://www.amazon.com:3456/index.html" digest:sha256 pkey:rsa (generate-key pkey:rsa 1024)))

(define
  scurl6-generated
  (generate-scurl "http://www.ebay.au/blag/today" digest:sha256 pkey:rsa (generate-key pkey:rsa 256)))

; Main scurl test.
(define scurl-test
  (test-suite
   "Tests for scurl.rkt"
   
   ; Test scurl creation.
   (test-case 
    "Test scurl creation."
    
    (check-true (scurl? scurl1-null) "scurl1-null is not a valid scurl.")
    (check-true (scurl? scurl2-null) "scurl2-null is not a valid scurl.")
    (check-true (scurl? scurl3-full) "scurl3-full is not a valid scurl.")
    (check-true (scurl? scurl3-full-public) "scurl3-full-public is not a valid scurl.")
    (check-true (scurl? scurl4-full) "scurl4-full is not a valid scurl.")
    (check-true (scurl? scurl5-generated) "scurl5-generated is not a valid scurl.")
    (check-true (scurl? scurl6-generated) "scurl6-generated is not a valid scurl.")
    
    )
   
   (test-case
    "Test host-id operations used in scurl creation."
    
    (check-equal? (scurl-host-id scurl1-null)
                  (compress-host-id scurl1-host-id-bytes)
                  "The host-id in the scurl is not the same as compressing the manually input byte string.")
    (check-equal? (expand-host-id (scurl-host-id scurl1-null))
                  scurl1-host-id-bytes
                  "The host-id in the scurl when expanded is not the same as the manually input byte string.")
    (check-equal? (host-id-string->host-id-bytes scurl1-host-id-string)
                  (scurl-host-id scurl1-null)
                  "The manually input string to bytes of the host-id is not the same as the SCURL host-id.")
    (check-equal? (scurl-host-id scurl2-null)
                  (compress-host-id scurl2-host-id-bytes)
                  "The host-id in scurl2-null is not the same as compressing the scurl2-host-id-bytes.")
    (check-equal? (compress-host-id scurl1-host-id-bytes)
                  (find-host-id (url-path (string->url scurl1-url-text)))
                  "The scurl1-host-id-bytes is not that same as calling find-host-id on the url text.")
    (check-equal? "http://www.amazon.com:8002/scurl/23456789abcdefghijkmnpqrstuvwxyz/index.html"
                  (url->string (insert-host-id (string->url "http://www.amazon.com:8002/index.html")
                                               "23456789abcdefghijkmnpqrstuvwxyz"))
                  "insert-host-id failed to correctly insert a host-id in string form.")
    (check-equal? (make-host-id scurl3-url-text (scurl-digest-type scurl3-full) scurl3-pkey)
                  (host-id-string->host-id-bytes "sa7d8g3xunep38fvjy65r7zdhw59zua8")
                  "make-host-id failed to correctly create the expected host-id in byte form.")
    )
   
   (test-case
    "Test scurl operations."
    
    (check-true (null-scurl? scurl1-null) "scurl1-null should be a null-scurl.")
    (check-true (null-scurl? scurl2-null) "scurl2-null should be a null-scurl.")
    (check-false (null-scurl? scurl3-full) "scurl3-full should not be a null-scurl.")
    
    (check-true (private-scurl? scurl3-full) "scurl3-full should be a private-scurl.")
    (check-false (private-scurl? scurl3-full-public) "scurl3-full-public should not be a private-scurl.")
    
    (check-equal? "http://www.amazon.com:8002/index.html"
                  (url->string
                   (scurl->url-without-host-id scurl1-null))
                  "scurl->url-without-host-id does not work as expected.")
    (check-true (scurl=? scurl1-null scurl1-null) "scurl=? failed.")
    
    (check-true (scurl=? scurl2-null scurl2-null) "scurl=? of scurl2-null and scurl2-null failed.")
    (check-true (scurl=? scurl1-null scurl1-null) "scurl=? of scurl1-null and scurl2-null failed.")
    (check-false (scurl=? scurl1-null scurl2-null) "scurl=? of scurl1-null and scurl2-null passed!")
    
    (check-true (scurl=? scurl3-full (string->scurl scurl3-url-text))  "Test that the scurl is correct.")
    (check-equal? (scurl-key scurl3-full) scurl3-pkey "Test that the pkey is correct.")
    
    (check-true (scurl=? scurl3-full-public (string->scurl scurl3-url-text)) "string->scurl of scurl3-url-text is not correct.")
    (check-true (pkey? (scurl-key scurl3-full-public)) "scurl3-full-public key is not a pkey?")
    (check-false (pkey-private? (scurl-key scurl3-full-public)) "scurl3-full-public key is a private key!")
    
    ; The hex value of hid3 should be equal to what was manually inputed above.
    (check-equal? (hex (scurl-host-id scurl3-full)) scurl3-expected-host-id-in-hex
                  "scurl3-expected-host-id is incorrect for scurl3-full.")
    )))

; Provide everything.
(provide (all-defined-out))

; Uncomment to run this test from this file.
;(require rackunit/text-ui)
;(run-tests scurl-test)