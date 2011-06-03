#lang racket/base

(require racket/tcp
         racket/contract
         racket/function
         "msg.rkt")

(define payload1 #"/path/to/some/actor 200 OK\r\n")
(define payload2 (mime-headers `((#"Content-Type" . #"text/plain; charset=\"utf-8\"")
                                 (#"Content-Transfer-Encoding" . #"quoted-printable"))))
(define payload3 #"Body begins here")
(define payload4 #"\r\nthis is the rest of the body.")

(define-values (i o) (tcp-connect "localhost" 5004))

(define seq 0)

(define/contract (write-frame/update-seq! beepheader . bls)
  ([rec-byteslist/c] #:rest (listof rec-byteslist/c) . ->* . void)
  (write-rec-byteslist o beepheader)
  (let ([seqchange (write-rec-byteslist o bls)])
    (write-rec-byteslist o TRAILER)
    (set! seq (+/u32 seq seqchange))))

(write-frame/update-seq! (ans-header 0 0 #t 0 (rec-byteslist-length (list payload1 payload2 payload3)) 0)
                         payload1
                         payload2
                         payload3)

(write-frame/update-seq! (ans-header 0 0 #f seq (rec-byteslist-length payload4) 0)
                         payload4)

(write-frame/update-seq! (nul-header 0 0 seq))

(write-frame/update-seq! (msg-header 0 1 #f seq (rec-byteslist-length (list payload1 #"\r\n" payload3)))
                         payload1
                         #"\r\n"
                         payload3)