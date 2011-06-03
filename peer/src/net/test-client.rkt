#lang racket/base

(require racket/tcp
         "msg.rkt")

(define payload1
  (bytes-append #"/path/to/some/actor 200 OK\r\n"
                #"Content-Type: text/plain\r\n"
                #"Content-Transfer-Encoding: plain\r\n"
                #"\r\n"
                #"Body begins here"))
(define payload2
  #"\r\nthis is the rest of the body.")

(define-values (i o) (tcp-connect "localhost" 5006))

(define seq 0)

(write-rec-byteslist o (ans-header 0 0 #t 0 (bytes-length payload1) 0))
(write-rec-byteslist o payload1)
(write-rec-byteslist o TRAILER)
(set! seq (bytes-length payload1))

(write-rec-byteslist o (ans-header 0 0 #f seq (bytes-length payload2) 0))
(write-rec-byteslist o payload2)
(write-rec-byteslist o TRAILER)
(set! seq (+ seq (bytes-length payload2)))

(write-rec-byteslist o (nul-header 0 0 seq))
(write-rec-byteslist o TRAILER)

(write-rec-byteslist o (rpy-header 0 1 #f seq (bytes-length payload1)))
(write-rec-byteslist o payload1)
(write-rec-byteslist o TRAILER)
(set! seq (+ seq (bytes-length payload1)))