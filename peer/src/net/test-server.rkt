#lang racket/base

(require racket/tcp
         "msg.rkt"
         "connection.rkt")

(define tcp (tcp-listen 50000))
(define-values (i o) (tcp-accept tcp))

(let loop ()
  (with-handlers ([exn:fail:beep:frame? (λ (e) (printf "error: ~a~n" e))])
    (let ([bytes (read-bytes-line i 'return-linefeed)])
      (if (bytes? bytes) 
          (handle-frame bytes i)
          (raise-frame-warning "found eof"))))
  (loop))