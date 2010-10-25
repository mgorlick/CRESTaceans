#lang racket

(require racket/tcp 
         (except-in ffi/unsafe ->)
         "../vtx/module.rkt")
(provide (all-defined-out))

; given a new connection, turn into client mode by registering function pointers
; to closures that close over the tcp listener.
(define/contract (rkt:vortex-connection-set-client-closures conn)
  (VortexConnection*? . -> . void)
  (define inp #f)
  (define outp #f)
  
  (define/contract (connect/tcp host port)
    (string? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let-values ([(in out) (tcp-connect host port)])
        (set! inp in)
        (set! outp out))
      1))
  
  (define/contract (read/tcp buffer buffer-len)
    (string? integer?  . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let ([res (read-string! buffer inp 0 buffer-len)])
        (if (eof-object? res) 0 res))))
  
  (define/contract (write/tcp buffer buffer-len)
    (string? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (write-string buffer outp 0 buffer-len)))
  
  (define/contract (close/tcp)
    (-> integer?)
    (with-handlers ([exn:fail:network? -1])
      (close-input-port inp)
      (close-output-port outp)
      1))
  
  (vortex-connection-set-client-closures connect/tcp read/tcp write/tcp close/tcp))

; given a new connection, turn it into listener mode by registering function pointers
; to closures that close over the tcp listener.
(define/contract (rkt:vortex-connection-set-listener-closures conn)
  (VortexConnection*? . -> . void)
  (define inp #f)
  (define outp #f)
  (define listener #f)
  
  (define/contract (listen/tcp host port)
    (string? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (if (eq? host #f)
          (set! listener (tcp-listen port 10000 #t))
          (set! listener (tcp-listen port 10000 host)))
      1))
  
  (define/contract (accept/tcp)
    (-> integer?)
    (with-handlers ([exn:fail:network? -1])
      (let-values ([(in out) (tcp-accept listener)])
        (set! inp in)
        (set! outp out))
      1))
  
  (define/contract (read/tcp buffer buffer-len)
    (string? integer?  . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let ([res (read-string! buffer inp 0 buffer-len)])
        (if (eof-object? res) 0 res))))
  
  (define/contract (write/tcp buffer buffer-len)
    (string? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (write-string buffer outp 0 buffer-len)))
  
  (define/contract (close/tcp)
    (-> integer?)
    (with-handlers ([exn:fail:network? -1])
      (close-input-port inp)
      (close-output-port outp)
      1))
  
  (vortex-connection-set-listener-closures listen/tcp accept/tcp read/tcp write/tcp close/tcp))