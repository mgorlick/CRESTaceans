#lang racket

(require "msg.rkt")

(define/contract (handle bytes)
  (bytes? . -> . (or/c bytes? #f))
  (define (normal-msg-type? t)
    (or (equal? t #"MSG") (equal? t #"RPY") (equal? t #"ERR")))
  
  (printf "processing line: ~a~n" bytes)
  (match (regexp-split #" " bytes)
    
    [(list #"SEQ" ?channel ?ackno ?window)
     (printf "sequence ackno: ~a~n" ?ackno)
     #f]
    
    [(list (? normal-msg-type? ?type) ?channel ?msgno ?iscont ?seqno ?size)
     (printf "non-ans msg seqno ~a: ~a + ~a~n" ?type ?seqno ?size)
     (bytenums-syntax-valid? ?channel ?msgno ?seqno ?size)
     (iscont-syntax-valid? ?iscont)
     (read-payload/end (bytes->number ?size) i)]
    
    [(list #"ANS" ?channel ?msgno ?iscont ?seqno ?size ?ansno)
     (printf "ans msg seqno: ~a + ~a, ansno ~a~n" ?seqno ?size ?ansno)
     (bytenums-syntax-valid? ?channel ?msgno ?seqno ?size ?ansno)
     (iscont-syntax-valid? ?iscont)
     (read-payload/end (bytes->number ?size) i)]
    
    [(list #"NUL" ?channel ?msgno #"." ?seqno #"0")
     (bytenums-syntax-valid? ?channel ?msgno ?seqno)
     (read-end i)
     #f]
    
    ;; RFC 3080 2.2.1.1 error cases
    ;; for the NUL cases: try to recover by throwing away the useless data 
    ;; so it doesn't clog other channels. THEN throw the error
    [(list #"NUL" _ _ #"*" _ ?size)
     (bytenums-syntax-valid? ?size)
     (read-payload/end (bytes->number ?size) i)
     (raise-frame-warning "NUL message found with intermediate continuation indicator")]
    [(list #"NUL" _ _ _ _ ?size)
     (bytenums-syntax-valid? ?size)
     (read-payload/end (bytes->number ?size) i)
     (raise-frame-warning (format "NUL message found with non-zero size specification: ~a" ?size))]
    [(list some-other-type ...)
     (raise-frame-warning (format "header message type not recognized: ~a" some-other-type))]))

(define tcp (tcp-listen 5000))
(define-values (i o) (tcp-accept tcp))

(let loop ()
  (let ([bytes (read-bytes-line i 'return-linefeed)])
    (with-handlers ([exn:fail:beep:frame? (λ (e) (printf "error: ~a~n" e))])
      (handle bytes))
    (loop)))