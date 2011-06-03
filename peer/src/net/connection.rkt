#lang racket/base

(require racket/match
         racket/contract
         racket/tcp
         racket/class
         data/queue
         racket/dict
         unstable/dict
         unstable/contract
         "msg.rkt"
         "channel.rkt")

(provide handle-frame)

(define the-channel (new channel% [channelno 0] [this-pk #"ABCD"] [remote-pk #"EFGH"]))

;; error handling:
;; RFC 3080 2.2.1.1-2.2.2.1.3 defines a number of error cases to look for. here's where we handle them
;; - nonterminated frame: in connection layer
;; - unknown message type: in connection layer
;; - syntactically invalid header line: in connection layer
;; - invalid channel number: in connection layer
;; - semantically invalid NUL: in connection layer
;; - everything else: in channel layer
;; the goal is to only pass on potentially invalid messages to the channel layer which need
;; channel-specific state to validate.

(define/contract (handle-frame bytes i)
  (bytes? input-port? . -> . void)
  (define (normal-msg-type? t)
    (or (equal? t #"MSG") (equal? t #"RPY") (equal? t #"ERR")))
  
  (printf "processing line: ~a~n" bytes)
  (match (regexp-split #" " bytes)
    
    #|[(list #"SEQ" ?channel ?ackno ?window)
     (printf "sequence ackno: ~a~n" ?ackno)
     #f]|#
    
    [(list #"MSG" ?channel ?msgno ?iscont ?seqno ?size)
     (printf "MSG: msgno, seqno (+ size): ~a, ~a (+ ~a)~n" ?msgno ?seqno ?size)
     (bytenums-syntax-valid? ?channel ?msgno ?seqno ?size)
     (iscont-syntax-valid? ?iscont)
     (send the-channel new-msg-frame
           (bytes->number ?msgno)
           (bytes->number ?seqno)
           (contind->boolean ?iscont)
           (read-payload/end (bytes->number ?size) i))]
    
    [(list (? normal-msg-type? ?type) ?channel ?msgno ?iscont ?seqno ?size)
     (printf "non-ans msg seqno ~a: ~a + ~a~n" ?type ?seqno ?size)
     (bytenums-syntax-valid? ?channel ?msgno ?seqno ?size)
     (iscont-syntax-valid? ?iscont)
     (read-payload/end (bytes->number ?size) i)
     (void)]
    
    [(list #"ANS" ?channel ?msgno ?iscont ?seqno ?size ?ansno)
     (printf "ANS: msg, seqno (+ size): ~a, ~a (+ ~a), ansno ~a~n" ?msgno ?seqno ?size ?ansno)
     (bytenums-syntax-valid? ?channel ?msgno ?seqno ?size ?ansno)
     (iscont-syntax-valid? ?iscont)
     (let* ([msgno (bytes->number ?msgno)]
            [seqno (bytes->number ?seqno)]
            [ansno (bytes->number ?ansno)]
            [size (bytes->number ?size)]
            [payload (read-payload/end size i)])
       (send the-channel new-ans-frame msgno seqno ansno (contind->boolean ?iscont) payload))]
    
    [(list #"NUL" ?channel ?msgno #"." ?seqno #"0")
     (bytenums-syntax-valid? ?channel ?msgno ?seqno)
     (read-end i)
     (send the-channel new-nul-frame (bytes->number ?msgno) (bytes->number ?seqno))
     (void)]
    
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