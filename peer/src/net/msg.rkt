#lang racket/base

(require racket/contract
         racket/function
         racket/dict
         racket/provide
         unstable/contract
         unstable/dict
         "modular-arithmetic.rkt")

(struct response (beeptype url status-code status-msg mime headers remotepk payload) #:transparent)

(provide/contract
 [struct response ([beeptype bytes?] ; e.g., #"ANS" this could be one-of/c but it's easier to extend this way
                   [url bytes?]
                   [status-code number?] ; e.g., 200
                   [status-msg bytes?] ; e.g., #"OK"
                   [mime bytes?]
                   [headers (hash/c bytes? bytes?)]
                   [remotepk bytes?] ; public key of the sender
                   [payload bytes?])])

(provide (except-out (all-defined-out)
                     frame-header
                     struct:response
                     (matching-identifiers-out #rx"^response*" (all-defined-out))))

(define-modular-arithmetic 32 #f)
(define-modular-arithmetic 31 #f)
(define rec-byteslist/c (or/c bytes? (recursive-contract (listof rec-byteslist/c))))

(define number->bytes (compose string->bytes/utf-8 number->string))
(define bytes->number (compose string->number bytes->string/utf-8))
(define (contind->boolean i)
  (if (equal? i CONTINUEMSG)
      #t
      #f))


; RFC 3080 section 2.2.1.1 constants
(define SP #" ")
(define COLON #": ")
(define CONTINUEMSG #"*")
(define FINISHMSG #".")
(define TRAILER #"END\r\n")
; RFC 2045 section 2.1 constants
(define CRLF #"\r\n")

;; -------
;; SENDING
;; -------

(define/contract (seq-header channelno ackno window)
  (uint31/c uint32/c uint31/c . -> . rec-byteslist/c)
  (list #"SEQ" SP (number->bytes channelno) SP (number->bytes ackno) SP (number->bytes window) CRLF))

(define/contract (nul-header channelno msgno seqno)
  (uint31/c uint31/c uint32/c . -> . rec-byteslist/c)
  (list #"NUL" SP (number->bytes channelno) SP (number->bytes msgno) SP FINISHMSG SP (number->bytes seqno) SP #"0" CRLF))

(define/contract (ans-header channelno msgno more? seqno size ansno)
  (uint31/c uint31/c boolean? uint32/c uint31/c uint31/c . -> . rec-byteslist/c)
  (list #"ANS" SP (number->bytes channelno) SP (number->bytes msgno) SP
        (if more? CONTINUEMSG FINISHMSG) SP (number->bytes seqno) SP
        (number->bytes size) SP (number->bytes ansno) CRLF))

(define/contract (frame-header mode channelno msgno more? seqno size)
  ((one-of/c 'rpy 'err 'msg) uint31/c uint31/c boolean? uint32/c uint31/c . -> . rec-byteslist/c)
  (define modebytes (case mode ['rpy #"RPY"] ['err #"ERR"] ['msg #"MSG"]))
  (list modebytes SP (number->bytes channelno) SP (number->bytes msgno) SP
        (if more? CONTINUEMSG FINISHMSG) SP (number->bytes seqno) SP
        (number->bytes size) CRLF))

(define msg-header (curry frame-header 'msg))
(define rpy-header (curry frame-header 'rpy))
(define err-header (curry frame-header 'err))

(define/contract (mime-headers hds)
  ((dict/c bytes? bytes?) . -> . rec-byteslist/c)
  (for/fold ([lst (list CRLF)])
    ([(k v) (in-dict hds)])
    (cons (list k COLON v CRLF) lst)))

(define/contract (rec-byteslist-length bstr-or-list)
  (rec-byteslist/c . -> . exact-nonnegative-integer?)
  (cond [(list? bstr-or-list) (foldl + 0 (map rec-byteslist-length bstr-or-list))]
        [(bytes? bstr-or-list) (bytes-length bstr-or-list)]))


;; write the representation produced by `frame-header' to the output port.
;; return a count of the bytes written.
(define/contract (write-rec-byteslist oport bstr-or-list)
  (output-port? rec-byteslist/c . -> . exact-nonnegative-integer?)
  (define (w* oport bstr-or-list)
    (cond [(list? bstr-or-list) (foldl + 0 (map (curry w* oport) bstr-or-list))]
          [(bytes? bstr-or-list) (write-bytes bstr-or-list oport)]))
  (let ([ct (w* oport bstr-or-list)])
    (flush-output oport)
    ct))

;; ---------
;; RECEIVING
;; ---------

(struct exn:fail:beep:frame exn:fail:network ())
(struct exn:fail:beep:message exn:fail:network ())

(define/contract (raise-frame-warning a)
  (string? . -> . void)
  (raise (exn:fail:beep:frame (format "malformed frame warning: ~a" a) (current-continuation-marks))))

(define/contract (raise-message-warning a)
  (string? . -> . void)
  (raise (exn:fail:beep:message (format "malformed message warning: ~a" a) (current-continuation-marks))))

;; utilities for reading & parsing received frames

;; check to make sure that the numeric bitstring tokens are valid
;; note: doesn't check for the specific bit-size-based value constraints
(define/contract (bytenums-syntax-valid? . nums) ; RFC 3080 2.2.1.1 error
  ([] #:rest (listof bytes?) . ->* . void)
  (if (andmap (curry regexp-match-exact? #"[0-9]+") nums)
      (void)
      (raise-frame-warning "parameters in header are syntactically incorrect")))

;; check to make sure that the continuation indicator token is one of two
;; note: still need to check validity for specific message & channel state
(define/contract (iscont-syntax-valid? v) ; RFC 3080 2.2.1.1 error
  (bytes? . -> . void)
  (if (or (equal? FINISHMSG v) (equal? CONTINUEMSG v))
      (void)
      (raise-frame-warning "continuation indicator in header is syntactically incorrect")))

;; read the given payload size. then try to find the trailer sequence
;; if the trailer sequence was found, return the payload.
;; throws exn:fail:beep:frame if invalid
(define/contract (read-payload/end size i)
  (exact-nonnegative-integer? input-port? . -> . bytes?)
  (let ([payload (read-bytes size i)])
    (when (read-end i)
      payload))) ; RFC 3080 2.2.1.3

;; try to find the trailer sequence. 
(define/contract (read-end i) 
  (input-port? . -> . void)
  (define trailer (read-bytes (bytes-length TRAILER) i))
  (if (bytes=? TRAILER trailer)
      (void)
      (raise-frame-warning (format "trailer sequence not found, got ~a instead" trailer))))