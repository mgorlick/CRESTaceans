#lang racket/base

(require racket/list
         racket/match
         racket/contract
         "msg.rkt")

(provide payload->response)

;; quick 'n' dirty protocol header parsing.

(define CRLF-rx #rx#"\r\n")
(define hd-bd-delimit-rx #rx#"\r\n\r\n") 
(define hdr-rx #rx#"^.*: .*$")
(define :-rx #rx#": ")
(define sp-rx #rx#" ")

;; the "main" application logic parsing locus. the rest of the functions here
;; are private helpers so this code doesn't look _too_ disgusting.
(define/contract (payload->response beeptype rpk txt)
  (bytes? bytes? bytes? . -> . response?)
  ; restrict our future parsing to the header only - so we don't find \r\n in body
  (let ([where-CRLF (regexp-match-positions hd-bd-delimit-rx txt)])
    (cond
      [(cons? where-CRLF)
       (define rns (regexp-match-positions* CRLF-rx txt 0 (cdr (first where-CRLF))))
       (define-values (url code msg) (status-line->elements txt (car (first rns))))
       (define headers
         (set-defaults
          ;; this is really sort of a 3-arity unfold, but this ugly code works for now
          (let loop ([heads #hash()] [last (first rns)] [posns (rest rns)])
            (define next (first posns))
            (define start (cdr last))
            (define end (car next))
            (cond [(regexp-match? hdr-rx txt start end)
                   (let-values ([(k v) (header-line->elements txt start end)])
                     (loop (hash-set heads k v) next (rest posns)))]
                  [else heads]))))
       
       (response beeptype url code msg
                 (hash-ref headers #"Content-Type") (hash-remove headers #"Content-Type")
                 rpk (subbytes txt (cdr (first where-CRLF))))]
      [else (raise-message-warning "no delimit between head and body found")])))

(define/contract (status-line->elements txt end)
  (bytes? number? . -> . (values bytes? number? bytes?))
  (let ([spaces (regexp-match-positions* sp-rx txt 0 end)])
    (if (>= (length spaces) 2)
        (values (subbytes txt 0 (car (first spaces)))
                (bytes->number (subbytes txt (cdr (first spaces)) (car (second spaces))))
                (subbytes txt (cdr (second spaces)) end))
        (raise-message-warning "malformed status line"))))

(define/contract (header-line->elements txt start end)
  (bytes? number? number? . -> . (values bytes? bytes?))
  (match (regexp-split :-rx txt start end)
    [(list k v) (values k v)]
    [_ (raise-message-warning "malformed header")]))

;; for each default header key, if no existing value is specified,
;; adds the default value. see RFC 3080
(define default-headers `((#"Content-Type" . #"application/octet-stream")
                          (#"Content-Transfer-Encoding" . #"binary")))
(define/contract (set-defaults headers)
  ((hash/c bytes? bytes?) . -> . (hash/c bytes? bytes?))
  (define (add-unused-entry kv h)
    (if (hash-has-key? h (car kv))
        h
        (hash-set h (car kv) (cdr kv))))
  (foldl add-unused-entry headers default-headers))

#|
(define txt1 #"/clanA/actorB 299 OK to Party!\r\nContent-Language: en-us\r\nContent-Transfer-Encoding: base64\r\nContent-Type: crest/mischief-script\r\n\r\n\r\n bodybodybody")
(define txt2 #"/clanC/actorZ/some;distinguished/path 333 Moved to Jamaica\r\n\r\n")
(payload->response #"ANS" #"abcdefg" txt1)
(payload->response #"ERR" #"12456" txt2)|#