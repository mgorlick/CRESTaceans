#lang racket/base

(require racket/function
         racket/list
         "message.rkt"
         "../net/tcp-peer.rkt"
         "../../../Motile/baseline.rkt"
         "../../../Motile/compile/compile.rkt"
         "../../../Motile/generate/baseline.rkt"
         (planet "uuid-v4.rkt" ("zitterbewegung" "uuid-v4.plt" 2 0)))

(provide (all-defined-out)
         (all-from-out 
          "message.rkt"
          "../net/tcp-peer.rkt"
          "../../../Motile/baseline.rkt"   
          "../../../Motile/compile/compile.rkt"
          "../../../Motile/generate/baseline.rkt"))

(define (no-return)
  (semaphore-wait (make-semaphore)))

; argsassoc: string [call: string -> any] [no-val: any] [default: any] -> any
; separates the provided command line arguments of the form:
;             --key1=val1 --key2=val2 --key3
; if the provided key name is present and has a value, returns `call' applied to that value.
; if the provided key name is present but has no value, returns `default'.
; if the provided key name is not present, returns `no-val'.
(define CLargs (map (curry regexp-split #rx"=") (vector->list (current-command-line-arguments))))
(define (argsassoc key #:call [call (λ (x) x)] #:no-val [no-val #f] #:default [default #t])
  (let ([entry (assoc key CLargs)])
    (if entry
        (if (> (length entry) 1)
            (call (second entry))
            default)
        no-val)))

; contains-any?: returns a non-false value iff any argument beyond the first is present in the first.
; list any -> any
(define (contains-any? mlst . vs)
  (and (list? mlst) (ormap (curryr member mlst) vs)))

(define (get-public-key sc)
  (string->bytes/utf-8 (path/param-path (list-ref (url-path (string->url (if (scurl? sc) (scurl->string sc) sc))) 1))))

(define-syntax spawn
  (syntax-rules ()
    [(_ body ...)
     (thread (λ () body ...))]))

(define-syntax is?
  (syntax-rules ()
    [(_ v) (curry equal? v)]))

(define (uuid) (make-uuid))

(define (remote-curl-root rkey rhost rport)
  (message/uri/new rkey (cons rhost rport) "/"))

;; "client-side"

(define (ask/send request-thread method url expr
                  #:compile? [compile? #t]
                  #:metadata [metadata :no-metadata:]
                  #:reply [reply :no-reply:]
                  #:echo [echo :no-echo:])
  (define the-compiled-expr (if compile? (motile/compile expr) expr))
  (define msg (message/ask/new method url the-compiled-expr metadata reply echo))
  (thread-send request-thread
               (request (car (:message/uri/authority url))
                        (cdr (:message/uri/authority url))
                        (:message/uri/scheme url) msg)))

;; "server-side"

(define (start-program expr [be BASELINE] [args '()])
  (let ([fun (motile/call expr be)])
    (cond [(procedure? fun)
           (motile/call/3 fun be args)]
          [else fun])))