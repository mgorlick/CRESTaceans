#lang racket/base

(require racket/function
         racket/list
         racket/contract
         racket/dict
         "config.rkt"
         "../../../Motile/generate/baseline.rkt"
         "../../../Motile/persistent/environ.rkt"
         "../../../Motile/actor/actor.rkt"
         "../../../Motile/actor/curl.rkt"
         "../../../Motile/actor/send.rkt"
         "../../../Motile/persistent/hash.rkt"
         "../../../Motile/compile/serialize.rkt"
         "../../../Motile/actor/delivery.rkt"
         "../../../Motile/actor/promise.rkt"
         "../../../Motile/actor/jumpstart.rkt"
         "../../../Motile/actor/island.rkt"
         "../../../Motile/actor/locative.rkt"
         "../../../peer/src/net/tcp-peer.rkt" )

(define A-LONG-TIME 10e100)

(provide argsassoc
         meta-has-any?
         ->boolean
         curl/known-public?
         curl/get-public
         define-wrapper-for
         define-wrapper-for*
         listen-on/make-root
         wait-forever
         eval-actor)

; argsassoc: string [call: string -> any] [no-val: any] [default: any] -> any
; separates the provided command line arguments of the form:
;             key1=val1 key2=val2 key3
; if the provided key name is present and has a value, returns `call' applied to that value.
; if the provided key name is present but has no value, returns `default'.
; if the provided key name is not present, returns `no-val'.
(define CLargs (map (curry regexp-split #rx"=") (vector->list (current-command-line-arguments))))
(define (argsassoc key 
                   #:call [call values] 
                   #:no-val [no-val #f] 
                   #:default [default #t])
  (let ([entry (assoc key CLargs)])
    (if entry
        (if (> (length entry) 1)
            (call (second entry))
            default)
        no-val)))

;; meta-has-any? 
;; takes a Motile persistent hash table
;; and a list of cons cells (k/v) and retreives
;; the first value `v' in the hash table
;; for which a key `k' is present
(define (meta-has-any? meta . vs)
  (ormap (λ (k.v) (equal? (cdr k.v) (hash/ref meta (car k.v) #f)))
         vs))

;; ->boolean: convert any `truthy' value to #t.
(define-syntax-rule (->boolean expr)
  (if expr
      #t
      #f))

;; curl/known-public?
;; test to see whether we have a public curl for some host/port combination.
(define/contract (curl/known-public? host port)
  ((or/c string? bytes?) exact-nonnegative-integer? . -> . boolean?)
  (->boolean (dict-ref PUBLICS (cons (if (string? host) (string->bytes/utf-8 host) host) port) #f)))

;; curl/get-public:
;; returns a public CURL naming some computation on some other island
;; if we have one available.
;; currently this is only indexed by host/port. 
;; we could imagine some additional metadata distinguishing between
;; multiple public CURLs naming computations on the same island.
(define/contract (curl/get-public host port)
  ((or/c string? bytes?) exact-nonnegative-integer? . -> . curl?)
  (motile/deserialize (dict-ref PUBLICS (cons (if (string? host) (string->bytes/utf-8 host) host)
                                              port))
                      #f))

;; make-root/get-public/register-public:
;; makes a root actor, creates and registers a public locative
;; in this island's locative serialization table,
;; then retrieves a public locative and public CURL for use
;; in speaking to the root actor.
;; (public locatives and CURLs are stable between island restarts,
;; as long as a public locative is indexed before using them)s
(define/contract (make-root/get-public/register-public)
  (-> (values actor? locative? locative? curl?))
  (define-values (root root-locative) (actor/root/new))
  (define public-locative (locative/cons/any root-locative A-LONG-TIME A-LONG-TIME #t #t))
  (locative/id! public-locative '(public))
  (define public-curl (curl/new/any public-locative null #f))
  (motile/serialize public-curl) ; put in exports table.
  (values root root-locative 
          public-locative 
          (if (curl/known-public? (island/address/dns (this/island)) (island/address/port (this/island)))
              (curl/get-public (island/address/dns (this/island)) (island/address/port (this/island)))
              public-curl)))

;; listen-on/make-root: island/address? 
;; sets the island address, then calls `make-root/get-public/register-public' with that island address,
;; then starts up the networking layer listening on that island address,
;; with incoming messages delivered to the ROOT created by `make-root/get-public/register-public'.
(define/contract (listen-on/make-root island-address)
  (island/address? . -> . (values actor? locative? locative? curl? thread?))
  (this/island island-address)
  ; make root actor
  (define-values (ROOT ROOT-LOCATIVE PUBLIC-LOCATIVE PUBLIC-CURL) 
    (make-root/get-public/register-public))
  ; deliver incoming messages to ROOT
  (define COMM-thd (run-tcp-peer (bytes->string/utf-8 (island/address/dns island-address))
                                 (island/address/port island-address)
                                 (actor/thread ROOT) #:encrypt? #f))
  (set-inter-island-router! COMM-thd)
  (values ROOT ROOT-LOCATIVE PUBLIC-LOCATIVE PUBLIC-CURL COMM-thd))

;; wait-forever:
;; pause this thread forever, until externally killed.
(define (wait-forever)
  (semaphore-wait (make-semaphore)))


;; eval-actor:
;; given an actor record `a', a motile zero-argument procedure `p'
;; and a binding environment `benv', instantiate `a' as the 
;; procedure `p' evaluated in the context of `benv'
(define/contract (eval-actor a p benv)
  (actor? procedure? environ? . -> . any/c)
  (actor/jumpstart a 
                   (λ ()
                     (let ([ret (motile/call p benv)])
                       ret))))

;; define-wrapper-for:
;; defines a new procedure `g' that wraps around another function `f'.
;; `g' applies another procedure `precall' to its arguments,
;; then applies those same arguments to procedure `f'.
;; the intention here is that, before passing the arguments along
;; to the function being wrapped, you might e.g., update a backend
;; monitoring system about what is being called,
;; or perhaps kill off the calling thread, preventing any further activity.
(define-syntax-rule (define-wrapper-for f g precall)
  (define (g . xs)
    (apply precall (cons f xs))
    (apply f xs)))

;; define-wrapper-for*:
;; defines a new procedure named `g'.
;; `g' calls the supplied procedure `f' with the arguments
;; provided to `g', then calls the provided procedure
;; `postcall', with arguments:
;; * the result of applying `f' to the args of `g'
;; * a pointer to the specific procedure `f' that was wrapped
;; * the arguments provided to `g'
;; the intention here is that a post-call-wrapper
;; might intercept whatever a binding environment call returns
;; to an actor and log, modify or deny it in some way.
(define-syntax-rule (define-wrapper-for* f g postcall)
  (define (g . xs)
    (let ([v (apply f xs)])
      (apply postcall (list* v f xs)))))