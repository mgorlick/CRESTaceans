#lang racket/base

(require
 (only-in racket/vector vector-copy vector-count)
 (only-in "../persistent/hash.rkt" hash/eq? hash/persist?)
 (only-in "island.rkt" island/address? island/address/ok? this/island)
 (only-in "actor.rkt" this/actor)

 (only-in
  "locative.rkt"
  locative?
  locative/curl/authority?
  locative/expires
  locative/id
  locative/export
  locative/revoked?
  locative/sends/positive?
  locative/unexpired?
  locative/send))

(provide
 curl?
 curl/ok?
 curl/island
 curl/id
 curl/id!
 curl/path
 curl/sends
 curl/signing
 curl/signing!
 
 curl/export
 curl/new
 curl/new/any
 curl/send
 
 curl/pretty) ; For debugging.
 
(define-syntax-rule (curl/island  x) (vector-ref x 1)) ; Island address for CURL.
(define-syntax-rule (curl/path    x) (vector-ref x 2)) ; Path of CURL as list (possibly empty) of symbols.
(define-syntax-rule (curl/id      x) (vector-ref x 3)) ; Either the id of locative this CURL represents or the locative itself.
(define-syntax-rule (curl/expires x) (vector-ref x 4)) ; Expiration stamp of CURL.
(define-syntax-rule (curl/sends   x) (vector-ref x 5)) ; Nominal use count of CURL.
(define-syntax-rule (curl/meta    x) (vector-ref x 6)) ; Arbitrary metadata for CURL.
(define-syntax-rule (curl/signing x) (vector-ref x 7)) ; Island signing for CURL.

(define-syntax-rule (curl/id! x y)      (vector-set! x 3 y))
(define-syntax-rule (curl/signing! x y) (vector-set! x 7 y))

;; We assume SHA-512 for signing so each CURL signature is a 64-byte bytes string.
;(define CURL/SIGNING/LENGTH 64)
;; For now we are using sha1 as a stub which produces a 20-byte bytes string.
(define CURL/SIGNING/LENGTH 20)

; x - subject of type test
; type? - type predicate for x
; where - symbol (typially name of function containing the type assertion)
; expectation - string giving expected type
(define-syntax-rule (assert/type x type? where expectation)
  (when (not (type? x))
    (raise-type-error 'where expectation x)))

(define (curl? x)
  (and
   (vector? x)
   (= (vector-length x) 8)
   (eq? (vector-ref x 0) '<curl>)))

;; Returns #t if x is an "extended" curl/id and #f otherwise.
;; An extended curl/id is a triple #(<locative id> <actor id> <actor clan id>), each element a symbol
;; (specifically a UUID).
(define (curl/id/extended? x)
  (and
   (vector? x)
   (= (vector-length x) 3)
   (= (vector-count symbol? x) 3)))

;; Return a version of CURL c suitable for serialization.
(define (curl/export c)
  (let ((x (curl/id c)))
    (cond
      ((locative? x)
       (let ((d (curl/duplicate c)))
         (curl/id! d (locative/export x))
         d))
      ((symbol? x) c) ; x is the id of the locative to which CURL c refers.
      ((curl/id/extended? x) c)
      (else #f))))

;; Returns #t if CURL c is signed and #f otherwise.
(define (curl/signed? c)
  (let ((s (curl/signing c)))
    (and (bytes? s) (= (bytes-length s CURL/SIGNING/LENGTH)))))

;; The path of a locative is a list (possibly empty) of symbols.
;; The null path is equivalent the path name /
;; The path (s_1 s_2 ... s_n) is equivalent to the path name /s_1/s_2/.../s_n
(define (curl/path/ok? p)
  (and (list? p) (andmap symbol? p)))

(define (finite-positive? n)
  (and (number? n) (positive? n) (< n +inf.f)))
         
;; Returns an unsigned CURL derived from locative x.
;; The CURL is immutable with the exception of the signing field which is set if and when the CURL is exported.
;; If the CURL returns from another island then the internal version of the CURL will be reconstructed with
;; the signature intact.

;; x - the locative from which the CURL will be drawn
;; path - the path of the new CURL, a list (possibly empty) of symbols
;; meta - either #f or a persistent hash table containing arbitrary metadata
;; expires - an optional expiration date for the CURL.
;;   If omitted then the CURL adopts the expiration date of locative x.
;;   If given then expires <= expiration date 
(define (curl/new/any x path meta  . lifespan)
  (assert/type x locative? curl/new/any "locative")
  (assert/type path curl/path/ok? curl/new/any "path")
  (assert/type meta curl/meta/ok? curl/new/any "meta")
  (assert/type lifespan (lambda (n) (or (null? n) (finite-positive? (car n)))) curl/new/any "lifespan")
  
  (and
   (locative/unexpired? x)
   (not (locative/revoked? x))
   (let ((deadline (if (null? lifespan) 
                       (locative/expires x)
                       (+ (current-inexact-milliseconds) (* (car lifespan) 1000)))))
     (if (<= deadline (locative/expires x))
         (let ((sends (locative/sends/positive? x)))
           (and sends (vector '<curl> (this/island) path x deadline sends meta #f)))
         
         #f))))

(define (curl/new x path meta . lifespan)
  (assert/type x locative? curl/new/any "locative")
  (assert/type path curl/path/ok? curl/new/any "path")
  (assert/type meta curl/meta/ok? curl/new/any "meta")
  (assert/type lifespan (lambda (n) (or (null? n) (finite-positive? (car n)))) curl/new/any "lifespan")
  
  (and
   (locative/unexpired? x)
   (not (locative/revoked? x))
   (locative/curl/authority? x (this/actor)) ; Additional restriction over curl/new/any.
   (let ((deadline (if (null? lifespan) 
                       (locative/expires x)
                       (+ (current-inexact-milliseconds) (* (car lifespan) 1000)))))
     (if (<= deadline (locative/expires x))
         (let ((sends (locative/sends/positive? x)))
           (and sends (vector '<curl> (this/island) path x deadline sends meta #f)))
         
         #f))))

;(define (curl/new x path meta . expires)
;  (assert/type x locative? curl/new "locative")
;  (assert/type path curl/path/ok? curl/new "curl path")
;  (assert/type meta curl/meta/ok? curl/new "curl metadata")
;
;  (and
;   (locative/unexpired? x)
;   (not (locative/revoked? x))
;   (locative/curl/authority? x (this/actor))
;   (or
;    (null? expires)
;    (let ((t (car expires)))
;      (and (number? t) (positive? t) (<= t (locative/expires x)))))
;
;   (let ((sends (locative/sends/positive? x)))
;    (if sends
;        (vector '<curl> (this/island) path x
;                (if (null? expires) (locative/expires x) (car expires))
;                sends meta #f) ; Note #f indicates CURL is unsigned.
;        #f))))

(define (curl/island/ok? i)
  (and (island/address? i) (island/address/ok? i)))

(define (curl/id/ok? id)
  (and
   (vector? id)
   (= (vector-length id) 3)
   (= (vector-count symbol? id) 3)))

(define (curl/expires/ok? e)
  (and (number? e) (positive? e) (< e +inf.f))) ; Any positive, finite number.

(define (curl/sends/ok? s)
  (and (number? s) (positive? s) (or (integer? s) (= s +inf.0))))

(define (curl/meta/ok? m)
  (or (not m) (and (hash/persist? m) (hash/eq? m)))) ; Either #f or a persistent eq?-hash table.

(define (curl/signing/ok? s strict?)
  (if strict?
      (and (bytes? s) (= (bytes-length s) CURL/SIGNING/LENGTH))
      (or (not s) (and (bytes? s) (= (bytes-length s) CURL/SIGNING/LENGTH)))))

;; We assume that c is of type <curl>.
;; Returns #t if c is well-formed and #f otherwise.
;; This predicate is intended for use by the deserializer as it checks incoming CURLs.
(define (curl/ok? c strict?)
  (and
   (curl/island/ok?   (curl/island c))
   (curl/path/ok?     (curl/path c))
   (curl/id/extended? (curl/id c))
   (curl/expires/ok?  (curl/expires c))
   (curl/sends/ok?    (curl/sends c))
   (curl/meta/ok?     (curl/meta c))
   (curl/signing/ok?  (curl/signing c) strict?)))
;  (and
;   (and (curl/island/ok?   (curl/island c))      (log-info "curl/island/ok\n") #t)
;   (and (curl/path/ok?     (curl/path c))        (log-info "curl/path/ok\n")   #t)
;   (and (curl/id/extended? (curl/id c))          (log-info "curl/id/extended?\n") #t)
;   (and (curl/expires/ok?  (curl/expires c)) (log-info "curl/expires/ok\n") #t)
;   (and (curl/sends/ok?    (curl/sends c))       (log-info "curl/sends/ok\n") #t)
;   (curl/meta/ok?     (curl/meta c))
;   (curl/signing/ok?  (curl/signing c) strict?)))

(define (curl/intra? c)
  (and (curl? c) (locative? (curl/id c))))

;; Stub for signing routine from NaCl crypto library.
;; The real implementation will return a new byte string s that is the signed version of the original
;; byte string x.
;; For now it just returns the original byte string x.
(define-syntax-rule (crypto/sign x key/secret) x)

;; Stub for routine from NaCL crypto library that verifies a signing.
;; x - original byte string
;; s - byte string signing of x
;; key/public - public key corresponding to the secret signing key.
;; The real implementation returns #t if s is the signing of x and #f otherwise.
;(define-syntax-rule (crypto/sign/open x s key/public) #t)

;; Stub implementation of signing for CURLs:
;;   * Uses SHA1 for now instead of SHA-512 (I'll change it when we get the NACL library fully integrated)
;;   * Omits encryption of SHA hash with private key of island.
;(define (curl/sign! c key/secret)
;  (curl/signing! c (crypto/sign (curl/SHA/generate c) key/secret)))

;; Stub for now.
;(define (curl/SHA/generate x) #"FOOBAR")


(define-syntax-rule (curl/duplicate c) (vector-copy c))


;; Need to move a more general form of this routine to the serializer!
;(define (curl/SHA/generate c)
;  (let ((x (curl/duplicate c))
;        (out (open-output-bytes)))
;    (curl/signing! x #f)
;    ;(write (motile/serialize x) out)
;    (sha1-bytes (open-input-bytes (get-output-bytes out)))))
  
;; Stub for now.
;; Returns #t if we can verify the signing of CURL c and #f otherwise.
;(define (curl/signing/ok? c key/public)
;  (and
;   (curl/signed? c)
;   (crypto/sign/open (curl/SHA/generate c) (curl/signing c) key/public)))

;; Used for debugging.
;; Returns the (partial) contents of a CURL as an association list.
(define (curl/pretty c)
  (list
   (cons 'PATH (curl/path c))
   ; curl/id may be #f, a symbol, a vector of three symbols, or a locative.
   (cons 'id   (and (curl/id c) (if (symbol? (curl/id c)) (curl/id c) (locative/id (curl/id c)))))
   (cons 'expires (curl/expires c))
   (cons 'sends   (curl/sends c))
   (cons 'meta    (curl/meta c))
   (cons 'signing (curl/signing c))))

(define (curl/send/inter x message) #f) ; Stub for now.

;; Transmit a generic message m to the actor denoted by CURL c.
;; reply - an optional argument, if given, is the CURL to which a reply message is sent.
(define (! c m . reply)
  (curl/send c (vector 'memora c m (if (null? reply) #f (car reply)))))

;; Using CURL c as a destination address send an arbitrary message m to the actor denoted by c.
;; If the sender desires a reply then the CURL metadata should contain an item reply/x where
;; reply is the symbol reply and x is the CURL to which the reply should be directed.
;; If CURL c is local then curl/send returns #t on successful delivery and #f otherwise.
;; If CURL c is an off-island reference 
(define (curl/send c m)
  (when (not (curl? c))
    (raise-type-error 'curl/send "<curl>" c))

  (let ((x (curl/id c)))
    (cond
      ((symbol? x)           (curl/send/inter c m))         ; An off-island CURL that contains a locative id.
      ((locative? x)         (locative/send x (cons c m)))  ; All on-island CURLs carry a locative in the id slot.
      ((curl/id/extended? x) (curl/send/inter c m))         ; An off-island CURL that contains an extended locative id.
      ; The curl/id is #f. This occurs if CURL c was:
      ;   * Issued by this island, exported to another island, and then used as the target for a message transmission
      ;     back to this island, AND
      ;   * The locative referenced by CURL c has expired in the meantime
      ; Life is a bitch.
      (else #f))))

;(require (only-in "island.rkt" island/address/new this/island))
;(define (tests)
;  (this/island (island/address/new #"" #"www.example.com" 9009))
;  (let*-values ([(root l/root)           (actor/root/new)]
;                [(chieftain l/chieftain) (actor/chieftain/new root 'chieftain)]
;                [(a l/a)                 (actor/new chieftain 'a)])
;
;    (define (execute)
;      (let loop ((message (thread-receive)))
;        (cond
;          ((procedure? message)
;           (message) ; The message is a thunk.
;           (loop (thread-receive)))
;
;          ((pair? message) ; The message is (curl . thunk).
;           ((cdr message))
;           (loop (thread-receive)))
;
;          (else (loop (thread-receive)))))) ; Throw the message away.
;             
;    (locative/curl/authority! l/a (list chieftain root))
;
;    (actor/jumpstart root execute)
;    (actor/jumpstart chieftain execute)
;    (actor/jumpstart a execute)
;
;    (newline)
;
;    (define (test/5c) ; Can we derive a CURL from a locative and does curl/send work?
;      (thread-send
;       (actor/thread root)
;       (lambda ()
;         (let ((c (curl/new l/a '(foo bar baz) #f)))
;           (log-info (format "test/5c\n\t curl: ~a\n\n" (and c (curl/pretty c))))
;           (log-info (format "test/5c\n\t curl/ok: ~a\n\n" (curl? c)))
;           (log-info
;            (format "test/5c\n\t curl/send: ~a\n\n"
;                    (curl/send
;                     c
;                     (lambda () (log-info (format "test/5c\n\t hello world from ~a\n\n" (actor/nickname (this/actor))))))))))))
;    
;    (test/5c)))