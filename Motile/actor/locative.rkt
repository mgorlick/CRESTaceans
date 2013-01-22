#lang racket/base

(require
 (only-in racket/vector vector-memq)
 (only-in "../utility/uuid.rkt" uuid/symbol)
 (only-in "../generate/baseline.rkt" define/global/1 motile/call)
 (only-in "../generate/constant.rkt" constant/generate)
 (only-in "../generate/lambda.rkt" lambda/generate)
 (only-in "../generate/utility.rkt" k/RETURN)
 (only-in "../persistent/environ.rkt" pairs/environ)
 (only-in "../baseline.rkt" BASELINE)
 
 (only-in
  "actor.rkt"
  actor?          ; Type test.
  actor/id
  actor/chieftain
  actor/clan/id
  actor/nickname
  actor/thread
  actor/pretty    ; Debugging.
  this/actor))

(provide
 locative?      ; Type test.
 
 ; Fields
 locative/actor
 locative/expires
 locative/id
 locative/id!
 locative/revoked 
 
 locative/cons/authority?
 locative/cons/authority!
 locative/curl/authority?
 locative/curl/authority!
 
 locative/expired?
 
 locative/revoked?
 locative/sends/positive?
 locative/unexpired?
 
 locative/cons
 locative/cons/any
 locative/send
 locative/unrestricted/new
 
 locative/pretty) ; Debugging.

;; NOTES for applications of locatives.
;; (1) Revocation of locatives by creator (when evidence of abuse for example)

;; (2) Active monitoring of use of locatives by embedding monitoring code inside
;;     send or inside any of the regulating closures

;; (3) Tracing propagation of locatives via send or regulating closures

;; (4) Use models of disease propagation to predict unforeseen transmission of locatives or CURLs

;; (5) "CDC" warnings to islands of "capability outbreaks"

;; (6) Probabilistic success of use for locatives or CURLs (regulators return #t, #f or real value in interval (0, 1).

;; (7) Probabilistic prediction of transfer of capability

;; (8) Tracing propagation of binding environs or dangerous functions as they travel from one actor to another

;; (9) Using serialization to sterilize function or environ transfer

;; (10) Wrapping specific dangerous functions in anti-delegation armor

;; (11) Tracing the provenance of each locative on an island

;; (12) Taxonomy of attack vectors (based on bigraphs) for COAST islands

;; What constraints should locatives enforce?
;; * Lifespan
;;    * Derived locative may shorten lifespan
;; * Number of uses
;;    * Derived locative may reduce number of uses
;; * Path
;;    * Superior path may be extended by derivative locatives but never contracted
;; * Revocation
;;     * Revocation of a superior locative implies revocation of all derived locatives
;; * Delegation
;;    * Uses a hierarchy of island, clan, and actor whitelists and blacklists
;;    * Blacklists take precedence over whitelists
;;    * Island list takes precedence over clan list which takes precedence over actor list

;; If a locative does not have a sends/max count then it inherits its current count from its superior.
;; The blacklist of a locative is the union of its blacklist and the blacklists of its (transitive) superiors.
;; The whitelist of a locative is the uniion of its whitelist and the whitelists of its (transitive) superiors.


;; Type checking

; x - subject of type test
; type? - type predicate for x
; where - symbol (typially name of function containing the type assertion)
; expectation - string giving expected type
(define-syntax-rule (assert/type x type? where expectation)
  (when (not (type? x))
    (raise-type-error 'where expectation x)))

;; Definition of a locative.
(define-syntax-rule (locative/id            x) (vector-ref x 1)) ; UUID as island-unique locative identifier.
(define-syntax-rule (locative/actor         x) (vector-ref x 2)) ; Actor this locative guards.
(define-syntax-rule (locative/expires       x) (vector-ref x 3)) ; Expiration time of locative.
(define-syntax-rule (locative/sends         x) (vector-ref x 4)) ; Remaining sends permitted against this locative.
(define-syntax-rule (locative/senders/intra x) (vector-ref x 5)) ; Predicate for deciding legal sender intra-island.
(define-syntax-rule (locative/senders/inter x) (vector-ref x 6)) ; Predicate for deciding legal sender inter-island.
(define-syntax-rule (locative/revoked       x) (vector-ref x 7)) ; #t if this locative is revoked and #f otherwise.
(define-syntax-rule (locative/prior         x) (vector-ref x 8)) ; Reference to the superior locative (if any) of this locative.
(define-syntax-rule (locative/cons/authority x) (vector-ref x 9)) ; The set of actors (given by actor id) that may locative/cons to this locative
(define-syntax-rule (locative/curl/authority x) (vector-ref x 10)) ; The set of actors (given by actor id) that construct a CURL on this locative.

(define (locative/unrestricted/new actor)
  (vector
   '<locative>
   (uuid/symbol) ; 1 - Locative id.
   actor         ; 2 - Actor this locative represents.
   +inf.f        ; 3 - Never expires.
   +inf.0        ; 4 - Infinite sends.
   ANYONE        ; 5 - No restrictions on intra-island sends.
   ANYONE        ; 6 - No restrictions on inter-island sends.
   #f            ; 7 - Not revoked.
   #f            ; 8 - Not a derived locative.
   #f            ; 9 - No actor may apply locative/cons to this locative.
   #f))          ; 10 - No actor may construct a CURL based on this locative.

(define-syntax-rule (locative/id!      x y) (vector-set! x 1 y))
(define-syntax-rule (locative/expires! x y) (vector-set! x 3 y))
(define-syntax-rule (locative/sends!   x y) (vector-set! x 4 y))
(define-syntax-rule (locative/senders/intra! x y) (vector-set! x 5 y))
(define-syntax-rule (locative/senders/inter! x y) (vector-set! x 6 y))
(define-syntax-rule (locative/revoked!  x)  (vector-set! x 7 #t))

;; The value of locative/senders/intra is always a Motile single-argument predicate that takes
;; an actor as an argument and returns #t if that actor may use the locative as the target of
;; a send (message transmission) and #f otherwise. The default predicate in the primal locative
;; L of any actor a is ANYONE, which returns #t for all values of its argument.
;; Here we effectively "hand compile" just such a Motile predicate.
(define ANYONE
  (let ((definition (lambda/generate 1 (constant/generate #t)))) ; MAG generated by Motile compiling expression: (lambda (_) #t).
    (definition k/RETURN #f #f))) ; Create the closure.

;; Hack to work around bug in start-atomic, end-atomic.
;; segmentation fault when using `start-atomic/end-atomic' from ffi/unsafe/atomic.
(define ATOMIC (make-semaphore 1))
(define-syntax-rule (start-atomic) (semaphore-wait ATOMIC))
(define-syntax-rule (end-atomic)   (semaphore-post ATOMIC))

(define-syntax-rule (time/now) (current-inexact-milliseconds))

;; Set the whitelist of actors permitted to execute a locative/cons against locative x.
(define (locative/cons/authority! x actors)
  (assert/type x locative? locative/cons/authority! "locative")
  (assert/type actors (lambda (a) (andmap actor? a)) locative/cons/authority! "list[actor]")
  (when (not (locative/cons/authority x))
    (vector-set! x 9 (list->vector actors))))


;; Returns #t if actor a is a member of the locative/cons whitelist of locative x and #f otherwise.
(define (locative/cons/authority? x a)
  (assert/type x locative? locative/cons/authority? "locative")
  (assert/type a actor?    locative/cons/authority? "actor")
  (and (vector? (locative/cons/authority x)) (vector-memq a (locative/cons/authority x)) #t))



;; Set the whitelist of actors permitted to execute a curl/new against locative x.
(define (locative/curl/authority! x actors)
  (assert/type x locative? locative/curl/authority! "locative")
  (assert/type x (lambda (y) (andmap actor? y)) locative/curl/authority! "list[actor]")
  (when (not (locative/curl/authority x))
    (vector-set! x 10 (list->vector actors))))

;; Returns #t if actor a is a member of the curl/new whitelist of locative x and #f otherwise.
(define (locative/curl/authority? x a)
  (assert/type x locative? locative/curl/authority "locative")
  (assert/type a actor?    locative/curl/authority "actor")
  (and (vector? (locative/curl/authority x)) (vector-memq a (locative/curl/authority x)) #t))

;; Type test for locatives.
(define (locative? x)
  (and
   (vector? x)
   (= (vector-length x) 11)
   (eq? (vector-ref x 0) '<locative>)))

;; For debugging.
(define (locative/pretty l)
  (cond
    ((not l) null)
    (else
     (cons
      (list
       (cons 'id      (locative/id l))
       (cons 'actor   (actor/nickname (locative/actor l)))
       (cons 'expires (locative/expires l))
       (cons 'sends   (locative/sends l))
       (cons 'revoked (locative/revoked l)))
      
      (locative/pretty (locative/prior l))))))

;; Returns #t if locative x has expired and #f otherwise.
(define (locative/expired? x)
  (>= (time/now) (locative/expires x)))
;; Returns #t if locative x is unexpired and #f otherwise.
(define (locative/unexpired? x)
  (< (time/now) (locative/expires x)))

;; Return the remaining liespan of locative x in inexact milliseconds.
(define (locative/lifespan/remaining x)
  (let ((now (time/now)))
    (if (< now (locative/expires x))
        (- (locative/expires x) now)
        0.0)))

;; Returns the serialized representation of locative x as a vector #(<locative id> <actor id> <actor clan id>)
;; where the actor id and actor clan id are taken from the actor denoted by the loative.
;; Used by the serializer when serializing CURLs.
;; A full locative is not effectively serializable.
;;  !!! DEPRECATED !!!
;(define (locative/export x)
;  (let ((a (locative/actor x)))
;    (vector (locative/id x) (actor/id a) (actor/clan/id a))))

;; Returns n = sends count > 0 for this locative or #f if sends count is exhausted.
(define (locative/sends/positive? x)
  (let ((n (locative/sends x)))
    (cond
      ((number? n)
       (cond
         ((zero? n) #f)
         ((= n +inf.0) +inf.0)
         ((and (integer? n) (positive? n)) n)
         (else #f))) ; Negative number or float? Something is wrong!
      ((not n) ; n is #f.
       ; This locative is living off the sends count of its superior locative.
       (let ((prior (locative/prior x)))
         (and prior (locative/sends/positive? prior))))
      (else #f)))) ; Unknown type for n.

;; Returns #t if locative x can be used as the target of a message transmission.
;; Returns #f if x has been revoked, has expired, is exhausted (sends = 0),
;; or the caller lacks the authority to use this locative.
(define (locative/sendable? x)
  (and
   (not (locative/revoked? x))        ; Not revoked.
   (locative/unexpired? x)            ; Unexpired.
   (locative/sends/positive? x)       ; Sends count is not exhausted.
   (locative/send? x (this/actor))))  ; Actor not banned from using this locative in a send.

;; Return #t if locative x itself has been revoked or if any superior locative of x has been revoked.
;; Return #f if locative x itself has not been revoked and no superior locative of x has been revoked.
;; As an optimization if x proves to be revoked because some superior of x has been revoked then we
;; set the revoke flag of x so that the next search will terminate more quickly.
(define (locative/revoked? x)
  (let loop ((this x))
    (cond
      ((locative/revoked this)
       (when (not (eq? this x))
         (locative/revoke! x))
       #t)
      ((locative/prior this)
       (loop (locative/prior this)))
      (else #f))))

(define-syntax-rule (->bool e)
  (if e #t #f))

;; Returns #t if the given message was delivered to the actor given by locative x.
;; Returns #f if the locative is invalid or revoked. No message is delivered in these cases.
(define (locative/send x message)
  ; Because we have to adjust the contents of the locative we block all other threads from executing
  ; while the send is in progress.
  (start-atomic)
  (let ((ok?
         (cond
           ((locative/sendable? x)
            (locative/sends/--! x) ; Decrement the locative use count.
            #t)
           (else #f))))
    (end-atomic) ; Release the scheduler to other threads.
    (and ok?
         ; Note: If the thread is dead then the #f prevents an exception from being generated.
         (->bool (thread-send (actor/thread (locative/actor x)) message #f)))))

;; Returns diagnostic codes indicating why a locative/send failed for locative x.
(define (locative/send/diagnose x)
  (cond
    ((locative/revoked? x)
     (vector '(send fail revoked) "locative revoked" #f))
    ((locative/expired? x)
     (vector '(send fail expired) "locative expired" #f))
    ((not (locative/send? x (this/actor)))
     (vector '(send fail delegate) "locative forbidden" (actor/pretty (this/actor))))
    (else
     (vector '(send fail exhausted) "locative exhausted" #f))))

(define (motile/actor? x)
  (actor? x))

(define (motile/actor/id a)
  (assert/type a actor? actor/id a)
  (actor/id a))

(define (motile/actor/clan/id a)
  (assert/type a actor? actor/clan/id a)
  (actor/clan/id a))

(define (motile/actor/chieftain a)
  (assert/type a actor? actor/chieftain a)
  (actor/chieftain a))

(define ENVIRON/SENDERS
  (pairs/environ
   BASELINE
   (list
    (define/global/1 'actor?          motile/actor?)
    (define/global/1 'actor/id        motile/actor/id)
    (define/global/1 'actor/clan/id   motile/actor/clan/id)
    (define/global/1 'actor/chieftain motile/actor/chieftain))))

;; Returns #t if actor a can use locative x as a destination address in a (send ...).
;; Since an arbitrary actor can restrict the set of senders we must guard against malicious or erroneous restrictions.
(define (locative/send? x a)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (motile/call (locative/senders/intra x) ENVIRON/SENDERS a)))

;; A legal sends value is one of:
;;  #f
;;  a positive integer
;;  +inf.0
(define (sends/legal? m)
  (or
   (not m) ; m is #f.
   (and
    (number? m)
    (or 
     (and (integer? m) (positive? m)) ; m is a postive integer
     (= m +inf.0)))))                 ; m is +inf.0

;; For (locative/cons ...) a legal senders argument is either #t or a single-argument predicate.
(define (senders/legal? p)
  (or (eq? p #t) (procedure? p)))

;; Returns #t iff m is capable of restricting the sends count of locative x.
(define (locative/sends/restrict? x m)
  (or
   (not m) ; #f means use the count of a predecessor locative.
   
   (let ((n (locative/sends x)))
     (if (not n)
         ; locative x is living off the sends count of a predecessor locative.
         (let ((tail (locative/prior x)))
           (and tail (locative/sends/restrict? tail)))                 
         
         (<= m n))))) ; (m > 0 | m = +inf.0) ^ (n >= 0 | n = +inf.0).

;; Decrement the use count of locative x by m.
;; m - a positive integer or +inf.0.
;; We assume that (locative/sends/restrict? x m) always guards the execution of this function.
(define (locative/sends/restrict! x m)
  (let ((n (locative/sends x)))
    (cond
      ((not n) (locative/sends/restrict! (locative/prior x) m))
      ((integer? n) (locative/sends! x (- n m)))
      ((= n +inf.0) (void)))))

;; p - decision predicate of one argument
;; q - #t or a decision predicate of one argument to restrict the set {a | (p a) is #t}.
(define (senders/restrict p q)
  (if (procedure? q)
      (lambda (a) (and (q a) (p a)))
      p))

(define (locative/expires/restrict? x expiration)
  (or
   (not expiration)
   (and (number? expiration) (<= expiration (locative/expires x)))))

;; Given locative x derive and return a more restricted locative y.
;; Derived locatives form a tree in which descendant locative grants no more (and often less) capability than
;; its parent locative.
;; The capability of a locative X comprises:
;;   * The path of X
;;   * The expiration date of X
;;   * The total number of sends permitted for X
;;   * The set of actors that may use X to send a message to the actor denoted by X
;; Each child locative Y of X is at least as restrictive as X:
;;   * The path of X is always prefix of the path of Y
;;   * The expiration of Y is always <= the expiration of X
;;   * The total number of sends allocated to Y is always <= the total number of sends outstanding for X at the time of the
;;     creation of Y. Each Y may be allocated a specific n > 0 of sends at creation or time or it may share the pool of sends
;;     granted to X at the time of its creation.
;;   * The set of actors that may use Y to send a message to the actor denoted by X is always a subset (though perhaps not proper)
;;     of the set of actors that may use X to send a message to the actor denoted by X.


;; expiration - #f or an expiration timestamp <= the expiration timestamp of locative x
;; sends - #f, m > 0, or +inf.0
;; senders/intra - a single argument predicate over actors
;; senders/inter - a single argument predicate over island addresses
;; Returns a restricted locative y on success and #f on failure.
;; NOTE: locative x may be side-effected by having its sends count reduced so the restriction must be
;; generated atomically.
;; Permits any actor (or more generally, any thread) to create a derivative of any locative x.
(define (locative/cons/any x expires sends senders/intra senders/inter)
  (assert/type x locative? locative/cons "locative")
  (assert/type sends sends/legal? locative/cons "sends")
  (assert/type senders/intra senders/legal? locative/cons "senders restriction")
  (assert/type senders/inter senders/legal? locative/cons "senders restriction")
  
  (start-atomic)
  (let ((outcome
         (cond
           ((and
             (locative/expires/restrict? x expires)        ; Is the new expiration <= the old expiration?
             (not (locative/revoked? x)))                  ; If the locative has been revoked then it is useless.
            
            (cond
              ((locative/sends/positive? x)
               (cond
                 ((not sends)
                  ; Return a derived locative.
                  (vector
                   '<locative>
                   (uuid/symbol)
                   (locative/actor x)
                   expires
                   #f ; Each send will decrement some parent locative sends count.
                   (senders/restrict (locative/senders/intra x) senders/intra)
                   (senders/restrict (locative/senders/inter x) senders/inter)
                   #f
                   x
                   (locative/cons/authority x)
                   (locative/curl/authority x)))
                 
                 ((locative/sends/restrict? x sends) ; Do the sends requested fit within the budget of locative x?
                  (locative/sends/restrict! x sends)
                  ; Return a derived locative.
                  (vector
                   '<locative>
                   (uuid/symbol)
                   (locative/actor x)
                   expires
                   sends
                   (senders/restrict (locative/senders/intra x) senders/intra)
                   (senders/restrict (locative/senders/inter x) senders/inter)
                   #f
                   x
                   (locative/cons/authority x)
                   (locative/curl/authority x)))
                 
                 (else #f))) ; Insufficient budget for requested sends count.
              
              (else #f))) ; Sends count exhausted in locative x.
           
           ; Three possibilities:
           ;   (1) Requested expiration is > expiration of locative x
           ;   (2) Calling actor does not have authority to cons with locative x
           ;   (3) Locative x is revoked
           (else #f))))
    (end-atomic)
    outcome))

(define (locative/cons x expires sends senders/intra senders/inter)
  (and
       (locative/cons/authority? x (this/actor))
       (locative/cons/any x expires sends senders/intra senders/inter)))

;; OBSOLETE: Returns #t if the actor calling locoative/cons has sufficient authority to cons a new locative to
;; a locative referencing actor a.
;; Returns #f if the calling actor does NOT have sufficient authority.
;; Given a locative x for an actor a then the only actors with sufficient authority are:
;;  * a itself
;;  * the clan chieftain of a
;;  * the root chieftain of the island of a
;(define (locative/cons/authority? a)
;  ; Only the actor itself, its clan chieftain, or the root chieftain
;  ; has the authority to generate a restriction of a locative of the actor.
;  ;(display (format "locative/cons/authority: actor is: ~a\n" (actor/pretty (this/actor))))
;
;  (let ((t (current-thread)))
;    (or
;     (eq? t (actor/thread a)) ; Actor a itself is performing the locative/cons
;     (if (actor/chieftain a)
;         ; Is the chieftain of a or the root actor performing the locative/cons?
;         (or (eq? t (actor/thread (actor/chieftain a))) (eq? t (actor/thread (actor/root/find a))))
;
;         ; Actor a does not have a chieftain hence it must be the root actor and
;         ; if we got here then t is not the thread of the root actor.
;         #f))))


;; Returns a diagnosis as to why the caller may not be able to apply locative/cons to locative x.
;; If the locative/cons is ill-formed then a diagnostic vector will be returned.
;; If the locative/cons is well-formed then #f (signifying no diagnostics) is returned.
(define (locative/cons/diagnose x expires sends)
  (cond
    ((locative/revoked? x)
     #((locative/cons fail revoked) "revoked" #f))
    
    ((not (locative/expires/restrict? x expires))
     #((locative/cons fail expires) "bad expires" #f))
    
    ((not (locative/cons/authority? (locative/actor x)))
     #((locative/cons fail authority) "insufficient authority" #f))
    
    ((not (locative/sends/positive? x))
     #((locative/cons fail exhausted) "sends exhausted" #f))
    
    ((not (locative/sends/restrict? x sends))
     #((locative/cons fail budget) "insufficent sends" #f))
    
    (else #f)))


;; Decrement the use count of the given locative x.
;; Returns the new value of the sends count or #f if the sends count is exhausted.
(define (locative/sends/--! x)
  (let ((n (locative/sends x)))
    (cond
      ((number? n)
       (cond
         ((zero? n) #f)
         ((= n +inf.0) +inf.0)
         ((and (integer? n) (positive? n))
          (let ((m (sub1 n)))
            (locative/sends! x m)
            m))
         (else #f))) ; Negative integer or float? Something is wrong.
      
      ((not n) ; n is #f.
       ; Locative x is living off the sends count of a superior locative.
       (let ((tail (locative/prior x)))
         (and tail (locative/sends/--! tail))))
      
      (else #f)))) ; Unknown type for n.

;; Revoke locative x.
;; Only the actor to whom locative x refers or the chieftain of that actor can revoke locative x.
(define (locative/revoke! x)
  (let ((t (current-thread)))
    (when (or (eq? t (actor/thread x)) (eq? t (actor/thread (actor/chieftain x))))
      (locative/revoked! x))))
