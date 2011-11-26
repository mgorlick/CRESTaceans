#lang racket/base

(require
 ; !!! Bug in racket 5.1.2 causes drracket to crash in test/7b with message "about to suspend in atomic mode"
 ;(only-in ffi/unsafe/atomic start-atomic end-atomic)
 (only-in "../utility/uuid.rkt" uuid/symbol)

 (only-in
  "actor.rkt"
  ; Type test.
  actor?

  ; Fields.
  actor/chieftain
  actor/clan/id
  actor/id
  actor/thread
  actor/thread!

  ; Actor equivalent of (current-thread).
  this/actor
  
  ; Debugging and error reporting.
  actor/pretty

  ; Constructors.
  actor/skeleton/new
  actor/chieftain/skeleton/new)
 
 (only-in
  "locative.rkt"
  ; Fields.
  locative/actor
  ; Locative constructor.
  locative/unrestricted/new))

(provide
 ; Actor constructors.
 actor/new
 actor/chieftain/new
 actor/root/new
 
 ; Start an actor.
 actor/jumpstart
 
 ; Tests for chieftains and the root actor.
 actor/chieftain?
 actor/root?
 
 ; Actor execution states.
 actor/dead?
 actor/alive?
 actor/suspended?)


; x - subject of type test
; type? - type predicate for x
; where - symbol (typially name of function containing the type assertion)
; expectation - string giving expected type
(define-syntax-rule (assert/type x type? where expectation)
  (when (not (type? x))
    (raise-type-error 'where expectation x)))

(define-syntax-rule (time/now) (current-inexact-milliseconds))

;; Tests for the execution state of an actor.
(define (actor/dead? x)
  (and (actor? x) (thread-dead? (actor/thread x))))

(define (actor/alive? x)
  (and (actor? x) (thread-running? (actor/thread x))))

(define (actor/suspended? x)
  (and
   (actor? x)
   (not (thread-dead? (actor/thread x)))
   (not (thread-running? (actor/thread x)))))

;; Constructing actors.

;; Default actor nickname.
(define ANONYMOUS 'anonymous)

;; Create a new actor whose clan chieftain is given returning the primal locative of the actor.
;; There is only one primal locative per actor.
(define (actor/new chieftain . nickname)
  (let* ((actor (actor/skeleton/new chieftain (if (null? nickname) ANONYMOUS (car nickname))))
         (locative (locative/unrestricted/new actor))
         (t (thread (actor/shrinkwrap locative))))
    (actor/thread! actor t) ; Patch up the actor object.
    (values actor locative)))

;; Create a clan chieftain.
;; parent - chieftain of the superior clan
;;    In esssence chieftains are the only actors that are members of two clans:
;;    the parent clan that gave them birth and the clan that they themselves represent.
;; In addition clan chieftains are the only actors for which their actor id and their clan id are identical.
;; Returns the primal locative of the the clan chieftain.
(define (actor/chieftain/new chieftain . nickname)
  (let* ((actor (actor/chieftain/skeleton/new chieftain (if (null? nickname) ANONYMOUS (car nickname))))
         (locative (locative/unrestricted/new actor))
         (t (thread (actor/shrinkwrap locative))))
    (actor/thread! actor t) ; Patch up the actor object.
    (values actor locative)))

;; Create a ROOT clan chieftain (only one per island).
;; The root chieftain is the only chieftain (and actor) on an island that does not have a superior chieftain.
(define (actor/root/new) (actor/chieftain/new #f 'root))

;; TYPE TESTS for CHIEFTAINS.

;; Returns #t iff x is the chieftain of the root clan and #f otherwise.
;; The root chieftain is the root of the tree of chieftains.
(define (actor/root? x)
  (and (actor? x) (eq? (actor/clan/id x) (actor/id x)) (not (actor/chieftain x))))

;; Returns #t iff x is the chieftain of some clan and #f otherwise.
;; Clan chieftains are the only actors whose actor id and clan id are identical.
(define (actor/chieftain? x)
  (and (actor? x) (eq? (actor/clan/id x) (actor/id x))))

;; Trace back up the tree of actors all the way to the root actor.
(define (actor/root/find a)
  (if (actor/root? a)
      a ; a is the root actor.
      (actor/root/find (actor/chieftain a))))

;; x is either a locative or a clan id.
;; Returns a closure that returns #t if the caller is a member of the clan denoted by x and #f otherwise.
;(define (CLAN-ONLY x)
;  (cond
;    ((symbol? x)   (lambda () (eq? (actor/clan/id (this/actor)) x)))
;    ((locative? x) (CLAN-ONLY (actor/clan/id x)))
;    (else          NEVER))) ; Guarantee that the derived locative will be non-delegable.

;; set is a vector containing actor ids.
;; Returns a predicate that determines if the caller is a member of the delegation.
;(define (vector/member? v)
;   (lambda () (and (vector-memq (actor/id (this/actor)) v) #t)))

;; Given a lifespan of n > 0 milliseconds returns a closure that will return #t for the next n  milliseconds
;; and #f thereafter.
;(define (actor/when/lifespan n)
;  (if (and (integer? n) (positive? n))
;      (let ((expiration (+ (time/now) n)))
;        (lambda () (< (time/now) expiration)))
;      NEVER))

;; Return the actor/ids of the chieftains in the chain of command of actor x in order of increasing authority.
;(define (locative/chain-of-command x)
;  ; Each actor in the chain adds the actor id of its higher authority to the list.
;  (let loop ((x x) (chain null))
;    (cond
;      ((not (actor/chieftain? x)) (reverse chain)) ; x is the root chieftain. Return the list in order of increasing authority.
;
;      ((eq? (actor/id x) (actor/clan/id x)) ; x is a clan chieftain.
;       (let ((higher (actor/chieftain x)))
;         (loop higher (cons (actor/id higher) chain)))) ; x's clan chieftain is next in the chain of command.
;
;      (else
;       ; x is just an ordinary actor. So just add x's clan chieftain to the list.
;       (loop (actor/chieftain x) (cons (actor/clan/id x) chain))))))

;; Returns #t if actors a and b are members of the same clan.
(define (actor/clan/same? a b)
  (and
   (actor? a)
   (actor? b)
   (eq? (actor/clan/id a) (actor/clan/id b))))

; Start the actor executing the given thunk.
(define (actor/start actor thunk)
  (thread-send (actor/thread actor) thunk))
  
;; Return the parent chieftain of chieftain c if it exists.
;; Returns #f if the c is the root chieftain.
(define (chieftain/parent c)
  (assert/type c actor? chieftain/parent "actor")
  (assert/type c actor/chieftain? chieftain "chieftain")
  (let ((parent (actor/chieftain c)))
    (if (eq? parent c) #f parent))) ; The parent of the root chieftain is itself.

(define this/clan      (make-parameter #f))
(define this/chieftain (make-parameter #f))
(define this/locative  (make-parameter #f))

;; Returns a thunk that jumpstarts an actor thread with the proper thread parameters.
(define (actor/shrinkwrap locative)
  (lambda ()
    (let ((x (locative/actor locative)))
      (parameterize
          ((this/actor     x)
           (this/clan      (actor/clan/id x))
           (this/locative  locative)
           (this/chieftain (actor/chieftain x)))
        (with-handlers
            ([exn:fail?
              (lambda (e)
                (log-error (format "actor/shrinkwrap: ~s <error>: ~s\n\n" (actor/pretty (this/actor)) (exn-message e))))])
          
          (let ((jumpstart (thread-receive)))
            (when (procedure? jumpstart)
              (jumpstart))))))))

(define-syntax-rule (actor/jumpstart a thunk)
  (thread-send (actor/thread a) thunk))
