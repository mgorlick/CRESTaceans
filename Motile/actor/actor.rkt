#lang racket/base

;; Defines the structure of an actor.
;; This file exists solely to break cyclic dependencies between locative.rkt and jumpstart.rkt

(require
 (only-in "../utility/uuid.rkt" uuid/symbol))

(provide
 actor?

 ; Fields.
 actor/clan/id
 actor/chieftain
 actor/id
 actor/id!
 actor/thread
 actor/thread!
 actor/birthdate
 actor/nickname
 actor/pretty
 
 ; Constructors.
 actor/skeleton/new
 actor/chieftain/skeleton/new

 this/actor)

;; Actor structure.
;; DO NOT change this structure without making like changes in jumpstart.rkt!
(define-syntax-rule (actor/clan/id    x) (vector-ref x 1)) ; The clan id of which this actor is a member.
(define-syntax-rule (actor/chieftain  x) (vector-ref x 2)) ; The chieftain of this actor.
(define-syntax-rule (actor/id         x) (vector-ref x 3)) ; The id of the actor.
(define-syntax-rule (actor/thread     x) (vector-ref x 4)) ; The thread that is animating this actor.
(define-syntax-rule (actor/birthdate  x) (vector-ref x 5)) ; The creation timestamp of the actor.
(define-syntax-rule (actor/nickname   x) (vector-ref x 6)) ; For debugging and logging.

;; Required for actor construction.
(define-syntax-rule (actor/id!     a x) (vector-set! a 3 x))
(define-syntax-rule (actor/thread! a t) (vector-set! a 4 t))

(define-syntax-rule (time/now) (current-inexact-milliseconds))

;; Type test for actors.
(define (actor? x)
  (and
   (vector? x)
   (= (vector-length x) 7)
   (eq? (vector-ref x 0) '<actor>)))

(define ANONYMOUS 'anonymous)

;; Utility routine for constructing an actor.
;; Returns an actor structure that will be completed by the caller.
(define (actor/skeleton/new chieftain nickname)
  (vector
   '<actor>
   (actor/id chieftain) ; Actor clan id.
   chieftain            ; Actor chieftain
   (uuid/symbol)        ; Actor id
   #f                   ; Placeholder for thread animating this actor.
   (time/now)           ; Actor birthdate.
   nickname))

;; Utility routine for constructing an actor that is a clan chieftain.
;; Returns an actor structure that will be completed by the caller.
;; Note: clan chieftains are the only actors whose actor/id is the same as their clan/id.
(define (actor/chieftain/skeleton/new chieftain nickname)
  (let ((id (uuid/symbol)))
    (vector
     '<actor>
     id           ; Clan id of new clan.
     chieftain    ; The superior clan chieftain of this clan chieftain.
     id           ; Actor id of new clan chieftain.
     #f           ; Placeholder for thread animating this actor.
     (time/now)   ; Birthdate.
     nickname)))

(define this/actor (make-parameter #f))

;; For debugging.
;; x - an actor
;; Returns a list ((id_1 . nickname_1) ... (id_m . nickname_m)) tracing the parentage of actor x
;; from x itself (id_1 . nickname_1) all the way to the root chieftain (id_m . nickname_m).
(define (actor/pretty x)
  (cond
    ((not x) null)
    (else (cons (cons (actor/id x) (actor/nickname x)) (actor/pretty (actor/chieftain x))))))

;
;(require (only-in "island.rkt" island/address/new this/island))
;(define (tests)
;  (this/island (island/address/new #"" #"www.example.com" 9009))
;  (let*-values ([(root l/root)           (actor/root/new)]
;                [(chieftain l/chieftain) (actor/chieftain/new root 'chieftain)]
;                [(a l/a)                 (actor/new chieftain 'a)]
;                [(forward l/forward)     (actor/new chieftain 'forward)])
;
;    (define (execute)
;      (let loop ((message (thread-receive)))
;        (cond
;          ((procedure? message)
;           (message) ; The message is a thunk.
;           (loop (thread-receive)))
;          (else (loop (thread-receive))))))
;             
;    ; Set the authority for locative/cons for each actor.
;    (locative/cons/authority! l/root (list root))
;    (locative/cons/authority! l/chieftain (list root chieftain))
;    (locative/cons/authority! l/a (list root chieftain a))
;    (locative/cons/authority! l/forward (list root chieftain forward))
;
;    ; Start actors a and forward.
;    (actor/jumpstart root execute)
;    (actor/jumpstart chieftain execute)
;    (actor/jumpstart a execute)
;    (actor/jumpstart forward execute)
;
;    (pretty-display (locative/pretty l/root))
;    (pretty-display (locative/pretty l/chieftain))
;    (pretty-display (locative/pretty l/a))
;
;    (newline)
;
;    ; t - name of test as integer or symbol
;    (define (actor/display t)
;      (log-info
;       (format
;        "test/~a\n\t this/actor: ~a\n\t this/chieftain: ~a\n" t (actor/pretty (this/actor)) (actor/pretty (this/chieftain)))))
;
;    (define (locative/display t)
;      (log-info
;       (format
;        "test/~a\n\t this/locative: ~a\n" t (locative/pretty (this/locative)))))
;
;
;    (define (test/alpha) ; Test that this/locative is properly set for each actor in the hierarchy.
;      (thread-send (actor/thread root)      (lambda () (locative/display 'root)))
;      (sleep 1)
;      (thread-send (actor/thread chieftain) (lambda () (locative/display 'chieftain)))
;      (sleep 1)
;      (thread-send (actor/thread a)         (lambda () (locative/display 'a)))
;      (sleep 1))
;    
;    (define (test/beta) ; Test that (actor/root/find x) always finds the root actor.
;      (log-info (format "test/beta\n\t ~a\n\t ~a\n\t ~a\n" 
;                       (actor/pretty (actor/root/find a))
;                       (actor/pretty (actor/root/find chieftain))
;                       (actor/pretty (actor/root/find root)))))
;    
;    
;    (define (test/1) ; Make sure that all of the per-actor parameters are ok.
;      (thread-send
;       (actor/thread a)
;       (lambda () 
;         (actor/display '1a)
;
;         (thread-send
;          (actor/thread chieftain)
;          (lambda () 
;            (actor/display '1b)
;      
;            (thread-send
;             (actor/thread root)
;             (lambda ()
;               (actor/display '1c))))))))
;
;    
;    (define (test/2) ; Make sure that (this/locative) works.
;      (thread-send
;       (actor/thread a)
;       (lambda ()
;         (locative/display '2a)
;         
;         (thread-send
;          (actor/thread chieftain)
;          (lambda ()
;            (locative/display '2b)
;            
;            (thread-send
;             (actor/thread root)
;             (lambda () (locative/display '2c))))))))
;           
;
;    (define (test/3) ; Make sure that all of the "locative is good" tests work."
;      (thread-send
;       (actor/thread a)
;       (lambda ()
;         (let ((x (this/locative)))
;           (log-info
;            (format
;             "test/3\n\t revoked: ~s\n\t sends/positive: ~s\n\t unexpired: ~s\n\t send: ~s\n\n"
;             (locative/revoked? x)
;             (locative/sends/positive? x)
;             (locative/unexpired? x)
;             (locative/send? x x)))))))
;
;    (define (test/4a) ; locative/cons
;      (thread-send
;       (actor/thread a)
;       (lambda ()
;         (let ((x (locative/cons
;                   (this/locative) ; Restrict the unrestricted locative of this actor.
;                   (+ (time/now) 30000) ; 30 seconds.
;                   1 ; single-use
;                   (lambda (x) (eq? x forward)) ; Usable only by the forward actor.
;                   #t)))
;           (log-info (format "test/4a\n\t locative: ~a\n\n" (locative/pretty x)))))))
;
;    (define (test/5a) ; locative/sendable?
;      (thread-send
;       (actor/thread a)
;       (lambda ()
;         (log-info (format "test/5a\n\t sendable: ~a\n\n" (locative/sendable? (this/locative)))))))
;
;    (define (test/5b) ; Does locative/send work?
;      (thread-send
;       (actor/thread root)
;       (lambda ()
;         (log-info
;          (format "test5b\n\t locative/send: ~a\n"
;                  (locative/send
;                   l/a
;                   (lambda () (log-info (format "test/5b\n\t hello world from ~a\n\n" (actor/pretty (this/actor)))))))))))
;
;
;
;    (define (test/6a) ; Restrict a locative to a particular sender.
;      (thread-send
;       (actor/thread root)
;       (lambda ()
;         ; Construct a locative that can be used only by a and just once for a message back to the root.
;         (let ((a_only (locative/cons l/root +inf.0 1 (lambda (x) (eq? x a)) #t)))
;           (log-info (format "test/6a\n\t a_only: ~a\n\n" (and a_only (locative/pretty a_only))))
;           (thread-send
;            (actor/thread a)
;            (lambda ()
;              (locative/send a_only (lambda () (log-info "test/6a\n\t root got message from a\n\n")))))))))
;
;    (define (test/6b) ; Restrict a locative to a particular sender.
;      (thread-send
;       (actor/thread root)
;       (lambda ()
;         ; Construct a locative that can be used only by a and just once for a message back to the root.
;         (let ((a_only (locative/cons l/root +inf.0 1 (lambda (x) (eq? x a)) #t)))
;           (log-info (format "test/6b\n\t a_only: ~a\n\n" (and a_only (locative/pretty a_only))))
;           ; We will send locative a_only to both a and a's chieftain.
;           ; The locative/send from a's chieftain should fail.
;
;           (thread-send
;            (actor/thread chieftain)
;            (lambda ()
;              (log-info (format "test/6b\n\t locative/send from chieftain: ~a\n\n"
;                        (locative/send a_only (lambda () (log-info "test/6a\n\t root got message from chieftain!!\n\n")))))
;
;           (thread-send
;            (actor/thread a)
;            (lambda ()
;              (locative/send a_only (lambda () (log-info "test/6b\n\t root got message from a\n\n")))))))))))
;
;    (define (test/6c) ; Restrict a locative to a particular sender and ensure that its sends count is exhausted.
;      (thread-send
;       (actor/thread root)
;       (lambda ()
;         ; Construct a locative that can be used only by a and just once for a message back to the root.
;         (let ((a_only (locative/cons l/root +inf.0 1 (lambda (x) (eq? x a)) #t)))
;           (log-info (format "test/6c\n\t a_only: ~a\n\n" (and a_only (locative/pretty a_only))))
;           ; We will send locative a_only to both a and a's chieftain.
;           ; The locative/send from a's chieftain should fail.
;
;           (thread-send
;            (actor/thread chieftain)
;            (lambda ()
;              (log-info (format "test/6c\n\t locative/send from chieftain: ~a\n\n"
;                        (locative/send a_only (lambda () (log-info "test/6a\n\t root got message from chieftain!!\n\n")))))
;              ; Wait until locative a_only is exhausted.
;              (let loop ((usable (positive? (locative/sends a_only))))
;                (when usable
;                  (loop (positive? (locative/sends a_only)))))
;              (log-info "test/6c\n\t locative a_only is exhausted\n\n"))) ; Tell us that the sends count is exhausted.
;
;           (thread-send
;            (actor/thread a)
;            (lambda ()
;              (locative/send a_only (lambda () (log-info "test/6c\n\t root got message from a\n\n")))))))))
;    
;    (define (test/7a) ; Testing the expiration time of a locative.
;      (thread-send
;       (actor/thread root)
;       (lambda ()
;         ; Construct a locative that can be used only by a for just a limited period of time (~ 300 msecs).
;         (let ((x (locative/cons l/root (+ (time/now) 375) 100 (lambda (x) (eq? x a)) #t)))
;           (log-info (format "test/7a: x: ~a\n\n\n" (and x (locative/pretty x))))
;           (thread-send
;            (actor/thread a)
;            (lambda ()
;              (let loop ((n 0))
;                (cond
;                  ((locative/send x (lambda () (log-info "test/7a: root got message from actor a\n\n\n")))
;                    (sleep 0.1)
;                    (loop (add1 n)))
;                  (else
;                   (log-info (format "test/7a: total sends: ~a\n\n\n" (- 100 (locative/sends/positive? x)))))))))))))
;
;    (define (test/7b) ; Testing a shared count.
;      (thread-send
;       (actor/thread root)
;       (lambda ()
;         ; Construct a locative that can be used only by a for just a limited period and that shares a count with a parent locative.
;         (let* ((expires (+ (time/now) 1000))
;                (x (locative/cons l/root +inf.0  100 (lambda (x) (eq? x a)) #t))
;                (y (locative/cons x      expires #f  #t                     #t))) ; Share the sends count with locative x.
;           (log-info (format "test/7b: x: ~a\n\n" (locative/pretty x)))
;           (log-info (format "test/7b: y: ~a\n\n" (locative/pretty y)))
;           (thread-send
;            (actor/thread a)
;            (lambda ()
;              (log-info (format "test/7b: locative/send? : ~a\n\n" (locative/send? y (this/actor))))
;              (if (locative/send y (lambda () (log-info "test/7b: root got message from a\n\n")))
;                    (log-info "test/7b: locative/send: #t\n\n\n")
;                    (log-info (format "test/7b: locative/send: ~a\n\n" (locative/send/diagnose y))))))))))
;
;
;
;    (test/alpha)
;    (test/beta)
;    (test/1)
;    (test/2)
;    (test/3)
;    (test/4a)
;    (test/5a)
;    (test/5b)
;;    (test/5c) ; Moved to curl.rkt
;    (test/6a)
;    (test/6b)
;    (test/6c)
;    (test/7a)
;    (sleep 1)
;    (test/7b)
;    ))
