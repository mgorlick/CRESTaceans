#lang racket/base

(require
 (only-in
  "jumpstart.rkt"
  actor/jumpstart
  actor/new
  actor/chieftain/new)

  (only-in "locative.rkt" locative/cons/any)
  (only-in "root.rkt" ROOT)
  
  "logger.rkt"

 (only-in "curl.rkt" curl? curl/new/any curl/send curl/path curl/sends curl/pretty))

(define-syntax-rule (assert precondition where message)
  (when (not precondition)
    (error 'where message)))

(define-syntax-rule (promise/semaphore p) (vector-ref p 1))
(define-syntax-rule (promise/lifespan p)  (vector-ref p 2)) ; Total lifespan of promise in real seconds.
(define-syntax-rule (promise/deadline p)  (vector-ref p 3)) ; Expiration time of promise.
(define-syntax-rule (promise/kept p)      (vector-ref p 4)) ; Set when promise is resolved.
(define-syntax-rule (promise/ruined p)    (vector-ref p 5)) ; Set when promise expires before resolution.
(define-syntax-rule (promise/value p)     (vector-ref p 6)) ; Value of promise when kept.

(define-syntax-rule (promise/kept! p)    (vector-set! p 4 #t))
(define-syntax-rule (promise/ruined! p)  (vector-set! p 5 #t))
(define-syntax-rule (promise/value! p x) (vector-set! p 6 x))
  
(define (promise? p)
  (and (vector? p) (= (vector-length p) 7) (eq? (vector-ref p 0) '<promise>)))

(define-syntax-rule (assert/type x type? where expectation)
  (when (not (type? x))
    (raise-type-error 'where expectation x)))

(define (promise/keep! p x)
  ; Order is crucial here.
  (promise/value! p x)
  (promise/kept! p)
  (semaphore-post (promise/semaphore p))) ; Wakeup any threads waiting on this promise.

(define (promise/ruin! p)
  (promise/ruined! p)
  (semaphore-post (promise/semaphore p)))

(define (promise/kept? p)   (promise/kept p))
(define (promise/ruined? p) (promise/ruined p))
;; Return the remaining lifespan of the promise in milliseconds >= 0.
(define (promise/lifespan/remaining p) (max 0 (- (promise/deadline p) (current-inexact-milliseconds))))

(define (timespan/milliseconds days hours minutes seconds . milliseconds)
  (+
   (if (null? milliseconds) 0 (car milliseconds))
   (* seconds 1000)
   (* minutes 60 1000)
   (* hours 60 60 1000)
   (* days 24 60 60 1000)))

;; lifespan - lifespan of promise in real seconds (for example 5.003 is 5003 milliseconds).
(define (promise/virgin lifespan)
  (vector '<promise>
          (make-semaphore 0) ; Semaphore for threads waiting on promise.
          lifespan           ; Total lifespan in seconds.
          (+                 ; Deadline for promise resolution.
           (* lifespan 1000)
           (current-inexact-milliseconds)) 
          #f   ; #t iff promise has been kept (resolved).
          #f   ; #t iff promise has been ruined (unresolved prior to deadline)
          #f)) ; Value when resolved.



;; Returns the thunk to be executed by the nanny actor assigned to promise p.
(define (nanny/thunk p)
  (lambda ()
      (if (sync/timeout (promise/lifespan p) (thread-receive-evt))
          ; Be paranoid and (weakly) confirm the structure of the CURL.
          (let ((m (thread-receive)))
            (if (and (pair? m) (curl? (car m)))
              (promise/keep! p (cdr m)) ; The promise is complete.
              (promise/ruin! p)))       ; Something is wrong.
          
          ; Deadline was tripped.
          (when (not (promise/kept? p))
            (promise/ruin! p)))))

;; Returns the pair (p . r) where p is the promise and r is the resolver of p.
;; lifespan - the maximum lifespan (in real seconds) of the promise
;; Note: By adding additional arguments for delegation predicates senders/intra and senders/inter we can
;; control the distribution and exercise of the CURL and hence the resolver.
(define (promise/new lifespan)
  (assert/type lifespan
               (lambda (n) 
                 (and (number? n) (positive? n) (< n +inf.f)))
               promise/new "positive real")

  (let-values ([(nanny nanny/locative) (actor/new PROMISSARY (gensym 'nanny))])
    (let* ((x (locative/cons/any
               nanny/locative 
               (+ (current-inexact-milliseconds) (* 1000 lifespan))
               1 #t #t))

           (p (promise/virgin lifespan))

           (c (curl/new/any x '(promise resolve) #f)))

           ;(resolver (lambda (v) (curl/send c v)))) ; The resolver should actually be in Motile so that it is exportable! FIX LATER!
      (actor/jumpstart nanny (nanny/thunk p))
      ;(log-info (format "promise/new: ~a\n\n" (curl/pretty c)))
      ;(cons p resolver))))
      (cons p c))))

;; Wait on promise p.
;; patience - a postive number giving the span of time (in real seconds) that the calling thread (actor) is willing to wait
;; failure - the value returned by wait if: the promise is not kept by the caller's deadline, or if the promise has been ruined.
;; Returns the value of the promise if the promise was kept and returns failure if the promise has been ruined or
;; if the caller's patience is exhausted.
(define (promise/wait p patience failure)
  (assert/type patience
               (lambda (n) (and (number? n) (positive? n) (< n +inf.f)))
               promise/wait "positive real")
  (cond
    ((promise/kept p)   (promise/value p))
    ((promise/ruined p) failure)
    ((sync/timeout patience (semaphore-peek-evt (promise/semaphore p)))
     (semaphore-wait (promise/semaphore p))
     (let ((v (cond
                ((promise/ruined? p) failure)
                ((promise/kept? p)   (promise/value p))
                (else                failure))))
       (semaphore-post (promise/semaphore p))
       v))
    (else failure))) ; We lost patience.

;; Given a local promise p construct a remote promise derived from p for another island.
;(define (promise/remote/new island clan actor p)
;  (and
;   (promise? p)
;   (vector '<promise/remote> island clan (promise/id p))))

(define-values (PROMISSARY LOCATIVE/PROMISSARY) (actor/chieftain/new ROOT 'promissary))


;; The promissary is a unique clan responsible for the resolution of promises on the island.
;; When a promise/resolver pair (p . r) is created the promise id, p.id and the resolver r are
;; transmitted as a key/value pair p.id/r to the promissary which maintains a hash table whose keys are promise ids
;; and whose values are promise resolvers.
;; Resolving a promise requires sending a PUT message to the promissar whose path is the singleton list (p.id).
;; The content body of the PUT message is the value of the resolution.
;; The promissary, on receiving such a message, looks up the resolver, applies the resolver
;(define (promissary/new)
;  ; Returns #t if x is a (promise . resolver) pair and #f otherwise.
;  (define (promise/pair? x)
;    (and (pair? x) (promise? (car x)) (procedure? (cdr x))))
;
;  ; To add a promise PUT #() with payload (promise . resolver)
;  ; To resolve a promise to value v PUT #(id) with payload v
;  (define (promissary/PUT registry command)
;    (let* ((curl    (message/ask/curl command))
;           (payload (message/ask/body command))
;           (path    (curl/path curl)))
;      (cond
;        ((zero? (tuple/length path))
;         (if (promise/pair? payload)
;             (hash/cons registry (promise/id (car payload)) (cdr payload)) ; Add the promise resolver to the registry.
;             registry))
;
;        ((and
;          (= 1 (tuple/length path))
;          (bytes? (tuple/ref path 0))
;          (hash/ref registry (tuple/ref path 0) #f))
;         => ; Resolve the promise
;         (lambda (resolver)
;           (resolver payload) ; Resolve the promise.
;           (hash/remove registry (tuple/ref path 0)))) ; Remove the promise resolver from the registry.
;
;        (else registry)))) ; Ignore the PUT altogether and return the registry unchanged.
;
;  (define (promissary/GET registry total command)
;    (define /statistics (tuple #"statistics"))
;
;    (let ((path (curl/path (message/ask/curl command))))
;      (cond
;        ((equal? /statistics path)
;         ; Respond with basic statistics.
;         (send
;          (message/ask/reply command)
;          (message/tell/new 2xx/OK (tuple total (hash/length registry)) #f (message/ask/echo command))))
;        (else
;         (send
;          (message/ask/reply command)
;          (message/tell/new 4xx/Not-Found (message/ask/echo command)))))))
;         
;    
;  (thread
;   (lambda ()
;     (let loop ((registry hash/equal/null) (total 0) (command (thread-receive)))
;       (if (message/ask? command)
;           (case (message/ask/method command)
;             ((PUT) ; Add a promise or resolve a promise.
;              (loop (promissary/PUT registry command) (add1 total) (thread-receive)))
;             ((GET) ; Generate some simple statistics.
;              (promissary/GET registry total command)
;              (loop registry total (thread-receive))))
;           
;           ; Ignore everything else.
;           (loop registry total (thread-receive)))))))
;              
;
;
;  

;; Actors a1 and a2 will both see the promise resolved.
(define (test/promise/1)
  (let*-values ([(a1 l/a1)     (actor/new ROOT 'a1)]
                [(a2 l/a2)     (actor/new ROOT 'a2)]
                [(a3 l/a3)     (actor/new ROOT 'a3)])
    (let* ((x (promise/new 10.0))
           (p (car x))
           (c (cdr x)))
      (actor/jumpstart a1 (lambda () (log-info (format "a1: ~a\n\n" (promise/wait p 2.0 "darn")))))
      (actor/jumpstart a2 (lambda () (log-info (format "a2: ~a\n\n" (promise/wait p 2.0 "rats")))))
      (actor/jumpstart
       a3
       (lambda ()
         (sleep 1.5)
         (log-info "a3: resolving promise\n\n")
         (curl/send c "FOOBAR"))))))

;; Actor a1 will see the promise resolved but actor a2 will timeout before that.
(define (test/promise/2)
  (let*-values ([(a1 l/a1)     (actor/new ROOT 'a1)]
                [(a2 l/a2)     (actor/new ROOT 'a2)]
                [(a3 l/a3)     (actor/new ROOT 'a3)])
    (let* ((x (promise/new 10.0))
           (p (car x))
           (c (cdr x)))
      (actor/jumpstart a1 (lambda () (log-info (format "a1: ~a\n\n" (promise/wait p 2.0 "darn")))))
      (actor/jumpstart a2 (lambda () (log-info (format "a2: ~a\n\n" (promise/wait p 0.5 "rats")))))
      (actor/jumpstart
       a3
       (lambda ()
         (sleep 1.5)
         (log-info "a3: resolving promise\n\n")
         (curl/send c "FOOBAR"))))))

;; Actors a1 and a2 will both timeout before the promise is resolved.
(define (test/promise/3)
  (let*-values ([(a1 l/a1)     (actor/new ROOT 'a1)]
                [(a2 l/a2)     (actor/new ROOT 'a2)]
                [(a3 l/a3)     (actor/new ROOT 'a3)])
    (let* ((x (promise/new 3.0))
           (p (car x))
           (c (cdr x)))
      (actor/jumpstart a1 (lambda () (log-info (format "a1: ~a\n\n" (promise/wait p  0.75 "darn")))))
      (actor/jumpstart a2 (lambda () (log-info (format "a2: ~a\n\n" (promise/wait p  0.5 "rats")))))
      (actor/jumpstart
       a3
       (lambda ()
         (sleep 1.5)
         (log-info "a3: resolving promise\n\n")
         (curl/send c "FOOBAR"))))))

;; The promise will be ruined before actors a1 and a2 loose patience.
(define (test/promise/4)
  (let*-values ([(a1 l/a1)     (actor/new ROOT 'a1)]
                [(a2 l/a2)     (actor/new ROOT 'a2)])
    (let* ((x (promise/new 2.0))
           (p (car x))
           (c (cdr x)))
      (actor/jumpstart a1 (lambda () (log-info (format "a1: ~a\n\n" (promise/wait p  1.5 "darn")))))
      (actor/jumpstart a2 (lambda () (log-info (format "a2: ~a\n\n" (promise/wait p  1.5 "rats"))))))))
