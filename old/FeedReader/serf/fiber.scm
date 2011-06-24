
(define-generic :thread)
(define-generic :thread!)
(define-generic :environ)
(define-generic :environ!)
(define-generic :moniker)
(define-generic :moniker!)
(define-generic :thunk)
(define-generic :thunk!)
(define-generic :links)
(define-generic :links!)
(define-generic :trap-exceptions)
(define-generic :trap-exceptions!)

(define-nongenerative-class (<fiber>) |4706915e-fe87-444e-92f1-3251fe0900bd|
  (thread  :thread :thread!)   ; Scheme thread animating this fiber.
  (environ :environ :environ!) ; Global binding chain for fiber.
  (moniker :moniker :moniker!) ; Fiber name (for debugging and logging).
  (links   :links :links!)     ; Interfiber links for remote error and event notification.
  (thunk   :thunk :thunk!))    ; Thunk executed by this fiber.

(define this-dispatch  (make-parameter #f)) ; Mailbox of dispatcher for message relays to/from external sites.
(define this-fiber     (make-parameter #f)) ; Object instance of this fiber.
(define this-mailbox   (make-parameter #f)) ; Mailbox assigned to this fiber.
(define this-uri       (make-parameter #f)) ; URI under which the fiber is executing (if any).
(define this-log       (make-parameter #f)) ; The log (if any) for this fiber.
(define this-authority (make-parameter #f)) ; The authority (host . port) for this fiber.

(define-generic fiber/new)

(define (%fiber-default-error-handler awareness error-continuation)
;   (display "%fiber-default-error-handler CALLED\n")
;   (display (string-append (->string (this-log)) "\n"))
;   (display (error-message awareness)) (newline)
  (let ((location (or (error-location awareness) "error location unknown"))
	(message  (or (error-message awareness)  "error message unknown")))
    (log/audit (this-log) (format "~a: ~a" location message))
    #f))

; When called by a running fiber f causes it to sleep for m millisconds.
; Returns m, the length of the snooze, in milliseconds.
(define (fiber/snooze m) (sleep m) m)

;; Start the execution of fiber f.
(define (fiber/start  f) (thread/start (:thread f)))

; Set the name of fiber f (for debugging and logging).
; The name n may be any Scheme object though a string or symbol is recommended practice.
(define (fiber/moniker! f name) (:moniker! f name))

; Return the name of fiber f (for debugging and logging).
(define (fiber/moniker f) (:moniker f))

(add-methods
 initialize
 (list
  ; Make a fiber f with the given thunk but no mailbox hence, no other thread can communicate
  ; with f. Used to implement fibers spun up by REMOTE from one authority to another.
  (method
   ((<fiber> f) (<procedure> thunk))
   (:moniker! f #f)
   (:thunk!   f thunk)
   (:thread!  f 
    (parameterize
     ((this-fiber f))
     (thread/new
      (lambda ()
	(with/fc %fiber-default-error-handler thunk)))))
  (:links! f '())
  (thread/daemon! (:thread f) #t)) ; Shutdown authority if only daemon threads are alive.

  ; Make a fiber f with the given thunk and mailbox m.
  ; Used to implement fibers spun up by SPAWN from one authority to another.
  (method
   ((<fiber> f) (<procedure> thunk) (<mailbox> m))
  (:moniker! f #f)
  (:thunk!   f thunk)
  (:thread!  f 
    (parameterize
     ((this-fiber f)
      (this-mailbox m))
     (thread/new
      (lambda ()
	(with/fc %fiber-default-error-handler thunk)))))
  (:links! f '())
  (thread/daemon! (:thread f) #t)))) ; Shutdown authority if only daemon threads are alive.

(add-methods
 fiber/new
 (list
  (method ((<procedure> thunk)) (make <fiber> thunk))
  (method ((<procedure> thunk) (<mailbox> m)) (make <fiber> thunk m))))



;; (this-url) deserves explanation since it is at the heart of the CREST perspective.
;; Each fiber executes in the context of a particular URL-specific global binding environment G.
;; The authority-specific URL denoting G is a string "/a/b/.../z/".








;; Regression test.
;; Test the basics: fiber creation and messaging in the same address space.
; (define (fiber-ping-display)
;   (display "Fiber started\n\n")
;   (let loop ((x (? (this-mailbox))))
;     (display x) ; Print the message.
;     (newline)
;     (loop (? (this-mailbox))))) ; Wait for another message from the campanion fiber.

; (define (fiber-ping-send)
;   (display "Fiber started\n\n")
;   (let loop ((n 0))
;     (display (list "hello" n))
;     (newline)
;     (! (this-mailbox) `(PING ,n)) ; Send a message to the other fiber.
;     (snooze 1000) ; Sleep for 1000 milliseconds (1 second).
;     (loop (+ n 1))))

; (define (fiber-ping-test)
;   (let* ((m (make <mailbox>))
; 	 (f1 (make <fiber> ping-display m)) ; Create two fibers, f1 and f2, that share a mailbox m.
; 	 (f2 (make <fiber> ping-send m)))   ; f1 reads from mailbox m and f2 writes into mailbox m.
;     (start f1)
;     (start f2)))

