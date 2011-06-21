;; Copyright 2009 Michael M. Gorlick

(define-generic :queue)
(define-generic :queue!)
(define-generic :uri)
(define-generic :uri!)
(define-generic mailbox/new)
(define-generic mailbox/local?)
(define-generic mailbox/remote?)
(define-generic mailbox/empty?)
(define-generic mailbox/nonempty?)
(define-generic mailbox/capacity)
(define-generic mailbox/backlog)
(define-generic mailbox/uri)
(define-generic mailbox/send!)
(define-generic mailbox/receive!)
(define-generic mailbox/filter!)

;; Mailbox for inter-fiber messages.
(define-nongenerative-class (<mailbox>) |4f576f65-5c59-451f-9bcd-e21e169af828|
  (queue :queue :queue!) ; Thread-safe queue for message delivery.
  (uri :uri :uri!))      ; http://host:port/uuid (global unique URI for mailbox).

(add-methods
 initialize
 (list
   ; Unbounded mailbox.
  (method
   ((<mailbox> m))
   (:queue! m (queue/new #t))
   (:uri!   m #f))

  ;; Bounded mailbox with maximum capacity n > 0.
  (method
   ((<mailbox> m) (<number> n))
   (:queue! m (queue/new #t n))
   (:uri!   m #f))

   ; Unbounded mailbox with address.
  (method
   ((<mailbox> m) (<java.net.uri> u))
   (:queue! m (queue/new #t))
   (:uri!   m u))
  
  ;; Bounded mailbox with address and maximum capacity n > 0.
  (method
   ((<mailbox> m) (<java.net.uri> u) (<number> n))
   (:queue! m (queue/new #t n))
   (:uri!   m u))))

;; Returns #t if the mailbox is local and #f if it is remote (a URI).
(add-methods
 mailbox/local?
 (list
  (method ((<mailbox> m)) #t)

  (method ((<java.net.uri> m)) #f)))

(add-methods
 mailbox/new
 (list
   ; Unbounded mailbox.
  (method () (make <mailbox>))

  ;; Bounded mailbox with maximum capacity n > 0.
  (method ((<number> n)) (make <mailbox> n))

   ; Unbounded mailbox with address.
  (method ((<java.net.uri> u)) (make <mailbox> u))
  
  ;; Bounded mailbox with address and maximum capacity n > 0.
  (method ((<number> n) (<java.net.uri> u)) (make <mailbox> n u))))

;; Returns #t if the mailbox is remote (a URI) and #f if it is local.
(add-methods
 mailbox/remote?
 (list
  (method ((<mailbox> m)) #f)

  (method ((<java.net.uri> m)) #t)))

;; Returns #t if the mailbox is empty (no waiting messages) and #f otherwise.
(add-method mailbox/empty? (method ((<mailbox> m)) (queue/empty? (:queue m))))

;; Returns #t if the mailbox is nonempty (contains waiting messages) and #f otherwise.
(add-method mailbox/nonempty? (method ((<mailbox> m)) (queue/nonempty? (:queue m))))

(add-method mailbox/capacity (method ((<mailbox> m)) (queue/capacity (:queue m))))

;; Returns the number (n >= 0) of messages waiting in the mailbox.
(add-method mailbox/backlog (method ((<mailbox> m)) (queue/occupancy (:queue m))))

;; (! m x) deposits a message x in mailbox m.
(add-methods
 mailbox/send!
 (list
  ; Deliver a message x to local mailbox m.
  (method ((<mailbox> m) (<value> x)) (queue/put! (:queue m) x))

  ; Deliver a message x to a remote mailbox (denoted by a URI).
  ; (this-dispatch) is a thread-specific parameter returning the
  ; local mailbox of the CREST dispatcher for the thread. 
  (method ((<java.net.uri> to) (<value> x)) (queue/put! (this-dispatch) `(relay ,to ,x)))))

;; In Serf, each fiber has a fiber-specific parameter (this-mailbox) that either returns #f
;; or a mailbox m (#f means that no mailbox was assigned to the fiber). The variants of
;; (? ...) allow a fiber to read from either an explicit mailbox or its implicit mailbox.
(add-methods
 mailbox/receive!
 (list
  ; Blocking read of the oldest message in the mailbox.
  (method ((<mailbox> m)) (queue/take! (:queue m)))

  ; Blocking read of an "implicit" local, thread-specfic mailbox.
  (method () (queue/take! (:queue (this-mailbox))))

  ; Blocking read of the oldest message in mailbox m with a timeout (in milliseconds).
  ; The default is returned if we wait longer than timeout milliseconds.
  (method
   ((<mailbox> m) (<number> timeout) (<value> default))
   (queue/take! (:queue m) default timeout))

  ; Blocking read of the oldest message in the "implicit" mailbox with a timeout (in milliseconds).
  ; The default is returned if we wait longer than timeout milliseconds.
  (method 
   ((<number> timeout) (<value> default)) (queue/take! (this-mailbox) default timeout))))

;; The URI of a mailbox has the form http://host:port/uuid.
;; Returns an instance of <java.net.uri> or #f indicating that no URI denotes this mailbox.
(add-method  mailbox/uri (method ((<mailbox> m)) (:uri m)))

(add-methods
 mailbox/filter!
 (list
  ; Blocking read of the oldest message in the mailbox that satisfies predicate f.
  (method ((<mailbox> m) (<procedure> f)) (queue/take! (:queue m) f))

  ; Blocking read of the oldest message in the "implicit" mailbox that satisfies predicate f.
  (method ((<procedure> f)) (queue/take! (:queue (this-mailbox)) f))

  ; Return the oldest message in the mailbox that satisfies predicate f.
  ; If no such message exists then return the given default value.
  (method ((<mailbox> m) (<procedure> f) (<value> default)) (queue/take! (:queue m) f default))

  ; Blocking read of the oldest message in the mailbox that satisfies predicate f.
  ; Blocks for at most timeout milliseconds. If no such message is found within timeout milliseconds
  ; then the default value is returned.
  (method
   ((<mailbox> m) (<procedure> f) (<number> timeout) (<value> default))
   (queue/take! (:queue m) f default timeout))

  ; Blocking read of the oldest message in the "implicit" mailbox that satisfies predicate f.
  ; Blocks for at most timeout milliseconds. If no such message is found within timeout milliseconds
  ; then the default value is returned.
  (method
   ((<procedure> f) (<number> timeout) (<value> default))
   (queue/take! (:queue (this-mailbox)) f timeout default))))

