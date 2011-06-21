(import generic-procedures)
(import oo)
(import threading)
(import type-system)
(import pattern-matching) ; For (match ...)
(import hashtable)

;; Time/date library.
(require-library 'sisc/libs/srfi/srfi-19)
(import srfi-19)

(define (java-moniker j) (snip-package-name (java-to-string j)))
(define (java-to-string j) (->string (to-string j)))

(define-record-type <outgoing-ticket>
  (make-outgoing-ticket authority request tag reply id)
  <outgoing-ticket>?
  (authority :ticket-authority)           ; Target authority as string DNS/IP:port, for example www.example.com/69.54.221.17:8080
  (request   :ticket-request)             ; <java-http-request> instance.
  (tag       :ticket-tag)                 ; Object assigned by requester to distinguish this request from others.
  (reply     :ticket-reply)               ; <mailbox> instance to which response is delivered.
  (id        :ticket-id)                  ; Unique integer assigned by HTTP client.
  (sent      :ticket-sent :ticket-sent!)) ; Timestamp when request transmitted by HTTP client.

;; Return the id of ticket t as a string.
(define (ticket-id t) (number->string (:ticket-id t)))

(define-generics
  :user-agent :user-agent!
  :logger :logger!
  :outgoing :outgoing!
  ;:pending :pending!
  :inbox :inbox!
  :id-counter :id-counter!
  :request-responder :request-responder!
  :connection-listener :connection-listener!
  :session-responder :session-responder!
  ;:sessions :sessions!
  :connections :connections!
  :java-http-client :java-http-client!)

(define-class (<http-client>)
  ; Symbol or string identifying HTTP client to an HTTP server.
  (user-agent :user-agent :user-agent!)

  ; Logger instance for this HTTP client.
  (logger :logger :logger!)

  ; Hash table of authority/queue (A/Q) pairs for outbound HTTP requests. For each authority A queue Q contains
  ; the tickets for all HTTP requests targeted at A that are waiting to be sent.
  (outgoing :outgoing :outgoing!)

  ; Hash table of <java-http-context>/<queue> (c/q) pairs.
  ; For each context c (there is a one-to-one correspondence between connections
  ; and contexts) queue q contains all pending HTTP requests transmitted on the connection affliated with c,
  ; that is, those requests that have been sent to a server but for which no response has yet been received.
  ;(pending :pending :pending!)

  ; Hash table of authority/<java-default-nio-http-connection> (A/C) pairs where C is the connection to authority A.
  (connections :connections :connections!)

  ; Mailbox for requests sent to HTTP client by user threads.
  (inbox :inbox :inbox!)

  ; Counter for unique ticket ids.
  (id-counter :id-counter :id-counter!)

  ; Request responder responsible for sending requests and dispatching responses.
  (request-responder :request-responder :request-responder!)

  ; Connection event listener used to log connection events.
  (connection-listener :connection-listener :connection-listener!)

  ; Session responder responsbile for session management (for now used only for logging session events).
  (session-responder :session-responder :session-responder!)

  ; The Java NIO HTTP client.
  (java-http-client :java-http-client :java-http-client!))

(define-method (initialize (<http-client> self) (<java-apache-logger> logger))
  (:user-agent! self "CREST/Imposter 1.0")
  (:logger!     self logger)
  (:outgoing!            self (make-hashtable string=? hash-by-string= #t #f)) ; Thread-safe, strong key references.
  ;(:pending!             self (make-hashtable eqv? hash-by-eqv #t #t))         ; Thread-safe, weak key references.
  (:connections!         self (make-hashtable string=? hash-by-string= #t #f)) ; Thread-safe, strong key references.
  (:id-counter!          self 1)
  (:inbox!               self (make <mailbox>))
  (:request-responder!   self (proxy-http-request-responder self))
  (:connection-listener! self (proxy-connection-listener self))
  (:session-responder!   self (proxy-http-session-responder self))
  ;(:sessions!            self (make-hashtable string-ci=? hash-by-string-ci= #f #f)) ; Not thread-safe, strong key references.

  (:java-http-client!    self (make-nio-http-client (:user-agent self) (:request-responder self) (:connection-listener self)))
)

(define-generics
  dispatch
  bump
  finalize-context
  guarantee-outgoing
  guarantee-pending
  guarantee-session
  enqueue-outgoing
  next-outgoing-ticket
  schedule-connection
  subschedule-connection
  schedule-ticket
  submit-request
  handle-response)

;; Loop indefinitely queueing up any HTTP requests that you receive.
(define-method (dispatch (<http-client> self))
  (audit-http-client (:logger self) "dispatch" "start") ; Mark in the log when Imposter started.
  (match
   (? (:inbox self))

   ;; GET messages to imposter have the format (method uri tag reply) where
   ;; method: the HTTP method as a symbol
   ;; uri: a <java.net.uri> instance defining the authority, path, and query (if any), for the request
   ;; tag: a user-specific tag (value) identifying the request (used by the user to distinguish one response from another)
   ;; reply: the <mailbox> instance to which the HTTP response is sent
   ((get ,uri ,tag ,reply)
    (guard (and (legal-http-uri? uri) (instance-of? reply <mailbox>)))
    ;(serf-audit (:logger self) (->string (to-string uri)))
    ;(serf-audit (:logger self) (->string (to-string (make-http-get-request uri))))
    (let* ((canonical (uri-to-canonical-authority uri))
	   (request (make-http-get-request uri))
	   (ticket (make-outgoing-ticket canonical request tag reply (bump self))))
      ;(guarantee-outgoing self uri)
      ;(put! (hashtable/get (:outgoing self) authority) ticket) ; Enqueue the ticket for transmission to the server.
      ;(guarantee-session self uri)
      (schedule-ticket self ticket)
      (schedule-connection self canonical uri)
      (audit-http-client (:logger self) "dispatch" (string-append "GET " canonical " " (:path uri)))))

   ((relay ,uri ,reply)
    (audit-http-client (:logger self) "dispatch" "RELAY not implemented"))

   ((,method ,uri ,tag ,reply)
    (audit-http-client (:logger self) "dispatch" (string-append (string-upcase (always-string method)) " not implemented")))

   (,x (audit-http-client (:logger self) "dispatch" "unknown message"))
   )

  (dispatch self) ; Loop around and wait for the next dispatch message.
)

;; Increment id-counter by one returning the prior value.
(define-method (bump (<http-client> self))
  (let ((n (:id-counter self)))
    (:id-counter! self (+ n 1))
    n))

;; Ensures canonical authority (DNS/IP:host string) is installed as key in appropriate tables.
;; Enqueues ticket for transmission to authority.
;; Returns authority as value.
(define-method (schedule-ticket (<http-client> self) (<outgoing-ticket> ticket))
  (let ((authority (:ticket-authority ticket)))
    ; Ensure authority/queue pair appears in :outgoing table and enqueue ticket.
    (put!
     (hashtable/get! (:outgoing self) authority (lambda () (make <queue-1-n>)) #f) ; #f indicates that thunk is safe.
     ticket)
    (audit-http-client (:logger self) "schedule-ticket" (string-append authority " ticket:" (ticket-id ticket)))))

;; If no connection to authority exists then establish one.
;; If a connection exists and is viable then kick it.
;; If a connection exists and it is dead then try to replace it with a fresh connection.
;; If a ticket is enqueued for authority A and connection establishment
;; for A fails then the ticket will stay enqueued until another ticket arrives for A and stimulates another
;; round of connection establishment. This can be regarded as either a useful feature or a bug.

;; One solution is to have a nanny thread periodically tour the :outgoing table, authority by authority, attempting
;; to (re)establish a connection for any authority with enqueued tickets for which no viable connection exists
;; (not implemented here).

;; No current connection for the authority so (re)establish a connection.
;; Put a placeholder authority/#t pair in :connections. The :connection-listener will replace
;; #t with a fresh connection when it becomes available.
(define-method (subschedule-connection (<http-client> self) (<string> authority) (<java.net.uri> uri))
  (hashtable/put! (:connections self) authority #t) ; Placeholder in :connections table.
  ; Contact the bottom NIO layer and attempt to establish a session for authority.
  ; Ignore the session-request returned by open-session.
  (open-session (:java-http-client self) (get-host uri) (get-port uri) (:session-responder self))
  (audit-http-client (:logger self) "schedule-connection" (string-append authority " (re)open")))

(define-method (schedule-connection (<http-client> self) (<string> authority) (<java.net.uri> uri))
  (let ((c (hashtable/get (:connections self) authority)))
    (cond
     ((boolean? c)
      (if (not c)
	  (subschedule-connection self authority uri))) ; No connection for the given authority. Attempt to open one.
	  ; Otherwise, connection (re)establishment is underway.

     ((instance-of? c <java-default-nio-http-client-connection>) ; There is a connection for the authority.
      (cond
       ((connection-alive? c)       ; Is the connection still viable?
	(request-output connection) ; Kick the connection to notify us (via connection-listener) when ready for feeding.
	(audit-http-client (:logger self) "schedule-connection" (string-append authority " kicked")))

       (else ; Alas the connection is dead (was closed or timed out)! Attempt to reopen it.
	(subschedule-connection self authority uri))))

      (else ; Should never get here.
       (audit-http-client (:logger self) "schedule-connection" (string-append authority " unknown connection type"))))))

;      ((type<= flavor <boolean>) ; c must be #f. No such authority in :connections so (re)establish a connection.
;        ; Put a placeholder authority/session pair in :connections. The :connection-listener will replace
;        ; the session with a fresh connection when it becomes available.
;        (hashtable/put! 	(:connections self) authority (open-connection self host port))
;        (audit-http-client (:logger self) "schedule-connection" (string-append "session " authority)))

;       ((type<= flavor <java-session-request>) #t) ; Connection (re)establishment is in progress so just leave everything alone.

;       ((type<= flavor <java-default-nio-http-client-connection>) ; There is a connection available for this authority.
;        (cond
; 	((connection-alive? c)       ; Is the connection still viable?
; 	 (request-output connection) ; Kick the connection to notify (via connection-listener) when ready for feeding.
; 	 (audit-http-client (:logger self) "schedule-connection" (string-append "kicked " authority)))

; 	(else ; Alas the connection is dead (was closed or timed out)! Attempt to reopen it.
; 	 ; Put a placeholder authority/session pair in :connections. The :connection-listener will replace
;          ; the session with fresh connection when it becomes available. If session establishment fails
;          ; for some reason (timeout or failure to connect) then the session-responder will delete the
;          ; pair authority/c from the :connections table.
; 	 (hashtable/put! (:connections self) authority (open-connection self host port))
; 	 (audit-http-client (:logger self) "schedule-connection" (string-append "session " authority)))))

;       (else ; Should never get here.
;       (audit-http-client (:logger self) "schedule-connection" (string-append authority " unknown connection type"))))))

;; Guarantee that :outgoing contains an authority/queue pair for the given authority.
;; Note that the queue must be thread safe since the thread running the <http-client> instance may be
;; adding a ticket to the queue while a thread inside <java-nio-http-client> may be simultaneously removing
;; a ticket from the queue.
(define-method (guarantee-outgoing (<http-client> self) (<java.net.uri> uri))
  (let ((authority (:authority uri))) ; host:port as Scheme string.
    (if (not (hashtable/contains? (:outgoing self) authority))
	(hashtable/put! (:outgoing self) authority (make <queue-1-n>))))) ; Add authority/queue pair to outgoing table.

;; Guarantee that :pending contains a context/queue pair for the given <java-http-context>.
;; Using the context as a key allows us to have multiple connections/sessions each with their own
;; pending request queue.
;; Since we can extract an authority from a context (using (to-authority context) defined in nio_http_client.scm)
;; we can track all of the pending requests for all connections for an authority.
;; This method is called only by the request-responder given to the nio-http-client so we can guarantee that the
;; :pending hash table and the queues are accessed by only one thread at a time.
; (define-method (guarantee-pending (<http-client> self) (<java-http-context> context))
;   (hashtable/get! (:pending self) context (lambda () (make <queue-1-n>)) #f) ; Thunk is safe.
;   ;(if (not (hashtable/contains? (:pending self) context))
;   ;    (hashtable/put! (:pending self) context (make <queue-1-n>)))
;   (display "guarantee-pending\n")
;   (display (hashtable/keys (:pending self))) (newline)
;   (newline)
;   (audit-http-client (:logger self) "guarantee-pending" (string-append "context:" (java-object-to-id context))))

(define (uri-to-canonical-authority u)
  (let ((address (java-new <java-internet-socket-address> (get-host u) (get-port u))))
    (internet-socket-address-to-authority address)))

;; If no connection is currently open to the authority 
; (define-method (guarantee-connection (<http-client> self) (<java.net.uri> uri))
;   (let ((authority (uri-to-canonical-authority uri)))
;     (if (not (hashtable/contains? (:connections self) authority))
; 	(begin
; 	  ; Establish a session (which we will ignore hereafter) to force connection establishment.
; 	  (open-session (:java-http-client self) (get-host uri) (get-port uri) (:session-responder self))
; 	  ; The connection will be registered in the :conections hashtable by the :connection-listener.
; 	  (audit-http-client (:logger self) "guarantee-connection" authority)))))

;; Guarantee that :sessions contains an authority/session pair for the given authority.
;; NOTE: This is quite incomplete as there is no checking to guarantee session liveness over an extended time period.
; (define-method (guarantee-session (<http-client> self) (<java.net.uri> uri))
;   (let* ((authority (:authority uri))) ; host:port as Scheme string.
;     (if (not (hashtable/contains? (:sessions self) authority))
; 	(let ((session (open-session (:java-http-client self) (get-host uri) (get-port uri) (:session-responder self))))
; 	  (hashtable/put! (:sessions self) authority session)
; 	  (audit-http-client
; 	   (:logger self) "guarantee-session"
; 	   (string-append authority " " (snip-package-name (->string (to-string session)))))))))

(define-method (finalize-context (<http-client> self) (<java-http-context> context))
  (display "<http-client>.finalize-context\n")
  (hashtable/remove! (:pending self) context)) ; Any pending requests affilated with this context are lost forever.

;; Pick up the next outgoing ticket for the given authority.
;; Returns the ticket (if any) or #f.
(define-method (next-outgoing-ticket (<http-client> self) (<string> authority))
  (let ((queue (hashtable/get (:outgoing self) authority)))
    ;(display (list "next-outgoing-ticket" (if queue (backlog queue) queue))) (newline)
    (and queue (take! queue #f))))

; (define-method (submit-request (<http-client> self) (<string> authority))
;   (let ((ticket (next-outgoing-ticket self authority))
; 	(queue (hashtable/get (:pending self) context)))
;     (audit-http-client (:logger self) "submit-request" (string-append authority " " (java-moniker context)))))


;; If a <java-http-request> request r is waiting for to be sent to the authority represented
;; by the given context then return r. If no such request is waiting then return the null <java-http-request>.
(define-method (submit-request (<http-client> self) (<java-http-context> context))
  (let* ((authority (context-to-authority context))
	 (ticket (next-outgoing-ticket self authority))
	 ;(queue (hashtable/get (:pending self) context))
	 (queue (java-unwrap (get-attribute context :imposter.pending:)))
	 (id (java-object-to-id context)))

    (cond
     ((not queue) ; This should never happen.
      (audit-http-client (:logger self) "submit-request"  (string-append authority " context:" id " no queue"))
      :null-http-request:)

     (ticket
      (audit-http-client
       (:logger self) "submit-request" (string-append authority " context:" id " ticket:" (ticket-id ticket)))
      (put! queue ticket)
      (:ticket-sent! ticket (now))
      (:ticket-request ticket))

     (else
      (audit-http-client (:logger self) "submit-request" (string-append authority " context:" id " no ticket"))
      :null-http-request:)))) ; No waiting outgoing requests for the authority represented by this context.

(define-method (handle-response (<http-client> self) (<java-http-response> response) (<java-http-context> context))
;   (display "handle-response\n")
;   (display (hashtable/keys (:pending self))) (newline)
;   (display (context-to-authority context)) (newline)
;   (display (java-object-to-id context)) (newline)
;   (newline)
  (let* (;(queue (hashtable/get (:pending self) context))
	 (queue (java-unwrap (get-attribute context :imposter.pending:)))
	 (ticket (and queue (take! queue #f)))
	 (authority (context-to-authority context))
	 (id (java-object-to-id context)))
    (cond
     (ticket
      (! (:ticket-reply ticket) (list 'http/response (:ticket-tag ticket) response))
      (audit-http-client
       (:logger self) "handle-response"
       (string-append authority " context:" id " ticket:" (ticket-id ticket))))

     ((not queue)
      (audit-http-client
       (:logger self) "handle-response"
       (string-append authority " context:" id " no queue")))

     ((not ticket)
      (audit-http-client (:logger self) "handle-response" (string-append authority " context:"  id " no ticket"))))))

;; Exracting bits and pieces from a <java-http-response>.
;     (let* ((status-line (get-status-line response)) ; <java-status-line>
; 	   (status-code (->int (get-status-code status-line)))
; 	   (reason-phrase (->string (get-reason-phrase status-line)))
; 	   (entity (get-entity response))
; 	   (ticket (take! (hashtable/get (:pending imposter) http-context #f))))

;; Utility routines for Imposter.

(define (audit-http-client logger method message)
  (serf-audit logger (string-append "http-client." method ": " message)))

;; Construct an Apache HTTPComponents GET request
(define (make-http-get-request uri)
  (java-new
   <java-basic-http-request>
   :http-get-method:           ; Method for the request line (as Java String).
   (uri-to-request-path uri))) ; Path (+ query if any) for the request line (as Java String).

(define uri-to-http-get make-http-get-request) ; Synonym

;; Returns #t iff uri is a legitimate URL for the HTTP protocol.
(define (legal-http-uri? uri)
  (and
   (instance-of? uri <java.net.uri>)
   (string-ci=? "http" (:scheme uri)) ; Is it http://... ?
   (absolute? uri)))    ; The path must be absolute.

