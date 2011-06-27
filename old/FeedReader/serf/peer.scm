;; Copyright 2009 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(define :peer-default-port: 19519) ; The year/month of my (MMG) birthday, 1951/09/30.

;; Returns a local network address (as a string) if available and #f otherwise.
(define (who-am-i)
  (let* ((ip (get-local-host))
	 (dns (and ip (get-host-name-by-ip ip))))
    (cond
     (dns dns)
     (ip  ip)
     (else #f))))

(define peer/authority (make-generic-method))
(define peer/dispatch  (make-generic-method))
(define peer/host      (make-generic-method))
(define peer/log       (make-generic-method))
(define peer/port      (make-generic-method))
(define peer/sham      (make-generic-method))

(define peer/spawn        (make-generic-method))
(define peer/spawn/unsafe (make-generic-method))
(define peer/birth        (make-generic-method))
(define peer/mailbox      (make-generic-method))
(define audit             (make-generic-method))


(define (<serf/peer>)
  (let ((self (make-object))
	(host #f) ; IP or DNS host for peer.
	(port #f) ; IP port number for peer.
	(sham #f) ; Instance of Sham for peer.
	(imposter #f) ; Instance of Imposter for peer.
	(imposter/inbox (make <mailbox>)) ; Mailbox for requests to be issued by Imposter.
	(log-id (uuid/string)) ; Unique string identifier for peer's log.
	(log #f) ; Log instance for peer.
	(spawns (make-hashtable string=? hash-by-string= #t)) ; Maps mailbox uuid to spawn birth record.
	(this-authority #f))

    (make-method!
     self instantiate
     (lambda (self h p root) ; root: document root for Sham or #f if no static file serving.
       (set! host (or h (who-am-i) "localhost"))
       (set! port (or p :peer-default-port:))
       (set! log (log/new log-id (format "~a:~d" host port))) ; Log filename is host:port
       (set! sham     (object/forge <serf/sham> host port root log))
       (set! imposter (object/forge <serf/imposter> log imposter/inbox))
       (set! this-authority (make-parameter (cons host port)))
    ))

    ; Logging for the peer.
    (make-method!
     self audit
     (lambda (self method message)
       (log/audit log (format "serf/imposter.~a: ~a" method message))))

    (make-method!
     self peer/authority
     (lambda (self) (cons host port)))

    (make-method!
     self peer/host
     (lambda (self) host))

    (make-method!
     self peer/port
     (lambda (self) port))

    (make-method!
     self peer/dispatch
     (lambda (self) imposter/inbox))

    (make-method!
     self peer/log
     (lambda (self) log))

    (make-method!
     self peer/sham
     (lambda (self) sham))

     ; Returns the UUID of the mailbox assigned to the SPAWNed closure.
     ; The UUID is the key to the birth vector, #(fiber inbox timestamp), for the spawn where:
     ;   fiber: the fiber executing the thunk
     ;   inbox: the mailbox assigned to the spawn
     ;   timestamp: the birthtime of the spawn
     (make-method!
      self peer/spawn
      (lambda (self thunk)
	(let* ((inbox (make <mailbox>))
	       (fiber (fiber/new thunk inbox imposter/inbox))
	       (uuid  (uuid/string))
	       (birth (vector fiber inbox (now/utc))))
	  (hashtable/put! spawns uuid birth)
	  (fiber/start fiber) ; Start the thunk running in its own thread.
	  uuid)))

     (make-method!
      self peer/spawn/unsafe
      (lambda (self thunk)
	(let* ((inbox (make <mailbox>))
	       (fiber (fiber/new thunk inbox imposter/inbox self)) ; Unsafe use of peer.
	       (uuid  (uuid/string))
	       (birth (vector fiber inbox (now/utc))))
	  (hashtable/put! spawns uuid birth)
	  (fiber/start fiber) ; Start the thunk running in its own thread.
	  uuid)))

     (make-method!
      self peer/birth ; Given the mailbox uuid of a spawn return its birth vector [fiber mailbox timestamp].
      (lambda (self uuid)
	(hashtable/get spawns uuid #f)))

     (make-method!
      self peer/mailbox ; Given the mailbox uuid of a spawn return the mailbox object.
      (lambda (self uuid)
	(cond
	 ((hashtable/get spawns uuid #f) => (lambda (b) (vector-ref b 1)))
	 (else #f))))

    self))

(define :mailbox-prefix: "/mailbox/") ; Prefix for unique mailbox URI.
(define :uuid-length: 36)             ; Length in characters of a random UUID.

;; Given an URI "/mailbox/UUID" or /mailbox/UUID/... extract the UUID string and return it.
;; Return #f if the uri does not have the expected structure.
(define (mailbox/uuid uri)
  (cond
   ((string-prefix? :mailbox-prefix: uri)
    (let* ((tail (string-drop uri (string-length :mailbox-prefix:))) ; Drop "/mailbox/"
	   (i (string-index tail #\/))) ; Find the first / following "/mailbox/".
      ;(display (format "i:~a tail:~a" i tail))(newline)
      (cond
       ((and i (= i :uuid-length:)) (substring tail 0 i)) ; We found UUID/... so return the UUID.
       ((and (not i) (= (string-length tail) :uuid-length:)) tail) ; No / was found following "/mailbox/" so does the tail length fit?
       (else #f)))))) ; Whatever followed /mailbox/ was not a UUID.

;; Strip the prefix /mailbox/UUID from the URI and return the tail (if any) or /.
(define (mailbox/path uri)
  (cond
   ((string-prefix? :mailbox-prefix: uri)
    (let* ((tail (string-drop uri (string-length :mailbox-prefix:)))
	   (i (string-index tail #\/)))
      (cond
       ((and i (= i :uuid-length:)) (string-drop tail i)) ; We found UUID/... so return /...
       ((and (not i) (= (string-length tail) :uuid-length:)) "/") ; URI was /mailbox/UUID and nothing more so return /
       (else #f)))) ; Whatever followed /mailbox/ was not a UUID.
   (else #f))) ; URI prefix was NOT "/mailbox/"

(define :max-header-name:   64) ; Maximum length in characters of the name portion of an HTTP header.
(define :max-header-value: 128) ; Maximum length in characters of the value portion of an HTTP header.
(define :max-headers:       32) ; Maximum number of HTTP headers that may appear in a response.
(define :max-reason-phrase  32)
(define :max-sham-patience: 200) ; Maximum time (in milliseconds) Sham is willing to wait for response from spawn.
(define :max-payload:       (* 64 1024)) ; Maximum length in characters for a payload generated by a spawn.

;; Returns #t if s is a valid HTTP status code and #f otherwise
;; (where "valid" has a broad definition).
(define (http/status/valid? s)
  (and (integer? s) (>= s 100)))

;; Returns #t if reason phrase is valid and #f otherwise.
(define (http/reason/valid? r)
  (and (string? s) (<= (string-length r) :max-reason-phrase)))

;; Returns #t if a set of HTTP headers (an association list of (name . value) where
;; both name and value are strings) is valid and #f otherwise.
(define (http/headers/valid? headers)
  (and
   (list? headers)
   (< (length headers) :max-headers:)
   (all-true
    (lambda (pair)
      (and
       (pair? pair)
       (string? (car pair))
       (string? (cdr pair))
       (<= (string-length (car pair)) :max-header-name:)
       (<= (string-length (cdr pair)) :max-header-value:)))
    headers)))

;; Payload must be a string, nonempty and no longer than the :max-payload: limit.
(define (http/payload/valid? payload)
  (and
   (string? payload)
   (let ((n (string-length payload)))
     (and (< 0 n) (<= n :max-payload:)))))

;; Returns #t if predicate f is #t on each item in items list and #f otherwise.
(define (all-true? f items)
  (cond
   ((null? items) #t)
   ((f (car items)) (all-true? f (cdr items)))
   (else #f)))

;; Assemble an HTTP response from a reply message whose form is one of:
;;   (status reason headers payload)
;;   (status reason headers)
;; The second form is for responses for which no payload is required.
(define (assemble-http-response reply response)
  (display (format "assemble-http-response: path+body:~a\n" (message/path+body reply)))
  (match
   (message/path+body reply)
   (#(/http/response #(,status ,reason ,headers ,payload))

;     (guard ; For some reason this guard fails when it shouldn't!
;      (and
;       (http/status/valid? status)
;       (http/reason/valid? reason)
;       (http/headers/valid? headers)
;       (http/payload/valid? payload)))

    (display "assemble-http-response: about to status!\n")
    (http/response/status! response status)
    (display "assemble-http-response: about to reason!\n")
    (http/response/reason! response reason)
    (display "assemble-http-response: about to header!\n")
    (for-each
     (lambda (h) (http/response/header! response h))
     headers)
    (display "assemble-http-response: about to entity!\n")
    (http/response/entity! response payload))

   (#(/http/response #(,status ,reason ,headers))
    (guard 
     (and
      (http/status/valid? status)
      (http/reason/valid? reason)
      (http/headers/valid? headers)
      (http/payload/valid? payload)))
    (http/response/status! response status)
    (http/response/reason! response reason)
    (for-each
     (lambda (h) (http/response/header! response h))
     headers))

   (,_
    (http/response/status! response 500)
    (http/response/reason! response "Response Unacceptable")
    (http/response/entity! response "Response violated one or more Sham constraints."))))

;; Generates a responder for intercepting HTTP requests directed to paths /mailbox/UUID or /mailbox/UUID/* .
;; peer: the Serf peer responsible for managing the spawns.
(define (responder/mailbox/new)
  (define (unknown-mailbox m response uri uuid)
    (http/response/status! response 404)
    (http/response/reason! response "Not Found")
    (http/response/entity! response
      (if uuid
          (format "404 Not Found\r\nReasons:\r\n(1) UUID ~a unknown\r\n(2) Path /mailbox/~a... was withdrawn\r\n" uuid uuid)
          (format "404 Not Found\r\nReasons:\r\n URI ~a is malformed\r\n" (uri/path uri))))
    (! (:message/reply m) response :no-metadata: #f (:message/echo m))
    )
  (audit (this-peer) "mailbox redirector" "starting")
  (let loop ((m (? (this-mailbox))))
     (match
        (message/path+body m)
        (#(/http/get #(,origin ,uri ,request ,response))
          (let* ((uuid (mailbox/uuid (uri/path uri)))
            (inbox (and uuid (peer/mailbox (this-peer) uuid))))
            (cond
              (inbox (! (@ inbox '/http/get) (:message/body m) (:message/metadata m) (:message/reply m) (:message/echo m)))
              (else (unknown-mailbox m response uri uuid)))))
        (#(/http/post #(,origin ,uri ,request))
          ; We know that the path prefix is /mailbox/...
          (let* ((uuid (mailbox/uuid (uri/path uri)))
            (inbox (and uuid (peer/mailbox (this-peer) uuid)))) ; Spawn mailbox.
             (cond
              (inbox (! (@ inbox '/http/post) (:message/body m) (:message/metadata m) (:message/reply m) (:message/echo m)))
              (else (unknown-mailbox m response uri uuid))
              )))
        (,_ignore #f))
      (loop (? (this-mailbox)))))

;; Generates a responder for intercepting HTTP requests directed to fixed, well-known path that is an alias
;; for a spawn.
;; mailbox the mailbox assigned to the spawn
(define (responder/alias/new mailbox)
  (lambda (sham origin uri request response)
    (display (format "responder/alias/new: origin:~a\n" origin))
    (display (format "responder/alias/new: uri:~a\n" uri))
    (display (format "responder/alias/new: uri:~a\n" (uri/path uri)))
    (case (string->symbol (http/request/method request))
      ((|GET|)
       (http/get/relay mailbox origin uri request response)) ; Relay the GET to the spawn and wait (briefly) for spawn response.

      ((|POST| |PUT|)
       (http/post/relay mailbox origin uri request)) ; Relay the contents of the POST/PUT to the spawn.

      (else
       (http/response/status! response 501)
       (http/response/reason! response "Method Not Implemented")
       (http/response/entity! response "504 Method Not Implemented")))))

;; DEPRECATED.
;; Generates a responder for intercepting HTTP GETs directed to paths /mailbox/UUID or /mailbox/UUID/* .
;; peer: the Serf peer responsible for managing the spawns.
; (define (responder/http/get/new peer)
;   (lambda (sham origin uri request response)
;     (display (format "responder/http/get: origin:~a\n" origin))
;     (display (format "responder/http/get: uri:~a\n" uri))
;     (display (format "responder/http/get: uri:~a\n" (uri/path uri)))
;     (display (format "responder/http/get: uuid:~a\n" (mailbox/uuid (uri/path uri))))
;     (let* ((uuid  (mailbox/uuid (uri/path uri)))        ; We know that the path prefix is /mailbox/...
; 	   (inbox (and uuid (peer/mailbox peer uuid)))) ; Spawn mailbox.
;       (cond
;        (inbox
; 	(display "responder/http/get: inbox found\n")
; 	(http/get/relay inbox origin uri request response)) ; Relay the GET request to the spawn and wait (briefly) for spawn response.
;        (else
;         ; URI is malformed, UUID is unknown, or /mailbox/UUID* path has been retracted
;         ; because the spawn gracefully terminated, generated an exception, or was killed.
; 	(http/response/status! response 404)
; 	(http/response/reason! response "Not Found")
; 	(http/response/entity!
; 	 response
; 	 (if uuid
; 	     (format "404 Not Found\r\nReasons:\r\n(1) UUID ~a unknown\r\n(2) Path /mailbox/~a... was withdrawn\r\n"
; 		     uuid uuid)
; 	     (format "404 Not Found\r\nReasons:\r\n URI ~a is malformed\r\n" (uri/path uri)))))))))

;; DEPRECATED.
;; Responder for intercepting HTTP GETs directed to paths /mailbox/UUID or /mailbox/UUID/* .
; (define (responder/http/post/new peer)
;   (lambda (sham origin uri request response)
;     (let* ((uuid  (mailbox/uuid (uri/path uri)))        ; We know that the path prefix is /mailbox/...
; 	   (inbox (and uuid (peer/mailbox peer uuid)))) ; Mailbox for spawn.
;       (cond
;        (inbox (http/post/relay inbox origin uri request)) ; Relay the payload of the POST request to the spawn.
;        (else
;         ; URI is malformed, UUID is unknown, or /mailbox/UUID* path has been retracted
;         ; because the spawn gracefully terminated, generated an exception, or was killed.
; 	(http/response/status! response 404)
; 	(http/response/reason! response "Not Found")
; 	(http/response/entity!
; 	 response
; 	 (if uuid
; 	     (format "404 Not Found\r\nReasons:\r\n(1) UUID ~a unknown\r\n(2) Path /mailbox/~a... was withdrawn\r\n"
; 		     uuid uuid)
; 	     (format "404 Not Found\r\nReasons:\r\n URI ~a is malformed\r\n" (uri/path uri)))))))))

;; Relay a GET request to a spawned thread.
;; spawn: mailbox of spawn
;; origin: (dns . port) of request origin
;; request: <http/request>
;; response: <http/response>
(define (http/get/relay spawn origin uri request response)
  (display (format "http/get/relay: origin:~a uri:~a\n" origin uri))
  ;(display spawn) (newline) (newline)
  (let ((reply (make <mailbox>))
	(body  (vector origin uri request response)))
    (display "about to send to spawn\n")
    (! (@ spawn '/http/get) body :no-metadata: (@ reply '/http/response))
    (display "sent to spawn\n")
    (cond
     ((? reply :max-sham-patience: #f) => ; Wait no more than :max-sham-patience: milliseconds for a reply.
      (lambda (answer)
        ; Body of answer is #(status reason headers) or #(status reason headers payload).
	;(assemble-http-response answer response)))

	; Body of answer is response sent in ! above.
	(if
	 (not (and (eq? (:message/path answer) '/http/response) (eq? (:message/body answer) response)))
	 (begin
	  (http/response/status! response 500)
	  (http/response/reason! response "Response Unacceptable")
	  (http/response/entity! response "Response violated one or more Sham constraints.")))))
     (else
      (http/response/status! response 503)
      (http/response/reason! response "Service Unavailable")
      (http/response/entity! response "503 Service Unavailable. No timely response.")))))

;; Relay a POST or PUT request to a spawned thread.
;; spawn: mailbox to which POST or PUT is directed.
;; origin:  (dns . port) of origin (unused at present)
;; uri:   request URI /mailbox/UUID/...
;; request: <http/request>
;; response: <http/response>
;; Note: The HTTP response has already been initialized to 200 OK by Sham so our only task is to pass
;; the payload of the request entity (translated if necessary) to the destination thread. In this case
;; the 200 OK means ONLY that the message arrived at the destination mailbox and NOTHING more.
;; There is NO guarantee that the destination thread will take any action in consequence.
(define (http/post/relay spawn origin uri request)
  (let ((n (http/request/content/length request))
	(header (http/request/content/type request))
	(entity (http/request/entity request)))

    (display (format "http/post/relay origin:~a uri:~a\n" origin (uri/path uri)))
    (display (format "http/post/relay length:~a type:~a\n" n (and header (cdr header))))

    (if (and n (> n 0) (pair? header) entity) ; There is a non-empty entity and a Content-Type header.
	(let* ((flavor   (http/header/content/type/parse (cdr header))) ; flavor is (media . (p_1 p_2 ...))
	       (media    (and (list? flavor) (car flavor)))
	       (encoding (and (list? flavor) (assoc "encoding" (cdr flavor))))) ; Unused for now.
	  (display (format "http/post/relay  flavor:~a\n" flavor))
	  (display (format "http/post/relay  media:~a\n" media))
	  (if (string=? media :mime/serf/binary:)
	      (let ((buffer (http/entity/buffer entity))) ; Convert payload of request to Scheme buffer.
		(mailbox/send! spawn (deserialize/from buffer))) ; buffer contains a serf/message structure.

	      (let* ((path     (mailbox/path (uri/path uri)))
		     (metadata (list `(path . ,path) `(flavor . ,flavor)))
		     (to       (@ spawn '/http/post)))
		(display (format "http/post/relay path:~a\n" path))
		(display (format "http/post/relay metadata:~a\n" metadata))
		(cond
		 ((string=? media :mime/json:)
		  (! to (vector origin uri (http/entity/string entity)) metadata))

		 ((string=? media :mime/text:)
		  (! to (vector origin uri (http/entity/string entity)) metadata))

		 ((string=? media :mime/form/urlencoded:)
		  (display (format "http/post/relay metadata just before (! ...):~a\n" metadata))
		  (! to (vector origin uri (http/entity/string entity)) metadata))

		 (else ; Something we don't recognize. Just pass it on and let the spawn figure it out.
		  ;(! to (vector origin uri (http/entity/string entity)) metadata)))))))
		  (! to (vector origin uri entity) metadata)))))))))

(define (peer/test/01 host)
  (let ((peer (object/forge <serf/peer> host 8080 #f))
	(counter 1))

    (define (responder sham origin uri request response)
      (http/response/entity! response (format "Hello! Nice to hear from you again [~d]." counter))
      (set! counter (+ counter 1)))

    (sham/register (peer/sham peer) "/*" responder) ; Register the responder with the Sham instance.

    peer))

(define (peer/test/02 host port)
  (let ((peer (object/forge <serf/peer> host port  :serf-sandbox-path:))  ; "/Users/mgorlick/Projects/sisc-1.16.6/serf/"
	(counter 1))

    (define (responder/hello sham origin uri request response)
      (http/response/entity! response (format "Hello! Nice to hear from you again [~d]." counter))
      (set! counter (+ counter 1)))

    (define (responder/cloud sham origin uri request response)
      (http/response/entity! response (cloud/json)))

    (define (thunk/echo)
      (let loop ((m (? (this-mailbox)))
		 (counter 1))
	(display (format "thunk/echo: path:~a body:~a\n" (:message/path m) (:message/body m)))
	(match
	 (message/path+body m)

	 (#(/http/get #(,origin ,uri ,request ,response))
	  (guard (string=? "GET" (http/request/method request)))

	  (display (format "thunk/echo: origin:~a uri:~a\n" origin uri))
	  (display (format "thunk/echo:~a\n" (request/echo request)))
	  
	  ;(! (:message/reply m) (vector 200 "OK" '() (format "visits:~d\n~a" counter (request/echo request))))
	  ;(http/response/status! 200)  ; Unnecessary since response is initialized 200.
	  ;(http/response/reason! "OK") ; Ditto for reason phrase.
	  (http/response/header! response "Content-Type" :http/content/text:)
	  (http/response/entity! response (format "visits:~d\n~a" counter (request/echo request)))
	  (! (:message/reply m) response :no-metadata: #f (:message/echo m))
	  (display "thunk/echo: replied\n")
	  ) ; Send back #(status reason headers entity)

	 (#(/http/post ,body) ; No reply field is sent since POST response is handled at a higher level.
	  (display (format "thunk/echo: metadata:~a\n" (:message/metadata m)))
	  (display (format "thunk/echo: content:~a\n" body)))

	 (,_ignore 
	  (display (format "thunk/echo: ignore:~a\n" ignore))
	  #f))
	(loop (? (this-mailbox)) (+ counter 1))))

    ;(sham/register (peer/sham peer) "/widget/cloud/*"   responder/cloud)
    ;(sham/register (peer/sham peer) "/widget/manager/*" responder/manager)
    ;(sham/register (peer/sham peer) "/widgets/manager/*" responder/manager)
    ;(sham/register (peer/sham peer) "/echo/*"      responder/echo)
    ;(sham/register (peer/sham peer) "/*"           responder/hello)
    ;(sham/register (peer/sham peer) "/echo/spawn"
		;   (responder/alias/new (peer/mailbox peer (peer/spawn peer thunk/echo)))) ; Alias for spawn.
    ;(sham/register (peer/sham peer) "/widget/manager/*"
		;   (responder/alias/new (peer/mailbox peer (peer/spawn/unsafe peer thunk/manager)))) ; Alias for spawn.
    (sham/register (peer/sham peer) "/echo/spawn"
		   (peer/mailbox peer (peer/spawn peer thunk/echo)))
    (sham/register (peer/sham peer) "/widget/manager/*" (peer/mailbox peer (peer/spawn/unsafe peer thunk/manager))) ; Alias for spawn.
    (sham/register (peer/sham peer) "/mailbox/*"   (peer/mailbox peer (peer/spawn/unsafe peer responder/mailbox/new))) ; Forwards HTTP requests to spawns.

    (display (format "thunk/echo URL is /mailbox/~a\n" (peer/spawn peer thunk/echo))) ; Create a spawn  of thunk/echo.
    peer))

(define (peer/test/03 host port)
  (let* ((thunk (lambda () (peer/test/02 host port)))
	 (fiber (fiber/new thunk))
	 (background (fiber/start fiber)))
    background))

(define (request/echo request)
  (let* ((method  (http/request/method request))
	 (uri     (http/request/uri request))
	 (headers (map
		   (lambda (h) (format "~a: ~a" (car h) (cdr h)))
		   (http/request/headers request)))
	 (entity  (cond
		   ((http/request/entity request) => (lambda (e) (http/entity/string e)))
		   (else #f)))
	 (prefix  (cons method (cons uri headers)))
	 (all     (if entity (append prefix (list entity)) prefix))) ; Tack the entity on the end if it exists.
    (string-join all "\n" 'suffix)))

(define (responder/echo sham origin uri request response)
  (let* ((headers (http/request/headers request))
	 (parts   (map (lambda (h) (format "~a: ~a" (car h) (cdr h))) headers))
	 (echo (string-join parts "\n"))
	 (entity (http/request/entity request)))

	(http/response/entity!
	 response 
	 (if entity
	     (string-append echo "\n\n>>>>> Request Entity <<<<<\n\n" (->string entity))
	     echo))))

(define (responder/manager sham origin uri request response)
  (set! :request: request)
  (display "***responder/manager***\n")
  (display (request/echo request)))

  
(define :request: #f)
(define :request-entity: #f)
