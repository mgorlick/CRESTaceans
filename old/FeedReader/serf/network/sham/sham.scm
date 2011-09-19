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

(define sham/audit      (make-generic-method))
(define sham/register   (make-generic-method))
(define sham/unregister (make-generic-method))
(define sham/port       (make-generic-method))

(define-generic-java-method get-remote-address)

(define (audit/connection self event connection)
  (let ((address (java-to-string (get-remote-address connection)))
	(id      (java-object-to-id connection)))
    (sham/audit self (format "connection/~a" event) (format "~a connection:~x" address id))))

(define (<serf/sham>)
  (let* ((self                (make-object))
	 (logger              #f)
	 (port                #f)
	 (request-responder   (proxy-http-request-responder self))
	 (connection-listener (proxy-connection-listener self))
	 (registry            (object/forge <sham/matcher>))
	 (java-bottom         #f))

    (make-method!
     self instantiate
     (lambda (self p l root)  ; root: directory root for serving static files. Should be #f if no file serving.
       (set! port p)
       (set! logger l)
       (set! java-bottom (bottom/new port request-responder connection-listener root)))) ; No file serving.

    ; Logging for sham.
    (make-method!
     self sham/audit
     (lambda (self method message)
       (log/audit logger (format "serf/sham.~a: ~a" method message))))

    ; Register a responder for a portion of the URL space.
    ; pattern: Scheme string of the form "*<uri>", "<uri>*" or "*".
    ; responder: (lambda (sham from uri request response) ...) where
    ;   from: (dns . ip)
    ;   uri:  the request URI as a java.net.URI (should rewrite uri.scm to be an object wrapper around java.net.URI)
    ;   request: Scheme HTTP request object
    ;   response: Scheme HTTP response object
    (make-method!
     self sham/register
     (lambda (self pattern responder)
       (matcher/register registry pattern responder)
       #t))

    ; Unregister a responder for a portion of the URL space.
    ; pattern: Scheme string of the form "*<uri>", "<uri>*" or "*".
    (make-method!
     self sham/unregister
     (lambda (self pattern)
       (matcher/unregister registry pattern)
       #t))

    (make-method!
     self sham/respond ; Generic method is defined in module serf/sham/proxy.
     ; from:     (dns . ip) both as strings
     ; request:  <http/request>
     ; response: <http/response>
     (lambda (self from request response)
       (let* ((u (http/request/uri* request)) ; The request URI as a Java String.
	      (responder (matcher/match registry u))
	      (uri (uri/new u))) ; Wrapper around java.net.URI
	 (sham/audit
	  self "sham/respond"
	  (format "from:~a/~a method:~a path:~a\n"
		  (car from) (cdr from)
		  (http/request/method request)
		  (uri/path uri)))

	 (cond
	  (responder
	   (responder self from uri request response))
	  (else
	   (http/response/status! response 404)
	   (http/response/reason! response "Not Found")
	   (http/response/entity! response (http/entity/new "404 Not Found. What did you expect?")))))))

     (make-method!
      self connection/open ; HTTP connection listener. Connection is now open.
      (lambda (self connection)
	(audit/connection self "open" connection)))

     (make-method!
      self connection/timeout ; HTTP connection listener. Connection timed out.
      (lambda (self connection)
	(audit/connection self "timeout" connection)))

     (make-method!
      self connection/closed ; HTTP connection listener. Connection was closed.
      (lambda (self connection)
	(audit/connection self "closed" connection)))

     (make-method!
      self connection/fatal-io-exception ; HTTP connection listener. Connection received fatal io error.
      (lambda (self connection)
	(audit/connection self "fatal-io-exception" connection)))

     (make-method!
      self connection/fatal-protocol-exception ; HTTP connection listener. Connection received fatal protocol error.
      (lambda (self connection)
	(audit/connection self "fatal-protocol-exception" connection)))

     (make-method!
      self sham/port
      (lambda (self) port))

     self))

(define (sham/test/01)
  (let* ((log     (log/new "serf:8080" "serf:8080"))    ; Create a log for this Sham instance.
	 (sham    (object/forge <serf/sham> 8080 log #f)) ; Create an instance of Sham.
	 (counter 1))
					
    (define (responder sham from uri request response)
      (http/response/entity! response (format "Hello! Nice to hear from you again [~d]." counter))
      (set! counter (+ counter 1)))

    (sham/register sham "/*" responder) ; Register the responder with the Sham instance.
    sham))

(define (sham/test/02)
  (let* ((log     (log/new "serf:8080" "serf:8080"))    ; Create a log for this Sham instance.
	 (sham    (object/forge <serf/sham> 8080 log
				"/Users/mgorlick/Projects/sisc-1.16.6/serf/")) ; Create an instance of Sham.
	 (counter 1))
					
    (define (responder sham from uri request response)
      (http/response/entity! response (format "Hello! Nice to hear from you again [~d]." counter))
      (set! counter (+ counter 1)))

    (sham/register sham "/*" responder) ; Register the responder with the Sham instance.
    sham))

