;; Copyright 2009 Michael M. Gorlick

;; SISC proxy wrapper around org.apache.http.protocol.HttpRequestHandler

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(define-java-class <java-http-server-request-responder> |org.apache.http.protocol.HttpRequestHandler|)
(define-java-class <java-connection-listener>           |org.apache.http.nio.protocol.EventListener|)

(define sham/respond                        (make-generic-method))
(define connection/open                     (make-generic-method))
(define connection/timeout                  (make-generic-method))
(define connection/closed                   (make-generic-method))
(define connection/fatal-io-exception       (make-generic-method))
(define connection/fatal-protocol-exception (make-generic-method))

(define :http.connection: (->jstring "http.connection"))
(define-generic-java-method get-attribute)
(define-generic-java-method get-host-name)
(define-generic-java-method get-host-address)
(define-generic-java-method get-remote-address)

;; Extract a DNS host name from a java.net.InetAddress.
(define (address/dns a)
  (let ((dns (get-host-name a)))
    (if (java-null? dns) #f (->string dns))))

;; Extract an IP dotted address from a java.net.InetAddress.
(define (address/ip a)
  (let ((ip (get-host-address a)))
    (if (java-null? ip) #f (->string ip))))

(define-java-proxy
  (proxy-http-request-responder sham)
  (<java-http-server-request-responder>)

  ; request:  HttpRequest
  ; response: HttpResponse
  ; context:  HttpContext containing the request, response, and incoming connection.
  (define (handle this request response context)
    (let* ((incoming (get-attribute context :http.connection:))
	   (remote   (get-remote-address incoming))
	   (dns      (address/dns remote))
	   (ip       (address/ip  remote)))
      ; Call on Sham to respond to the HTTP request.
     (with/fc (lambda (m e) (print-exception (make-exception m e))) (lambda ()
      (sham/respond
       sham
       (cons dns ip) ; Network source of request.
       (object/forge <http/request>  request)     ; Convert the HttpRequest to a Serf object.
       (object/forge <http/response> response))))   ; Convert the HttpResponse to a Serf object.
     ))

  (define (to-string this) (->jstring "http-server-request-responder")))

;; For each method:
;; connection: <java-default-nio-http-client-connection> instance
;; exception: either a <java-http-exception> or a <java-io-exception> instance
(define-java-proxy
  (proxy-connection-listener sham)
  (<java-connection-listener>)

  (define (connection-open this connection)
    (connection/open sham connection))

  (define (connection-timeout this connection)
    (connection/timeout sham connection))

  (define (connection-closed this connection)
    (connection/closed sham connection))

  (define (|fatal-IO-exception| this exception connection)
    (connection/fatal-io-exception sham connection exception))

  (define (fatal-protocol-exception this exception connection)
    (connection/fatal-protocol-exception connection exception))

  (define (to-string this) (->jstring "connection-listener")))
