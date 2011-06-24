;; Copyright 2009 Michael M. Gorlick

;; Definitions

(define <java-session-request>)
(define get-local-address)  ; InetSocketAddress getLocalAddress()
(define get-remote-address) ; InetSocketAddress getRemoteAddress()
(define is-completed)       ; boolean isCompleted()

(define <java-http-session-responder>) ; Java interface.
(define cancelled) ; void cancelled(SessionRequest)
(define completed) ; void completed(SessionRequest)
(define failed)    ; void failed(SessionRequest)
(define timeout)   ; void timeout(SesssionRequest)

;; Return the remote endpoint of the session as an authority "DNS/IP:port"
(define (network/imposter/session/authority session)
  (network/address/authority (get-remote-address session)))

#|(define (audit-http-session-responder logger method session)
  (serf-audit
   logger
   (string-append "proxy-http-session-responder." method ": " (session-to-authority session))))

(define-java-proxy
  (proxy-http-session-responder imposter) ; Proxy constructor.
  (<java-http-session-responder>)       ; Interfaces implemented by this proxy.

  (define (cancelled this session) ; Session establishment was cancelled.
    ; Delete placeholder session in connections table.
    (hashtable/remove! (:connections imposter) (session-to-authority))
    (audit-http-session-responder (:logger imposter) "cancelled" session))

  (define (completed this session) ; Session establishment succeeded.
    ; Do not touch (:connections imposter) since session was replaced by a new connection!
    (audit-http-session-responder (:logger imposter) "completed" session))

  (define (failed this session) ; Session establishment failed.
    ; Delete placeholder session in connections table.
    (hashtable/remove! (:connections imposter) (session-to-authority))
    (audit-http-session-responder (:logger imposter) "failed" session))

  (define (timeout this session) ; Session establishment timed out.
    ; Delete placeholder session in connections table.
    (hashtable/remove! (:connections imposter) (session-to-authority))
    (audit-http-session-responder (:logger imposter) "timeout" session))

  (define (to-string this) (->jstring "http-session-responder")))
|#

;; Expressions

(set-java-class <java-session-request> |org.apache.http.impl.nio.reactor.SessionRequestImpl|)
(set-generic-java-method get-local-address)  ; InetSocketAddress getLocalAddress()
(set-generic-java-method get-remote-address) ; InetSocketAddress getRemoteAddress()
(set-generic-java-method is-completed)       ; boolean isCompleted()

(set-java-class <java-http-session-responder> |org.apache.http.nio.reactor.SessionRequestCallback|)
(set-generic-java-method cancelled) ; void cancelled(SessionRequest)
(set-generic-java-method completed) ; void completed(SessionRequest)
(set-generic-java-method failed)    ; void failed(SessionRequest)
(set-generic-java-method timeout)   ; void timeout(SesssionRequest)
