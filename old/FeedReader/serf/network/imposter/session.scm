;; Copyright 2009 Michael M. Gorlick

;; Definitions

(define-java-class <java-session-request>        |org.apache.http.impl.nio.reactor.SessionRequestImpl|)
(define-java-class <java-http-session-responder> |org.apache.http.nio.reactor.SessionRequestCallback|)

;; session: <java-session-request> instance
(define-java-proxy
  (proxy-http-session-responder imposter) ; Proxy constructor.
  (<java-http-session-responder>)       ; Interfaces implemented by this proxy.

  (define (cancelled this session) ; Session establishent was cancelled.
    (session-cancelled imposter session))

  (define (completed this session) ; Session establishment succeeded.
    (session-completed imposter session))

  (define (failed this session) ; Session establishment failed.
    (session-failed imposter session))

  (define (timeout this session) ; Session establishment timed out.
    (session-timeout imposter session))

  (define (to-string this) (->jstring "http-session-responder")))
