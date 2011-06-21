;; Copyright 2009 Michael M. Gorlick

;(define-java-class <java-session-request>        |org.apache.http.impl.nio.reactor.SessionRequestImpl|)
;(define-java-class <java-http-context>           |org.apache.http.protocol.SyncBasicHttpContext|)
;(define-java-class <java-http-host>              |org.apache.http.HttpHost|)
(define-java-class <java-http-client-request-responder> |org.apache.http.nio.protocol.HttpRequestExecutionHandler|)
(define-java-class <java-connection-listener>           |org.apache.http.nio.protocol.EventListener|)
(define-java-class <java-http-session-responder>        |org.apache.http.nio.reactor.SessionRequestCallback|)
;(define-java-class <java-session-request>        |org.apache.http.impl.nio.reactor.SessionRequestImpl|)
;(define-java-class <java-http-exception>         |org.apache.http.HttpException|)
;(define-java-class <java-io-exception>           |java.io.IOException|)

(define-generic-java-method get-host-name)
(define-generic-java-method get-port)

;; Methods for <java-http-client-request-responder>.
(define initialize-context (make-generic-method))
(define finalize-context   (make-generic-method))
(define submit-request     (make-generic-method))
(define handle-response    (make-generic-method))
;; Methods for <java-connection-listener>.
(define connection/open                     (make-generic-method))
(define connection/timeout                  (make-generic-method))
(define connection/closed                   (make-generic-method))
(define connection/fatal-io-exception       (make-generic-method))
(define connection/fatal-protocol-exception (make-generic-method))
;; Methods for <java-http-session-responder>.
(define session/cancelled (make-generic-method))
(define session/completed (make-generic-method))
(define session/failed    (make-generic-method))
(define session/timeout   (make-generic-method))

(define-java-proxy
  (proxy-http-request-responder imposter)
  (<java-http-client-request-responder>)

  ; context: <java-http-context>
  ; attachment: <java-http-host>
  (define (initalize-context this context attachment)
    (initialize-context imposter context (get-host-name attachment) (get-port attachment)))

  (define (finalize-context this context)
    (finalize-context imposter context))

  ; Called whenever a connection is ready to accept another request.
  ; The request transmitted is the value returned by this call.
  ; A null request return value indicates that no request is ready for transmission.
  (define (submit-request this context)
    (submit-request imposter context))

  ; Called whenever a response arrives.
  ; response: <java-http-response>
  ; context: <java-http-context>
  (define (handle-response this response context)
    (handle-response imposter response context))

  ; For the sake of logging and debugging.
  (define (to-string this) (->jstring "http-request-responder")))

;; For each method:
;; connection: <java-default-nio-http-client-connection> instance
;; exception: either a <java-http-exception> or a <java-io-exception> instance
(define-java-proxy
  (proxy-connection-listener imposter)
  (<java-connection-listener>)

  (define (connection-open this connection)
    (connection/open imposter connection))

  (define (connection-timeout this connection)
    (connection/timeout imposter connection))

  (define (connection-closed this connection)
    (connection/closed imposter connection))

  (define (|fatal-IO-exception| this exception connection)
    (connection/fatal-io-exception imposter connection exception))

  (define (fatal-protocol-exception this exception connection)
    (connection/fatal-protocol-exception connection exception))

  (define (to-string this) (->jstring "connection-listener")))

;; session: <java-session-request> instance
(define-java-proxy
  (proxy-http-session-responder imposter)
  (<java-http-session-responder>)

  (define (cancelled this session) ; Session establishment was cancelled.
    (session/cancelled imposter session))

  (define (completed this session) ; Session establishment succeeded.
    (session/completed imposter session))

  (define (failed this session) ; Session establishment failed.
    (session/failed imposter session))

  (define (timeout this session) ; Session establishment timed out.
    (session/timeout imposter session))

  (define (to-string this) (->jstring "http-session-responder")))

