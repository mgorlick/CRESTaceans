(import s2j)

(define-java-class <java-session-request> |org.apache.http.impl.nio.reactor.SessionRequestImpl|)
(define-generic-java-methods
  get-local-address  ; InetSocketAddress ()
  get-remote-address ; InetSocketAddress ()
  is-completed)      ; boolean ()

(define-java-class <java-internet-address> |java.net.InetSocketAddress|)
(define-generic-java-methods
  get-host-name ; String ()
  get-port)     ; int ()

(define (audit-http-session-responder logger method session)
  (serf-audit
   logger
   (string-append "proxy-http-session-responder." method ": " (session-to-authority session))))

;; Return the remote endpoint of the session as an authority "DNS/IP:port"
;; Returns the remote address of the session request (<java-session-request> instance)
;; as a Scheme string "host:port".
(define (session-to-authority session)
  (internet-socket-address-to-authority (get-remote-address session)))

(define-java-class <java-http-session-responder> |org.apache.http.nio.reactor.SessionRequestCallback|)
(define-generic-java-methods
  cancelled
  completed
  failed
  timeout)

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
