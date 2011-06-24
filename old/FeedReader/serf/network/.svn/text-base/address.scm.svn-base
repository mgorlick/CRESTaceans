;; Copyright 2009 Michael M. Gorlick

;; Definitions
;(define-java-class <java-internet-address> |java.net.InetSocketAddress|)
;(define-generic-java-method to-string)

;; Given an host/port generate the string DNS/IP:port where host is the host DNS name,
;; IP is the Intenet Protocol dotted numeric address, and port is the IP port number.
;; host: DNS name or IP address as Java String
;; port: IP port number as Java int
(define (address/canonical host port)
  (format "~a:~a" (if (string? host) host (->string host))
                  (if (integer? port) port (->number port))))
  ; Convert the arguments to Java equivalents if necessary.
  ;(let ((host (if (string? host)  (->jstring host) host))
	;(port (if (integer? port) (->jint port)    port)))
  ;(let ((address (java-new <java-internet-address> host port)))
  ;  (->string (to-string address)))))

