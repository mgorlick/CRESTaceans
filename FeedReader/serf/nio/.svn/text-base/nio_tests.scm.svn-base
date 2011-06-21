(define CREST-IMPOSTER #f)
(define CREST-IMPOSTER-REQUEST-HANDLER #f)
(define CREST-IMPOSTER-CONNECTION-LISTENER #f)
(define CREST-IMPOSTER-SESSION-HANDLER #f)

;; Open a connection to www.google.com:80. using the raw IPv4 address.
(define (imposter-test98)
  (let* ((logger (this-logger))
	 (user-agent "CREST/Imposter 1.0")
	 (request-responder (proxy-http-request-responder logger (make <queue>)))
	 (connection-listener (proxy-connection-listener logger))
	 (http-client (make-http-client user-agent request-responder connection-listener))
	 (session-responder (proxy-http-session-responder logger)))
    (set! CREST-IMPOSTER http-client)
    (open-http-session http-client "66.102.7.99" 80 session-responder)))

;; Open a connection to www.google.com:80.
(define (imposter-test99)
  (let* ((logger (this-logger))
	 (user-agent "CREST/Imposter 1.0")
	 (request-responder (proxy-http-request-responder logger (make <queue>)))
	 (connection-listener (proxy-connection-listener logger))
	 (http-client (make-http-client user-agent request-responder connection-listener))
	 (session-responder (proxy-http-session-responder logger)))
    ;(set! CREST-IMPOSTER-REQUEST-HANDLER request-handler)
    ;(set! CREST-IMPOSTER-CONNECTION-LISTENER connection-listener)
    ;(set! CREST-IMPOSTER-SESSION-HANDLER session-handler)
    (set! CREST-IMPOSTER http-client)
    (open-http-session http-client "www.google.com" 80 session-responder)))

;; One request to a single authority.
(define (imposter-test100)
  (let* ((imposter (make <http-client> (this-logger)))
	 (uri (make-uri "http" "www.google.com" 80 "/"))
	 (reply (make <mailbox>))
	 (fiber (make <fiber> (lambda () (dispatch imposter)))))
    (set! CREST-IMPOSTER imposter)
    (start fiber)
    (! (:inbox imposter) (list 'get uri 'foobar reply))
    reply))

;; Multiple requests to a single authority.
(define (imposter-test101)
  (let* ((imposter (make <http-client> (this-logger)))
	 (uri (make-uri "http" "www.google.com" 80 "/"))
	 (reply (make <mailbox>))
	 (fiber (make <fiber> (lambda () (dispatch imposter)))))
    (set! CREST-IMPOSTER imposter)
    (start fiber)
    (! (:inbox imposter) (list 'get uri 'response1 reply))
    (! (:inbox imposter) (list 'get uri 'response2 reply))
    (! (:inbox imposter) (list 'get uri 'response3 reply))
    reply))

;; Multiple requests but each to a different authority.
(define (imposter-test102)
  (let* ((imposter (make <http-client> (this-logger)))
	 (google-uri  (make-uri "http" "www.google.com" 80 "/"))
	 (uci-uri     (make-uri "http" "www.isr.uci.edu" 80 "/"))
	 (latimes-uri (make-uri "http" "www.latimes.com" 80 "/"))
	 (reply (make <mailbox>))
	 (fiber (make <fiber> (lambda () (dispatch imposter)))))
    (set! CREST-IMPOSTER imposter)
    (start fiber)
    (! (:inbox imposter) (list 'get google-uri  'google reply))
    (! (:inbox imposter) (list 'get uci-uri     'uci reply))
    (! (:inbox imposter) (list 'get latimes-uri 'latimes reply))
    reply))

;; Nine total requests rotating thru three distinct authorities.
(define (imposter-test103)
  (let* ((imposter (make <http-client> (this-logger)))

	 (google-uri  (make-uri "http" "www.google.com"  80 "/"))
	 (uci-uri     (make-uri "http" "www.isr.uci.edu" 80 "/"))
	 (latimes-uri (make-uri "http" "www.latimes.com" 80 "/"))

	 (reply (make <mailbox>))
	 (imposter-fiber (make <fiber> (lambda () (dispatch imposter))))

	 (m1 (list 'get google-uri  'google reply))
	 (m2 (list 'get uci-uri     'uci reply))
	 (m3 (list 'get latimes-uri 'latimes reply)))

    (start imposter-fiber)

    (for-each
     (lambda (a b c)
       (! (:inbox imposter) a)
       (! (:inbox imposter) b)
       (! (:inbox imposter) c))

     (list m1 m2 m3)
     (list m2 m3 m1)
     (list m3 m1 m2))

    reply))
