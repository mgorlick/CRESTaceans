(import oo)
(import serf/fiber)
(import serf/http/response)
(import serf/imposter)
(import serf/object)
(import serf/mailbox)
(import serf/message)
(import serf/uri)


;; This test runs the Imposter dispatch in the REPL thread and gives better error messages
;; if something dies inside Imposter. Not safe though since it can deadlock.
(define (imposter/test/01a)
  (let* ((imposter/inbox (make <mailbox>))
	 (imposter (object/forge <serf/imposter> (this-log) imposter/inbox))
	 (uri (uri/new "http" "www.google.com" 80 "/"))
	 (reply (make <mailbox>)))
    (! (@ imposter/inbox '/http/get) (list uri #f #f) :no-metadata: (@ reply '/http/response) 'google)
    (imposter/dispatch imposter)
    (let ((m (? reply 5000 #f)))
      (if m
	  (let ((response (:message/body m)))
	    (display (format
		      "~a ~d ~a\n"
		      (:message/metadata m) ; tag
		      (http/response/status response)
		      (http/response/reason response))))
	  (display "No response after waiting 5 seconds\n")))))

;; A single request to a single authority.
(define (imposter/test/01)
  (let* ((imposter/inbox (make <mailbox>))
	 (imposter (object/forge <serf/imposter> (this-log) imposter/inbox))
	 (uri (uri/new "http" "www.google.com" 80 "/"))
	 (reply (make <mailbox>))
	 (imposter/loop (fiber/new (lambda () (imposter/dispatch imposter)))))
    (fiber/start imposter/loop)
    (! (@ imposter/inbox '/http/get) (list uri #f #f) :no-metadata: (@ reply '/http/response) 'google)
    (let ((m (? reply 5000 #f)))
      (if m
	  (let ((response (:message/body m)))
	    (display (format
		      "~a ~d ~a\n"
		      (:message/echo m)
		      (http/response/status response)
		      (http/response/reason response))))
	  (display "No response after waiting 5 seconds\n")))))

;; Multiple requests to a single authority.
(define (imposter/test/02)
  (let* ((imposter/inbox (make <mailbox>))
	 (imposter (object/forge <serf/imposter> (this-log) imposter/inbox))
	 (uri (uri/new "http" "www.google.com" 80 "/"))
	 (reply (make <mailbox>))
	 (body (list uri #f #f)) ; No headers and no payload.
	 (imposter/loop (fiber/new (lambda () (imposter/dispatch imposter)))))
    (fiber/start imposter/loop)
    (! (@ imposter/inbox '/http/get) body :no-metadata: (@ reply '/http/response) 'response1)
    (! (@ imposter/inbox '/http/get) body :no-metadata: (@ reply '/http/response) 'response2)
    (! (@ imposter/inbox '/http/get) body :no-metadata: (@ reply '/http/response) 'response3)
    (let loop ((m (? reply 5000 #f)))
      (if m
	  (let ((response (:message/body m)))
	    (display (format
		      "~a ~d ~a\n"
		      (:message/echo m)
		      (http/response/status response)
		      (http/response/reason response)))
	    (loop (? reply 5000 #f)))
	  (display "No more responses after waiting 5 seconds\n")))))

;; Nine total requests rotating thru three distinct authorities.
(define (imposter/test/03)
  (let* ((imposter/inbox (make <mailbox>))
	 (reply          (make <mailbox>))
	 (imposter (object/forge <serf/imposter> (this-log) imposter/inbox))

	 (google  (uri/new "http" "www.google.com"  80 "/"))
	 (uci     (uri/new "http" "www.isr.uci.edu" 80 "/"))
	 (latimes (uri/new "http" "www.latimes.com" 80 "/"))

	 (imposter/loop (fiber/new (lambda () (imposter/dispatch imposter))))

	 (to   (@ imposter/inbox '/http/get))
	 (from (@ reply          '/http/response))

	 (m1 (list to (list google  #f #f) :no-metadata: from 'google))
	 (m2 (list to (list uci     #f #f) :no-metadata: from 'uci))
	 (m3 (list to (list latimes #f #f) :no-metadata: from 'latimes)))

    (fiber/start imposter/loop)

    (for-each
     (lambda (a b c)
       (apply ! a)
       (apply ! b)
       (apply ! c))

     (list m1 m2 m3)
     (list m2 m3 m1)
     (list m3 m1 m2))

    (let loop ((m (? reply 5000 #f)))
      (if m
	  (let ((response (:message/body m)))
	    (display (format
		      "~a ~d ~a\n"
		      (:message/echo m)
		      (http/response/status response)
		      (http/response/reason response)))
	    (loop (? reply 5000 #f)))

	  (display "No more responses after waiting 5 seconds\n")))))




