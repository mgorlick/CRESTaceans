(import oo)
(import serf/fiber)
(import serf/http/response)
(import serf/imposter)
(import serf/object)
(import serf/mailbox)
(import serf/message)
(import serf/uri)

;; A single request to a single authority.
(define (imposter/test/01)
  (let* ((imposter/inbox (make <mailbox>))
	 (imposter (object/forge <serf/imposter> (this-log) imposter/inbox))
	 (uri (uri/new "http" "www.google.com" 80 "/"))
	 (reply (make <mailbox>)))
    (! (@ imposter/inbox '/http/get) (list uri #f #f) :no-metadata: (@ reply '/http/response) 'google)
    (let ((m (? reply 5000 #f)))
      (if m
	  (let ((response (:message/body m)))
	    (display (format
		      "~a ~d ~a\nheaders:~a\nentity:~d\n"
		      (:message/echo m)
		      (vector-ref response 0)
		      (vector-ref response 1)
                      (vector-ref response 2)
                      (if (vector-ref response 3) (string-length (vector-ref response 3)) 0))))
	  (display "No response after waiting 5 seconds\n")))))

;; Multiple requests to a single authority.
(define (imposter/test/02)
  (let* ((imposter/inbox (make <mailbox>))
	 (imposter (object/forge <serf/imposter> (this-log) imposter/inbox))
	 (uri (uri/new "http" "www.google.com" 80 "/"))
	 (reply (make <mailbox>))
	 (body (list uri #f #f))) ; No headers and no payload.
    (! (@ imposter/inbox '/http/get) body :no-metadata: (@ reply '/http/response) 'response1)
    (! (@ imposter/inbox '/http/get) body :no-metadata: (@ reply '/http/response) 'response2)
    (! (@ imposter/inbox '/http/get) body :no-metadata: (@ reply '/http/response) 'response3)
    (let loop ((m (? reply 5000 #f)))
      (if m
	  (let ((response (:message/body m)))
	    (display (format
		      "~a ~d ~a\nheaders:~a\nentity:~d\n\n"
		      (:message/echo m)
		      (vector-ref response 0)
		      (vector-ref response 1)
                      (vector-ref response 2)
                      (if (vector-ref response 3) (string-length (vector-ref response 3)) 0)))
            (loop (? reply 5000 #f)))
          
	  (display "No response after waiting 5 seconds\n")))))

;; Nine total requests rotating thru three distinct authorities.
(define (imposter/test/03)
  (let* ((imposter/inbox (make <mailbox>))
         (reply          (make <mailbox>))
         (imposter (object/forge <serf/imposter> (this-log) imposter/inbox))

         (google  (uri/new "http" "www.google.com"  80 "/"))
         (uci     (uri/new "http" "www.isr.uci.edu" 80 "/"))
         (latimes (uri/new "http" "www.latimes.com" 80 "/"))

         (google-body  (list  google  #f #f))
         (uci-body     (list  uci     #f #f))
         (latimes-body (list latimes  #f #f))

         (to   (@ imposter/inbox '/http/get))
         (from (@ reply          '/http/response))

         (m1 (list to google-body  :no-metadata: from 'google-1))
         (m2 (list to uci-body     :no-metadata: from 'uci-1))
         (m3 (list to latimes-body :no-metadata: from 'latimes-1))

         (m4 (list to uci-body     :no-metadata: from 'uci-2))
         (m5 (list to latimes-body :no-metadata: from 'latimes-2))
         (m6 (list to google-body  :no-metadata: from 'google-2))

         (m7 (list to latimes-body :no-metadata: from 'latimes-3))
         (m8 (list to google-body  :no-metadata: from 'google-3))
         (m9 (list to uci-body     :no-metadata: from 'uci-3)))

    (for-each
     (lambda (x) (apply ! x))
     (list m1 m2 m3 m4 m5 m6 m7 m8 m9))

    (let loop ((m (? reply 5000 #f)))
      (if m
          (let ((response (:message/body m)))
            (display (format
		      "~a ~d ~a\nheaders:~a\nentity:~d\n\n"
		      (:message/echo m)
		      (vector-ref response 0)
		      (vector-ref response 1)
                      (vector-ref response 2)
                      (if (vector-ref response 3) (string-length (vector-ref response 3)) 0)))
            (loop (? reply 5000 #f)))

          (display "No more responses after waiting 5 seconds\n")))))
