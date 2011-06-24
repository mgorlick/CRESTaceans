(define (parse-feed-content feed)
  (let* ((pl (abdera/feed/list feed))
         (pm (map abdera/entry/content pl))
         (pt (map abdera/entry/title pl)))
  (string-join (append pm pt))))

(define (generate-wordcount s l c)
  (let* ((hp (htmlparser/parse s))
         (ht (htmlparser/get-text hp))
         (wc (wordcount/list ht))
        )
   (topwc wc l c)))

(define (alist-filter list filter)
 (let loop ((pairs list) (outcome '()))
  (cond
   ((null? pairs) outcome)
    (else
     (if (filter (car pairs))
       (loop (cdr pairs) (cons (car pairs) outcome))
       (loop (cdr pairs) outcome))))))

(define (topwc s wordlen count)
  (alist-filter s (lambda (x)
    (and (> (string-length (car x)) wordlen) (>= (cdr x) count)))))

(define (thunk/rss-feed)
  (define (entry-to-alist e count)
    (list (cons "type" "message")
          (cons 'id (format "node~d" count))
          (cons 'label (abdera/entry/title e))
          (cons 'text (abdera/entry/content e))
          (cons 'sent (utc/string (abdera/entry/updated e)))))

  ; Convert a feed list (e_0 e_1 ...) to a vector where each e_i is converted to a small association list.
  (define (feed-list-to-vector things)
    (let loop ((things things)
               (count 1)
               (outcome '()))
      (cond
       ((null? things) (list->vector (reverse outcome)))
       (else
        (loop (cdr things) (+ count 1) (cons (entry-to-alist (car things) count) outcome))))))

  (define (clock-to-feed root clock)
    (let ((url (uri/new (format "~a~d.xml" root clock)))(reply (make <mailbox>))) ; Construct the feed URL.
      ; Issue a GET to the feed URI via Imposter.
      (! (@ (this-dispatch) '/http/get) `(,url #f #f) :no-metadata: (@ reply '/http/response))
      ; Deconstruct the feed response.
      (let ((rss (? reply 250 #f))) ; Wait no more than 250 milliseconds for a feed response.
        (if rss
            (let* ((feed-response (:message/body rss))
                   (response-body (vector-ref feed-response 3)))
              (abdera/parse response-body))
            #f))))

  ; Convert the feed contents to the JSON expected by the RSS Reader widget.
  (define (feed-to-json feed)
    (let ((v (feed-list-to-vector (abdera/feed/list feed))))
      (json/string (list (cons "label" "label") (cons 'items v)))))

  (define (feed-to-text feed)
    (parse-feed-content feed))

  (let ((root #f) ; http://host:port/a/b/c/ (the root URI to the feed server)
        (json  #f)  ; Latest feed text as JSON text.
        (text  #f)  ; Latest feed text as single long string.
        (counter 0)  ; Current counter
        (links '()))

    (set! counter (+ counter 1))
    (! (@ (this-mailbox) 'clock/tick) counter)
    (let loop ((m (? (this-mailbox))))
      (match
       (message/path+body m)

       (#(link/create ,mailbox)
        (set! links (cons mailbox links))
        (if text (! (@ mailbox 'link/data) text))
       )

       (#(link/data ,s) ; s is string http://host:port/a/b/c/ (the root URI to the feed server).
        (display (format "thunk/rss-feed: Updating feed to ~a\n" s))
    (set! counter 1)
    (! (@ (this-mailbox) 'clock/tick) counter)
        (set! root s))

       (#(clock/tick ,n) ; The virtual clock ticked. Fetch the next feed in the time series.
        (guard (and (integer? n) (> n 0)))
        (display (format "thunk/rss-feed: Fetching feed number ~d\n" n))
        (if root
            (let ((feed (clock-to-feed root n)))
              (if feed (let ((t '()))
                    (set! text (feed-to-text feed))
                    (set! json (feed-to-json feed))
                    (for-each (lambda (link) (! (@ link 'link/data) text)) links)
                    ))))
        (display (format "thunk/rss-feed: Fetched feed number ~d\n" n))
       )

       (#(/http/get #(,origin ,uri ,request ,response)) ; Respond to a GET request.
        (cond
         (json
          (http/response/entity! response json)
          (! (@ (this-mailbox) 'clock/tick) counter)
          (set! counter (+ counter 1))
         )
         (else
          (http/response/status! response 404)
          (http/response/reason! response "Not Found")))
         (! (:message/reply m) response :no-metadata: #f (:message/echo m)))

        (,_ignore #f)) ; Ignore anything else (including POSTs).

      (loop (? (this-mailbox)))))
)
