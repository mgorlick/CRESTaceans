(define (thunk/urlsel)
  (let ((current-url (uri/new "http" "localhost" 8080 "/static/feeds/espn/"))
        (links '())
        )
    (let loop ((m (? (this-mailbox))))
      (match
       (message/path+body m)

       (#(link/create ,mailbox)
        (set! links (cons mailbox links))
        (! (@ mailbox 'link/data) (uri/ascii current-url))
        )

       (#(/http/get #(,origin ,uri ,request ,response))
         (http/response/entity! response (json/string (list (cons 'items (list->vector(list (list (cons "url" (uri/ascii current-url)))))))))
          (! (:message/reply m) response :no-metadata: #f (:message/echo m)))

       (#(/http/post #(,origin ,uri ,req-body))
        (let* (
          (req-val (json/translate req-body))
          (table (make-hashtable string-ci=?)))
         (for-each (lambda (v) (hashtable/put! table (car v) (cdr v))) req-val)
         (let* ((new-url (uri/new (hashtable/get table "url"))))
          (set! current-url new-url)
          (for-each (lambda (link) (! (@ link 'link/data) (uri/ascii new-url))) links)
         )))

        (,_ignore #f))

      (loop (? (this-mailbox)))))
)
