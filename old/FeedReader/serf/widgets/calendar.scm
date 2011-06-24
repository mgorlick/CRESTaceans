(define (thunk/calendar)
  (let ((current-date "2009-08-18T08:47:55-07:00")
        (links '())
        )
    (let loop ((m (? (this-mailbox))))
      (match
       (message/path+body m)

       (#(link/create ,mailbox)
        (set! links (cons mailbox links))
        (! (@ mailbox 'link/data) current-date "date")
        )

       (#(/http/get #(,origin ,uri ,request ,response))
         (http/response/entity! response (json/string (list (cons 'items (list->vector(list (list (cons "date" current-date))))))))
          (! (:message/reply m) response :no-metadata: #f (:message/echo m)))

       (#(/http/post #(,origin ,uri ,req-body))
        (let* (
          (req-val (json/translate req-body))
          (table (make-hashtable string-ci=?)))
         (for-each (lambda (v) (hashtable/put! table (car v) (cdr v))) req-val)
         (let* ((new-date (hashtable/get table "date")))
          (set! current-date new-date)
          (for-each (lambda (link) (! (@ link 'link/data) new-date "date")) links)
         )))

        (,_ignore #f))

      (loop (? (this-mailbox)))))
)
