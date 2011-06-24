(define (thunk/google-news)
  (define (most-freq-word w)
    (let ((r (cons "" 0)))
      (for-each (lambda (a) (if (> (cdr a) (cdr r)) (set! r a))) w)
      (car r)))

  (let ((current-date "2009-08-18T08:47:55-07:00")
        (current-keyword "michael jackson")
        (links '())
        )
    (let loop ((m (? (this-mailbox))))
      (match
       (message/path+body+metadata m)

       (#(link/create ,mailbox ,metadata)
        (set! links (cons mailbox links))
        (! (@ mailbox 'link/data) (cons current-date current-keyword))
        )

       (#(link/data ,s ,metadata)
        (if s
          (cond ((string-ci=? "date" metadata) (set! current-date s))
                ((string-ci=? "text" metadata) (set! current-keyword (most-freq-word s)))
          ))
       )

       (#(/http/get #(,origin ,uri ,request ,response) ,metadata)
         (http/response/entity! response (json/string (list (cons 'items (list->vector(list (list (cons "date" current-date)(cons "text" current-keyword))))))))
          (! (:message/reply m) response :no-metadata: #f (:message/echo m)))

        (,_ignore #f))

      (loop (? (this-mailbox)))))
)
