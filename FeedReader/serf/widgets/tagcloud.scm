(define (thunk/tagcloud)

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
      (and (>= (string-length (car x)) wordlen) (>= (cdr x) count)))))

  (define (generate-wordcount s l c)
    (let* ((hp (htmlparser/parse s))
           (ht (htmlparser/get-text hp))
           (wc (wordcount/list ht))
         )
    (topwc wc l c)
   )
  )

  (define (nc-map e) (list (cons 'name (car e)) (cons 'count (cdr e))))
  (let ((current-json #f)
        (current-text #f)
        (links '()))
    (let loop ((m (? (this-mailbox))))
      (match
       (message/path+body m)

       (#(link/create ,mailbox)
        (set! links (cons mailbox links))
        (! (@ mailbox 'link/data) current-text "text"))

       (#(link/data ,s)
        (set! current-text (generate-wordcount s 4 3))
        (set! current-json (json/string (list (cons 'items (list->vector (map nc-map current-text))))))
        (for-each (lambda (link) (! (@ link 'link/data) current-text "text")) links)
        )

       (#(/http/get #(,origin ,uri ,request ,response))
        (cond
          (current-json
            (http/response/entity! response current-json))
          (else
            (http/response/status! response 404)
            (http/response/entity! response "Not Found")))
        (! (:message/reply m) response :no-metadata: #f (:message/echo m)))
      )

      (loop (? (this-mailbox)))))
)
