(define (thunk/sparkline)

  (define (create-count s w)
    (cons w
          (let ((a '()))
           (for-each (lambda (l)
            (map (lambda (l-w)
              (if (string-ci=? w (car l-w))
                   (set! a (cons (cdr l-w) a))))
              l))
            s)
            a
          )))

  (define (create-list s)
    (let ((l '()))
      (if s (for-each (lambda (w) (set! l (cons (create-count s (car w)) l))) (car s)))
      ;(for-each (lambda (w) (display (format "~a\n" w))) l)
      l
    ))


  (let ((current-csv #f)
        (current-raw `())
        (current-list `())
        (links '()))
    (let loop ((m (? (this-mailbox))))
      (match
       (message/path+body m)

       (#(link/create ,mailbox)
        (set! links (cons mailbox links))
        (! (@ mailbox 'link/data) current-text))

       (#(link/data ,s)
        (if s (set! current-raw (cons s current-raw)))
        (set! current-list (create-list current-raw))
        (let ((retval '()))
          (define (list-walk i c default)
            (if (<= c 30) (begin (set! retval (cons (map (lambda (l) (if (< c (length l)) (number->string (list-ref l c)) default)) i) retval))
            (list-walk i (+ c 1) default))))
          (list-walk current-list 1 "0")
          (set! retval (cons (map (lambda(l) (format "~s" l))(unzip1 current-list)) retval))
          (set! current-csv (string-join (map (lambda (l) (string-join l ",")) retval) "\n"))
        )
        (for-each (lambda (link) (! (@ link 'link/data) current-list)) links)
        )

       (#(/http/get #(,origin ,uri ,request ,response))
        (cond
          (current-csv
            (http/response/entity! response current-csv))
          (else
            (http/response/status! response 404)
            (http/response/entity! response "Not Found")))
        (! (:message/reply m) response  :no-metadata: #f (:message/echo m)))
      )

      (loop (? (this-mailbox)))))
)
