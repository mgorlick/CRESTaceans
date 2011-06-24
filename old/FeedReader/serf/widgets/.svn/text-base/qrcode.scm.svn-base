(define (thunk/qrcode)

  (let ((current-url #f))
    (let loop ((m (? (this-mailbox))))
      (match
       (message/path+body m)

       (#(link/data ,s)
        (set! current-url s))

       (#(/http/get #(,origin ,uri ,request ,response))
        (let* (
           (host-hdr (http/request/header request "Host"))
           (demo-uri (if current-url current-url (format "http://~a/static/dojo/demo/demo.html" (cdr host-hdr))))
           )
           (http/response/entity! response (json/string (list (cons 'items (list->vector(list (list (cons "url" demo-uri))))))))
           (! (:message/reply m) response :no-metadata: #f (:message/echo m))
           ))

        (,_ignore #f)
      )

      (loop (? (this-mailbox)))))
)
