#lang racket/base

(require web-server/servlet-env
         racket/runtime-path)

(define-runtime-path files "files")
;(directory-list files)
; http://docs.racket-lang.org/web-server/run.html
(define (start req)
  (response/xexpr
   `(html (body "Hello world!"))))
(thread (λ () (serve/servlet start
                             #:port 8000
                             #:servlet-regexp #rx"/foo"
                             #:extra-files-paths (list files)
                             #:servlet-path "/ui.html"
                             )))

(require net/websocket
         net/websocket/conn
         web-server/http)

(ws-serve
 #:listen-ip "localhost"
 #:port 8080
 (λ (wsc _)
   (let loop ()
     (define m (ws-recv wsc))
     (printf "~a\n" m)
     (unless (eof-object? m)
       (ws-send! wsc m)
       (loop))))
 #:conn-headers
 (λ (_ hs)
   (define origin
     (header-value (headers-assq* #"Origin" hs)))
   (values
    (list
     (make-header #"Sec-WebSocket-Origin" origin)
     (make-header #"Sec-WebSocket-Location"
                  #"ws://localhost:8080/"))
    #f)))