#! /usr/bin/env racket
#lang racket/base

(require racket/runtime-path
         racket/contract
         racket/match
         racket/unit
         racket/async-channel
         net/tcp-sig
         data/queue
         (prefix-in raw: net/tcp-unit)
         file/sha1
         net/base64 web-server/servlet-env
         web-server/http
         web-server/http/request
         web-server/http/request-structs
         web-server/private/dispatch-server-unit
         web-server/private/dispatch-server-sig
         web-server/private/connection-manager)

(struct ws-conn ([closed? #:mutable] line headers ip op)
  #:property prop:evt (struct-field-index ip))
(define (open-ws-conn? x)
  (and (ws-conn? x) (not (ws-conn-closed? x))))

(define/contract (mask-or-unmask src mask)
  (bytes? bytes? . -> . bytes?)
  (define dest (make-bytes (bytes-length src)))
  (for ([i (in-range (bytes-length dest))])
    (bytes-set! dest i 
                (bitwise-xor (bytes-ref src i)
                             (bytes-ref mask (modulo i 4)))))
  dest)

(define/contract (write-ws-frame! s op)
  (bytes? output-port? . -> . void)
  (define l (bytes-length s))
  (define mask (bytes (random 255) (random 255) (random 255) (random 255)))
  (define flags 129)
  (define data (mask-or-unmask s mask))
  (write-byte flags op)
  (cond [(< l 126)
         (write-byte (bitwise-ior l 128) op)]
        [(< l 65536)
         (write-byte 126 op)
         (write-bytes (integer->integer-bytes l 2 #f #t) op)]
        [(< l 9.22337204e18)
         (write-byte 127 op) 
         (write-bytes (integer->integer-bytes l 8 #f #t) op)])
  (write-bytes mask op)
  (write-bytes data op)  
  (flush-output op))

; return #t if the opcode seen permits us to CONTINUE
; with the application data after returning from `handle-opcode'
; or if we should just throw it away.
(define/contract (handle-opcode code 
                                mask/payload-len
                                extra-payload-bytes
                                masking-key
                                payload-data-maybe-masked
                                ip op)
  (byte? 
   byte? bytes? bytes? bytes? 
   input-port? output-port? . -> . boolean?)
  (case code
    [(#x8) ; connection close
     (displayln "Connection close")
     #f]
    [(#x9) ; ping
     (displayln "Ping")
     ; let's reply with a pong...
     (write-byte #xA op)
     (write-byte mask/payload-len op)
     (write-bytes extra-payload-bytes op)
     (write-bytes masking-key op)
     (write-bytes payload-data-maybe-masked op)
     (flush-output op)
     #f]
    [(#xA) ; pong
     (displayln "Pong")
     ;   A Pong frame sent in response to a Ping frame must have identical
     ;   "Application data" as found in the message body of the Ping frame
     ;   being replied to.
     
     ;   If an endpoint receives a Ping frame and has not yet sent Pong
     ;   frame(s) in response to previous Ping frame(s), the endpoint MAY
     ;   elect to send a Pong frame for only the most recently processed Ping
     ;   frame.
     
     ;   A Pong frame MAY be sent unsolicited.  This serves as a
     ;   unidirectional heartbeat.  A response to an unsolicited Pong frame is;;
     ;   not expected.
     
     ; WE DON'T CARE!
     #f]
    [else #t]))

(define (error-if-eof v)
  (when (eof-object? v) (error 'read-ws-frame "Premature connection close")))
(define-syntax-rule (define/check-eof id expr)
  (begin (define id expr)
         (error-if-eof id)))
(define-syntax-rule (print-with-label id)
  (printf "~s: ~s~n" 'id id))

(define/contract (extract-opcode fin/rsv/opcode)
  (byte? . -> . byte?)
  (bitwise-and fin/rsv/opcode #b00001111))
(define/contract (extract-payload-indicator payload/mask)
  (byte? . -> . byte?)
  (bitwise-and payload/mask #b01111111))
(define/contract (end-of-message? fin/rsv/opcode)
  (byte? . -> . boolean?)
  (bitwise-bit-set? fin/rsv/opcode 0))
(define/contract (is-masking-on? mask/payload-len)
  (byte? . -> . boolean?)
  (= (bitwise-and mask/payload-len #b10000000) #b10000000))

(define/contract (read-ws-frame ip op q)
  (input-port? output-port? queue? . -> . void)
  (define/check-eof fin/rsv/opcode (read-byte ip))
  (define opcode (extract-opcode fin/rsv/opcode))
  (define/check-eof mask/payload-len (read-byte ip)) ; 8-15
  (define payload/first (extract-payload-indicator mask/payload-len))
  (define-values (payload-size extra-payload-bytes)
    (cond [(< payload/first 126)
           (values payload/first #"")]
          [(= payload/first 126)
           (define/check-eof payload/next-2 (read-bytes 2 ip))
           (values (integer-bytes->integer payload/next-2 #f #t)
                   payload/next-2)]
          [(= payload/first 127)
           (define/check-eof payload/next-8 (read-bytes 8 ip))
           (values (integer-bytes->integer payload/next-8 #f #t)
                   payload/next-8)]))
  (define masking-key (cond [(is-masking-on? mask/payload-len) (read-bytes 4 ip)]
                            [else #""]))
  (define payload-data-maybe-masked (read-bytes payload-size ip))
  ;; assume no extension data
  (define application-data
    (if (is-masking-on? mask/payload-len)
        (mask-or-unmask payload-data-maybe-masked masking-key)
        payload-data-maybe-masked))
  (define give-value-to-user?
    (handle-opcode opcode mask/payload-len extra-payload-bytes 
                   masking-key payload-data-maybe-masked ip op))
  (cond [(and give-value-to-user? (end-of-message? fin/rsv/opcode))
         (enqueue! q application-data)
         (get-output-bytes (foldl (λ (b o) (write-bytes b o) o) (open-output-bytes) (queue->list q)))]
        [give-value-to-user?
         (enqueue! q application-data)
         (read-ws-frame ip op q)]
        [else
         ; we dealt with a control code.
         ; now read the frame the user really wanted.
         (read-ws-frame ip op (make-queue))]))

;; ------
; Public WS ui.
;; ------


(define/contract (ws-send! wsc s)
  (ws-conn? bytes? . -> . void)
  (match-define (ws-conn _ _ _ _ op) wsc)
  (write-ws-frame! s op))

(define (ws-recv wsc)
  (ws-conn? . -> . bytes?)
  (match-define (ws-conn _ _ _ ip op) wsc)
  (read-ws-frame ip op (make-queue)))

(define (ws-close! wsc)
  (ws-conn? . -> . void)
  (match-define (ws-conn _ _ _ ip op) wsc)
  ;; XXX FIXME @@@
  (close-input-port ip)
  (close-output-port op)
  (set-ws-conn-closed?! wsc #t))

;; ----
;; serve WS.
;; ----

(define (ws-serve conn-dispatch
                  #:tcp@ [tcp@ raw:tcp@]
                  #:port [port 80]
                  #:listen-ip [listen-ip #f]
                  #:max-waiting [max-waiting 4]
                  #:timeout [initial-connection-timeout (* 60 60)]
                  #:confirmation-channel [confirm-ch #f])
  (define (dispatch c _)
    (define ip (connection-i-port c))
    (define op (connection-o-port c))
    (define cline (read-bytes-line ip 'any))
    (define headers (read-headers ip))
    (define origin (header-value (headers-assq* #"Origin" headers)))
    (define accept-magic-suffix #"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
    (define accept-magic-key
      (base64-encode
       (sha1-bytes
        (open-input-bytes
         (bytes-append
          (header-value (headers-assq* #"Sec-WebSocket-Key" headers))
          accept-magic-suffix)))))
    (define (crlf op)
      (write-char #\return op)
      (write-char #\newline op))
    (define conn-headers
      (list 
       (make-header #"Upgrade" #"WebSocket")
       (make-header #"Connection" #"Upgrade")
       (make-header #"Sec-WebSocket-Origin" origin)
       (make-header #"Sec-WebSocket-Location"
                    #"ws://localhost:8080/a-websocket-location")
       (make-header #"Sec-WebSocket-Accept" accept-magic-key)))
    
    (fprintf op "HTTP/1.1 101 WebSocket Protocol Handshake")
    (crlf op)
    (for-each (match-lambda
                [(struct header (field value))
                 (fprintf op "~a: ~a" field value)
                 (crlf op)])
              conn-headers)
    ;(crlf op)
    (flush-output op)
    (define conn (ws-conn #f cline conn-headers ip op))
    (conn-dispatch conn))
  
  
  (define (read-request c p port-addresses)
    (values #f #t)) ; do not delete! dynamically linked here
  (define-unit-binding a-tcp@
    tcp@ (import) (export tcp^))
  (define-compound-unit/infer dispatch-server@/tcp@
    (import dispatch-server-config^)
    (link a-tcp@ dispatch-server@)
    (export dispatch-server^))
  (define-values/invoke-unit
    dispatch-server@/tcp@
    (import dispatch-server-config^)
    (export dispatch-server^))
  (serve #:confirmation-channel confirm-ch))

;; ===================================================================================
;; ===================================================================================
;; ===================================================================================
(require (planet dherman/json:4:=0))

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

(define (run-the-program t)
  (let loop ()
    (displayln "sending to connection")
    (thread-send t
                 (jsexpr->json
                  (hasheq 'action "newitem"
                          'item "button"
                          'label "foo"
                          'callback "function() { alert(\"foo\"); }")))
    (sleep 1)
    (loop)))

(define (maybe-string->bytes s)
  (cond [(string? s) (string->bytes/utf-8 s)] 
        [(bytes? s) s]
        [else (error 'ws-serve "need string or bytes")]))

(ws-serve
 #:listen-ip "localhost"
 #:port 8080
 (λ (wsc)
   (displayln "websocket connection being served")
   (define pid (current-thread))
   (thread (λ () (run-the-program pid)))
   (let loop ()
     (displayln "loop")
     (define m (thread-receive))
     (displayln "sending to browser")
     (ws-send! wsc (maybe-string->bytes m))
     (loop))))