#! /usr/bin/env racket
#lang racket

(require web-server/servlet-env
         racket/runtime-path web-server/http
         web-server/private/dispatch-server-unit
         web-server/private/dispatch-server-sig
         web-server/private/connection-manager
         web-server/http/response
         web-server/http/request
         web-server/http/request-structs
         racket/async-channel
         unstable/contract
         net/tcp-sig
         (prefix-in raw: net/tcp-unit)
         file/sha1
         net/base64
         web-server/http/request-structs)

(define framing-mode (make-parameter 'rfc))

(struct ws-conn ([closed? #:mutable] line headers ip op)
  #:property prop:evt (struct-field-index ip))
(define (open-ws-conn? x)
  (and (ws-conn? x) (not (ws-conn-closed? x))))

(define (write-ws-frame! t s op)
  (define l (bytes-length s))
  (define data (make-bytes l))
  (define mask (bytes (random 255) (random 255) (random 255) (random 255)))
  (for ([i (in-range l)])
    (bytes-set! data i 
                (bitwise-xor (bytes-ref s i)
                             (bytes-ref mask (modulo i 4)))))
  (define flags 129)
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
; reading from the input port after dealing with the opcode
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
     #t]
    [(#x9) ; ping
     (displayln "Ping")
     ; let's reply with a pong...
     (write-byte #xA op)
     (write-byte mask/payload-len op)
     (write-bytes extra-payload-bytes op)
     (write-bytes masking-key op)
     (write-bytes payload-data-maybe-masked op)
     (flush-output op)
     #t]
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
     #t]
    [else (printf "Unrecognized control message: ~s~n" code) #t]))

(define (error-if-eof v)
  (when (eof-object? v) (error 'read-ws-frame "Premature connection close")))
(define-syntax-rule (define/check-eof id expr)
  (begin (define id expr)
         (error-if-eof id)))
(define-syntax-rule (print-with-label id)
  (printf "~s: ~s~n" 'id id))

(define (read-ws-frame ip op)
  (case (framing-mode)
    [(rfc)
     (define/check-eof fin/rsv/opcode (read-byte ip))
     (define opcode (bitwise-and fin/rsv/opcode #b00001111))
     (define/check-eof mask/payload-len (read-byte ip)) ; 8-15
     (define payload/first (bitwise-and mask/payload-len #b01111111))
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
     (define is-masking-on? (bitwise-and mask/payload-len #b10000000))
     (define masking-key (cond [(= is-masking-on? #b10000000) (read-bytes 4 ip)]
                               [else #""]))
     (define payload-data-maybe-masked (read-bytes payload-size ip))
     ;; assume no extension data
     (define application-data
       (cond
         [(not (equal? masking-key #""))
          (define data (make-bytes (bytes-length payload-data-maybe-masked)))
          (for ([i (in-range (bytes-length payload-data-maybe-masked))])
            (bytes-set! data i 
                        (bitwise-xor (bytes-ref payload-data-maybe-masked i)
                                     (bytes-ref masking-key (modulo i 4)))))
          data]
         [else payload-data-maybe-masked]))
     (handle-opcode opcode 
                    mask/payload-len
                    extra-payload-bytes
                    masking-key
                    payload-data-maybe-masked
                    ip
                    op)
     (values #xff application-data)]
    [(old)
     (let ()
       (define l (read-byte ip))
       (cond [(eof-object? l) (values #x00 #"")]
             [(= #xff l)
              (read-byte ip)
              (values #x00 #"")]
             [else
              (values #xff (bytes->string/utf-8 (read-until-byte #xff ip)))]))]))

(define (read-until-byte b ip)
  (define ob (open-output-bytes))
  (let loop ()
    (define n (read-byte ip))
    (unless (or (eof-object? n) (= n b))
      (write-byte n ob)
      (loop)))
  (get-output-bytes ob))

(define (ws-send! wsc s)
  (match-define (ws-conn _ _ _ _ op) wsc)
  (write-ws-frame! #xff s op))

(define (ws-recv wsc)
  (match-define (ws-conn _ _ _ ip op) wsc)
  (define-values (ft m) (read-ws-frame ip op))
  (if (= #x00 ft)
      eof
      m))

(define (ws-close! wsc)
  (match-define (ws-conn _ _ _ ip op) wsc)
  
  (case (framing-mode)
    [(new)
     (write-ws-frame! #x00 "" op)]
    [(old)
     (write-byte #xff op)
     (write-byte #x00 op)
     (flush-output op)])
  
  (close-input-port ip)
  (close-output-port op)
  (set-ws-conn-closed?! wsc #t))

(define (ws-serve conn-dispatch
                  #:tcp@ [tcp@ raw:tcp@]
                  #:port [port 80]
                  #:listen-ip [listen-ip #f]
                  #:max-waiting [max-waiting 4]
                  #:timeout [initial-connection-timeout (* 60 60)]
                  #:confirmation-channel [confirm-ch #f])
  (define (read-request c p port-addresses)
    (values #f #t))
  (define (dispatch c _)
    (displayln (framing-mode))
    (define ip (connection-i-port c))
    (define op (connection-o-port c))
    (define cline (read-bytes-line ip 'any))
    (define headers (read-headers ip))
    (define origin (header-value (headers-assq* #"Origin" headers)))
    (define accept-magic-suffix 
      #"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
    (define accept-magic-key
      (base64-encode
       (sha1-bytes
        (open-input-bytes
         (bytes-append
          (header-value (headers-assq* #"Sec-WebSocket-Key" headers))
          accept-magic-suffix)))))
    
    (define (crlf op)
      (write-char (integer->char 13) op)
      (write-char (integer->char 10) op))
    
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

;;;;;;;;;;;; ===================================================================================
;;;;;;;;;;;; ===================================================================================
;;;;;;;;;;;; ===================================================================================
;;;;;;;;;;;; ===================================================================================
;;;;;;;;;;;; ===================================================================================
;;;;;;;;;;;; ===================================================================================

(framing-mode 'rfc)
(ws-serve
 #:listen-ip "localhost"
 #:port 8080
 (λ (wsc)
   (let loop ()
     (define m (ws-recv wsc))
     (printf "~s~n" m)
     (unless (eof-object? m)
       (ws-send! wsc m)
       (loop)))))
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