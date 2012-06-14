#! /usr/bin/env racket
#lang racket/base

(require racket/runtime-path
         racket/contract
         racket/match
         racket/unit
         racket/async-channel
         data/queue
         (prefix-in raw: net/tcp-unit)
         file/sha1
         net/base64
         net/tcp-sig
         web-server/private/dispatch-server-unit
         web-server/private/dispatch-server-sig)

(struct ws-conn ([closed? #:mutable] line headers session-id ip op)
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
  (define flags 129)
  (write-byte flags op)
  (cond [(< l 126)
         ; masking version
         ; (write-byte (bitwise-ior l 128) op)
         (write-byte l op)]
        [(< l 65536)
         ; masking version
         ;(write-byte (bitwise-ior 126 128) op)
         (write-byte 126 op)
         (write-bytes (integer->integer-bytes l 2 #f #t) op)]
        [(< l 9.22337204e18)
         ; masking version
         ;(write-byte (bitwise-ior 127 128) op)
         (write-byte 127 op)
         (write-bytes (integer->integer-bytes l 8 #f #t) op)
         (displayln "big send")]
        [else (displayln "no send")])
  ;; mask or don't mask data
  ;(define mask (bytes (random 255) (random 255) (random 255) (random 255)))
  ;(define data (mask-or-unmask s mask))
  ;(write-bytes mask op)
  ;(write-bytes data op)  
  (write-bytes s op)
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
  (match-define (ws-conn _ _ _ _ _ op) wsc)
  (write-ws-frame! s op))

(define (ws-recv wsc)
  (ws-conn? . -> . bytes?)
  (match-define (ws-conn _ _ _ _ ip op) wsc)
  (read-ws-frame ip op (make-queue)))

(define (ws-close! wsc)
  (ws-conn? . -> . void)
  (match-define (ws-conn _ _ _ _ ip op) wsc)
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
    (define session-id
      (match (regexp-split #rx" " cline)
        [(list #"GET" path #"HTTP/1.1")
         (match (regexp-split #rx"session=" path) [(list #"/?" sid) sid])]))
    (define headers (read-headers ip))
    (define origin (header-value (headers-assq* #"Origin" headers)))
    (define accept-magic-suffix #"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
    (define accept-magic-key 
      (base64-encode (sha1-bytes (open-input-bytes (bytes-append
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
    (for-each (match-lambda [(struct header (field value))
                             (fprintf op "~a: ~a" field value)
                             (crlf op)])
              conn-headers)
    ;(crlf op)
    (flush-output op)
    (define conn (ws-conn #f cline conn-headers session-id ip op))
    (conn-dispatch conn))
  (define (read-request c p port-addresses)
    (values #f #t)) ; do not delete! dynamically linked here, though it's not clear from drracket tool output.
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
(require web-server/servlet-env
         web-server/http
         web-server/http/request
         web-server/http/request-structs
         web-server/private/connection-manager
         (planet dherman/json:4:=0)
         net/sendurl
         "../bindings/nacl/libnacl/libnacl.rkt"
         "../peer/src/net/base64-url-typed.rkt"
         "../Motile/compile/serialize.rkt"
         "../Motile/actor/curl.rkt"
         "../Motile/actor/delivery.rkt")

(define BROWSER-PORT 8000)
(define BROWSER-PATH "/ui.html")

(define-runtime-path files "files")
;(directory-list files)
; http://docs.racket-lang.org/web-server/run.html
(define (start req)
  (response/xexpr
   `(html (body "Hello world!"))))
(thread (λ () (serve/servlet start
                             #:port BROWSER-PORT
                             #:servlet-regexp #rx"/foo"
                             #:extra-files-paths (list files)
                             #:servlet-path BROWSER-PATH
                             #:launch-browser? #f
                             )))

(define (maybe-string->bytes s)
  (cond [(string? s) (string->bytes/utf-8 s)] 
        [(bytes? s) s]
        [else (error 'ws-serve "need string or bytes")]))

(struct session (id ; bytes? the session ID 
                 cb ; procedure? the callback fired when a message is recieved on the server
                 [out-thd #:mutable] ; the thread that queues up outgoing messages for sending
                 ready-sema ; semaphore? posted once to indicate connection openness
                 )
  #:property prop:evt (struct-field-index ready-sema))
(define/contract sessions (hash/c bytes? session?) (make-hash))

(define (listener s wsc)
  (let loop ()
    (define retval (json->jsexpr 
                    (bytes->string/utf-8 
                     (ws-recv wsc))))
    ((session-cb s)
     (delivery (car retval)
               (cdr retval)))
    (loop)))

(ws-serve
 #:listen-ip "localhost"
 #:port 8080
 (λ (wsc)
   (define pid (current-thread))
   (define s (hash-ref sessions (ws-conn-session-id wsc)))
   (when (not (session-out-thd s))
     (set-session-out-thd! s pid)
     (displayln "websocket connection being served")
     (displayln (ws-recv wsc))
     (define it (thread (λ () (listener s wsc))))
     (displayln "making ready")
     (semaphore-post (session-ready-sema s)))
   (let loop ()
     (define m (thread-receive))
     (ws-send! wsc (maybe-string->bytes m))
     (loop))))

(define (open-a-tab cb)
  (define-values (pk _) (nacl:crypto-box-keypair))
  (define id (base64-url-encode pk))
  (define s (session id cb #f (make-semaphore 0))) ; fill in the output thread when it is created upon websocket connection creation
  (hash-set! sessions id s)
  (send-url (string-append "http://localhost:" (number->string BROWSER-PORT) BROWSER-PATH "?session=" (bytes->string/utf-8 id)))
  s)

(struct interface-action (identifier json-spec))
(struct callback (curl-to-notify source))

(define-syntax-rule (json k v ...)
  (jsexpr->json (hasheq k v ...)))

(define (make-interface-action-id label)
  (define-values (pk _) (nacl:crypto-box-keypair))
  (define id (base64-url-encode pk))
  (bytes->string/utf-8 id))

(define/contract (new-button label cb)
  (string? callback? . -> . interface-action?)
  (define id (make-interface-action-id label))
  (interface-action id (json 'action "newitem" 'id id 'item "button" 'label label 'callback (callback-source cb))))

(define (new-menu label)
  (define id (make-interface-action-id label))
  (interface-action id (json 'action "newitem" 'id id 'item "menu" 'label label)))

(define/contract (new-menu-item label parent cb)
  (string? interface-action? callback? . -> . interface-action?)
  (define id (make-interface-action-id label))
  (interface-action id (json 'action "newitem" 'id id 'item "menuitem" 'menuid (interface-action-identifier parent)
                             'label label 'callback (callback-source cb))))

(define (new-dropdown label data)
  (define id (make-interface-action-id label))
  (interface-action id (json 'action "newitem" 'id id 'item "dropdown" 'label label 'data data)))

(define (new-canvas label w h)
  (define id (make-interface-action-id label))
  (interface-action id (json 'action "newitem" 'id id 'item "canvas" 'label label 'width w 'height h)))

(define/contract (new-chart type title subtitle)
  ((one-of/c 'spline 'line 'pie 'bar) string? string?  . -> . interface-action?) 
  (define id (make-interface-action-id title))
  (interface-action id (json 'action "newitem" 'id id 'item "chart" 'type (symbol->string type) 'title title 'subtitle subtitle)))

(define/contract (update-canvas-contents c data)
  (interface-action? bytes? . -> . interface-action?)
  (define id (interface-action-identifier c))
  (interface-action #"" (json 'action "updateitem" 'id id 'item "canvas" 'data (bytes->list data))))

(define/contract (plot-a-point c x y)
  (interface-action? number? number? . -> . interface-action?)
  (define id (interface-action-identifier c))
  (interface-action #"" (json 'action "updateitem" 'id id 'item "chart" 'data (list x y))))

; make a callback for e.g. a button.
; deliver the results of evaluating the javascript expression EXPR to 
; the curl C.
;;;; XXX BIG SECURITY HOLE NEED TO FIX XXX
; ensure that only side effect free computations occur during the evaluation of EXPR
(define/contract (make-callback c expr)
  ((or/c curl? #f) string? . -> . callback?)
  (define c/serialized/str (let ([o (open-output-string)]) (write (motile/serialize c) o) (get-output-string o)))
  (callback c
            (string-append ;"function() {"
             "    alert(\'sending something\');"
             "    websocket.send(JSON.stringify("
             "        function() { return [\"" c/serialized/str "\"," expr "]}()"
             "    ));"
             ;"}"
             )))

;; actions

(define/contract (ui-ready-to-send? s)
  (session? . -> . boolean?)
  (thread? (session-out-thd s)))
(define/contract (ui-wait-for-readiness s)
  (session? . -> . void)
  (semaphore-wait (session-ready-sema s)))
(define/contract (ui-send! s v)
  (session? interface-action? . -> . void)
  ;(displayln (interface-action-json-spec v))
  (thread-send (session-out-thd s) (interface-action-json-spec v) 
               (λ () (displayln "Error: no connection to browser UI!"))))

(provide new-button
         new-menu
         new-menu-item
         new-dropdown
         new-canvas
         new-chart
         update-canvas-contents
         plot-a-point
         make-callback
         ui-ready-to-send?
         ui-wait-for-readiness
         ui-send!
         open-a-tab)