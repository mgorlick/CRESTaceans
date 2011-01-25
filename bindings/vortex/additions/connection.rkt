#lang racket

(require racket/tcp 
         openssl
         (except-in ffi/unsafe ->)
         "../vtx/module.rkt")
(provide rkt:vortex-connection-set-client-mode-closures
         rkt:vortex-connection-set-listener-mode-closures)

;; with-key (wk):
;; bind a set of keys (using vortex-connection-get-id ...)
;; this just cuts down on syntactic noise
(define-syntax wk
  (syntax-rules ()
    [(with-key ([id conn] ...) body ...)
     (let ([id (vortex-connection-get-id conn)]
           ...)
       body ...)]))

;; apply the same error-handling pattern: return -1 if there's an error inside
;; functions that touch the network (this is what Vortex expects from the BSD socket API)
(define-syntax handle-neterr
  (syntax-rules ()
    [(wh body ...)
     (with-handlers ([exn:fail:network? (lambda (e) (printf "Network exception: ~s ~n" e) -1)]
                     [exn:fail? (lambda (e) (printf "Non-network exception: ~s ~n" e) -1)]
                     )
       body ...)]))

;; read the designated amount from the designated connection's input port
;; return length of read when done, 0 if encountered only #<eof> or -1 on network error
(define-syntax-rule (define-read/tcp id inports)
  (define/contract (id conn buffer buffer-len)
    (VortexConnection*? cpointer? integer?  . -> . integer?)
    (handle-neterr
     (wk ([key conn])
         (let ([s (read-bytes buffer-len (hash-ref inports key))])
           (if (eof-object? s) ; returns `#<eof>' or some number
               0 
               (begin
                 (ptr-set! buffer _bytes s)
                 (bytes-length s))))))))

;; write the designated amount onto the designated connection's output port
;; return the amount written, or -1 on network error
(define-syntax-rule (define-write/tcp id outports)
  (define/contract (id conn buffer buffer-len)
    (VortexConnection*? cpointer? integer? . -> . integer?)
    (handle-neterr
     (wk ([key conn])
         (let* ([payload (ptr-ref buffer (_bytes o buffer-len))]
                [amt (write-bytes payload (hash-ref outports key) 0 buffer-len)])
           (flush-output (hash-ref outports key))
           amt)))))

;; close the connection's input and output ports, remove from the map,
;; unref the connection and return 1 when done, or -1 on network error
(define-syntax-rule (define-close/tcp id inports outports listeners)
  (define/contract (id conn who)
    (VortexConnection*? string? . -> . integer?)
    ;(printf "close/tcp called by ~a~n" who)
    (handle-neterr
     (wk ([key conn])
         (close-input-port (hash-ref inports key))
         (close-output-port (hash-ref outports key))
         (hash-remove! inports key)
         (hash-remove! outports key)
         (cond [(and (hash? listeners)
                     (eq? (vortex-connection-get-role conn) 'master-listener))
                (hash-remove! listeners key)])
         (vortex-connection-unref conn "close/tcp")
         1))))

;; perform a wait operation on the connection
;; if `timeout' is non-negative, allow timing out
;; return 1 when done, or -1 on network error

;; the `sync-on' argument is a lambda:
;; (lambda (conn key)
;;   (... specify the port used to sync on (either inport or outport) ....))
(define-syntax-rule (define-wait/tcp id sync-on)
  (define/contract (id conn timeout)
    (VortexConnection*? integer? . -> . integer?)
    (handle-neterr
     (wk ([key conn])
         (if (> timeout -1)
             (let ([res (sync/timeout (/ timeout 1000) (sync-on conn key))]) ; wait with a timeout
               (if (false? res)
                   -1
                   1))
             (let ([res (sync (sync-on conn key))]) ; wait without a timeout
               1))))))

;; take four char**, then write the connection's local and remote
;; addresses and ports into those char**
;; return 1 if successful, -1 if not
(define-syntax-rule (define-get-sock-name/tcp id addresses inports)
  (define/contract (id conn local-addr* local-port* remote-addr* remote-port*)
    (VortexConnection*? cpointer? cpointer? cpointer? cpointer? . -> . integer?)
    (handle-neterr
     (wk ([key conn])
         (let-values ([(locala localp remotea remotep) (addresses (hash-ref inports key) #t)])
           (ptr-set! local-addr* _string locala)
           (ptr-set! local-port* _string (number->string localp))
           (ptr-set! remote-addr* _string remotea)
           (ptr-set! remote-port* _string (number->string remotep))
           1)))))

;;; -----------
;;; CLIENT MODE
;;; -----------

; given a new connection, turn into client mode by registering function pointers
; to closures that close over the tcp listener.
(define/contract (rkt:vortex-connection-set-client-mode-closures this-connection use-ssl? ssl-cert-path)
  (VortexConnection*? boolean? (or/c path-string? false?) . -> . void)
  (define inports (make-hash))
  (define outports (make-hash))
  (define-values (connect addresses)
    (cond
      [use-ssl? (values ssl-connect ssl-addresses)]
      [else (values tcp-connect tcp-addresses)]))
  
  ;; given a new connection, a host and a port, connect
  ;; and bind the resulting input and output ports
  ;; and the socket addresses
  ;; return 1 when done or -1 on network error
  (define/contract (client/connect conn host port)
    (VortexConnection*? string? string? . -> . integer?)
    (handle-neterr
     (wk ([key conn])
         (vortex-connection-ref conn "connect/tcp")
         (let-values ([(in out) (connect host (string->number port))])
           (hash-set! inports key in)
           (hash-set! outports key out)
           (vortex-connection-set-socket conn 1 #f #f)
           1)
         )))
  
  (define-read/tcp client/read inports)
  (define-write/tcp client/write outports)
  (define-close/tcp client/close inports outports #f)
  (define-wait/tcp client/wait/read (lambda (conn key) (hash-ref inports key)))
  (define-wait/tcp client/wait/write (lambda (conn key) (hash-ref outports key)))
  (define-get-sock-name/tcp client/getsockname addresses inports)
  
  ;; transfer all of the above closures to the vortex side to be opaquely invoked
  (vortex-connection-set-client-mode-closures this-connection
                                              client/connect client/read client/write client/close
                                              client/getsockname client/wait/read client/wait/write))
;;; -------------
;;; LISTENER MODE
;;; -------------

; given a new connection, turn it into listener mode by registering function pointers
; to closures that close over the tcp listener.
(define/contract (rkt:vortex-connection-set-listener-mode-closures this-connection use-ssl? ssl-cert-path)
  (VortexConnection*? boolean? (or/c path-string? false?) . -> . void)
  (define inports (make-hash))
  (define outports (make-hash))
  (define listeners (make-hash))
  (define-values (listen accept addresses)
    (cond
      [use-ssl? (values ssl-listen ssl-accept ssl-addresses)]
      [else (values tcp-listen tcp-accept tcp-addresses)]))
  
  ;; listen on the given host/port (both strings) and set the
  ;; master listener for this connection
  ;; return 1 when done, or -1 if network error
  (define/contract (listener/listen conn host port)
    (VortexConnection*? string? string? . -> . integer?)
    (handle-neterr
     (wk ([key conn])
         (vortex-connection-ref conn "listen/tcp")
         (if (eq? host #f)
             (hash-set! listeners key (listen (string->number port) 10000 #t))
             (hash-set! listeners key (listen (string->number port) 10000 host)))
         (cond [use-ssl?
                (ssl-load-certificate-chain! (hash-ref listeners key) ssl-cert-path)
                (ssl-load-private-key! (hash-ref listeners key) ssl-cert-path)])
         1)))
  
  ;; accept a connection request with the master listener, and the new child listener
  ;; to bind together with the connection's input and output ports
  ;; return 1 when done, or -1 if network error
  (define/contract (listener/accept masterconn childconn)
    (VortexConnection*? VortexConnection*? . -> . integer?)
    (handle-neterr
     (wk ([masterkey masterconn] [childkey childconn])
         (vortex-connection-ref childconn "accept/tcp")
         (let-values ([(in out) (accept (hash-ref listeners masterkey))])
           (hash-set! inports childkey in)
           (hash-set! outports childkey out)
           (vortex-connection-set-socket childconn 1 #f #f)
           1))))
  
  (define-read/tcp listener/read inports)
  (define-write/tcp listener/write outports)
  (define-close/tcp listener/close inports outports listeners)
  (define-wait/tcp listener/wait/read (lambda (conn key)
                                        (cond
                                          [(eq? (vortex-connection-get-role conn) 'master-listener) 
                                           (hash-ref listeners key)]
                                          [else (hash-ref inports key)])))
  (define-wait/tcp listener/wait/write (lambda (conn key)
                                         (cond
                                           [(eq? (vortex-connection-get-role conn) 'master-listener) 
                                            (hash-ref listeners key)]
                                           [else (hash-ref outports key)])))
  (define-get-sock-name/tcp listener/getsockname addresses inports)
  
  ;; take a char** and an int* and write in the actual host address used for the listener
  ;; (NOT the connected input/output ports)
  (define/contract (listener/gethostused conn local-addr* local-port*)
    (VortexConnection*? cpointer? cpointer? . -> . integer?)
    (handle-neterr
     (wk ([key conn])
         (let-values ([(locala localp remotea remotep) (addresses (hash-ref listeners key) #t)])
           (ptr-set! local-addr* _string locala)
           (ptr-set! local-port* _int localp)
           1))))
  
  ;; transfer all of the above closures to the vortex side to be opaquely invoked
  (vortex-connection-set-listener-mode-closures this-connection
                                                listener/listen listener/accept
                                                listener/read listener/write listener/close
                                                listener/getsockname listener/gethostused
                                                listener/wait/read listener/wait/write))