#lang racket

(require racket/tcp 
         (except-in ffi/unsafe ->)
         "../vtx/module.rkt")
(provide (all-defined-out))

;;; -----------
;;; CLIENT MODE
;;; -----------

; given a new connection, turn into client mode by registering function pointers
; to closures that close over the tcp listener.
(define/contract (rkt:vortex-connection-set-client-mode-closures this-connection)
  (VortexConnection*? . -> . void)
  (define inports (make-hash))
  (define outports (make-hash))
  
  ;; given a new connection, a host and a port, connect
  ;; and bind the resulting input and output ports
  ;; and the socket addresses
  ;; return 1 when done or -1 on network error
  (define/contract (connect/tcp conn host port)
    (VortexConnection*? string? string? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let-values ([(in out) (tcp-connect host (string->number port))])
        (hash-set! inports (vortex-connection-get-id conn) in)
        (hash-set! outports (vortex-connection-get-id conn) out))
      (vortex-connection-ref conn "connect/tcp")
      (vortex-connection-set-socket conn 1 #f #f)
      1))
  
  ;; read the designated amount from the designated connection's input port
  ;; return 1 when done, 0 if encountered only #<eof> or -1 on network error
  (define/contract (read/tcp conn buffer buffer-len)
    (VortexConnection*? string? integer?  . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let ([res (read-string! buffer (hash-ref inports conn) 0 buffer-len)])
        (if (eof-object? res) ; returns `#<eof>' or some number
            0
            res))))
  
  ;; write the designated amount onto the designated connection's output port
  ;; return the amount written, or -1 on network error
  (define/contract (write/tcp conn buffer buffer-len)
    (VortexConnection*? string? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let* ([key (vortex-connection-get-id conn)]
             [amt (write-string buffer (hash-ref outports key) 0 buffer-len)])
        (flush-output (hash-ref outports key))
        amt)))
  
  ;; close the connection's input and output ports, remove from the map,
  ;; unref the connection and return 1 when done, or -1 on network error
  (define/contract (close/tcp conn)
    (VortexConnection*? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let ([key (vortex-connection-get-id conn)])
        (close-input-port (hash-ref inports key))
        (close-output-port (hash-ref outports key))
        (hash-remove inports key)
        (hash-remove outports key)
        (vortex-connection-unref conn "close/tcp")
        1)))
  
  ;; perform a wait-to-read operation on the connection input buffer
  ;; if `timeout' is non-negative, allow timing out
  ;; return 1 when done, or -1 on network error
  (define/contract (wait/read/tcp conn timeout)
    (VortexConnection*? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let ([key (vortex-connection-get-id conn)])
        (if (> timeout -1)
            (let ([res (sync/timeout timeout (hash-ref inports key))]) ; wait with a timeout
              (if (false? res)
                  -1
                  1))
            (let ([res (sync (hash-ref inports key))]) ; wait without a timeout
              1)))))
  
  ;; perform a wait-to-write operation on the connection's output buffer
  ;; if `timeout' is non-negative, allow timing out
  ;; return 1 when done, or -1 on network error
  (define/contract (wait/write/tcp conn timeout)
    (VortexConnection*? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let ([key (vortex-connection-get-id conn)])
        (if (> timeout -1)
            (let ([res (sync/timeout timeout (hash-ref outports key))]) ; wait with a timeout
              (if (false? res)
                  -1
                  1))
            (let ([res (sync (hash-ref outports key))]) ; wait without a timeout
              1)))))
  
  ;; take four char**, then write the connection's local and remote
  ;; addresses and ports into those char**
  ;; return 1 if successful, -1 if not
  (define/contract (getsockname/tcp conn local-addr* local-port* remote-addr* remote-port*)
    (VortexConnection*? cpointer? cpointer? cpointer? cpointer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let ([key (vortex-connection-get-id conn)])
        (let-values ([(locala localp remotea remotep) (tcp-addresses (hash-ref inports key) #t)])
          (ptr-set! local-addr* _string locala)
          (ptr-set! local-port* _string (number->string localp))
          (ptr-set! remote-addr* _string remotea)
          (ptr-set! remote-port* _string (number->string remotep))
          1))))
  
  ;; transfer all of the above closures to the vortex side to be opaquely invoked
  (vortex-connection-set-client-mode-closures this-connection
                                              connect/tcp read/tcp write/tcp 
                                              close/tcp getsockname/tcp wait/read/tcp wait/write/tcp))
;;; -------------
;;; LISTENER MODE
;;; -------------

; given a new connection, turn it into listener mode by registering function pointers
; to closures that close over the tcp listener.
(define/contract (rkt:vortex-connection-set-listener-mode-closures this-connection)
  (VortexConnection*? . -> . void)
  (define inports (make-hash))
  (define outports (make-hash))
  (define listeners (make-hash))
  
  ;; listen on the given host/port (both strings) and set the
  ;; master listener for this connection
  ;; return 1 when done, or -1 if network error
  (define/contract (listen/tcp conn host port)
    (VortexConnection*? string? string? . -> . integer?)
    (printf "in listen/tcp~n")
    (with-handlers ([exn:fail:network? -1])
      (let ([key (vortex-connection-get-id conn)])
        (if (eq? host #f)
            (hash-set! listeners key (tcp-listen (string->number port) 10000 #t))
            (hash-set! listeners key (tcp-listen (string->number port) 10000 host)))
        (vortex-connection-ref conn "listen/tcp")
        1)))
  
  ;; accept a connection request with the master listener, and the new child listener
  ;; to bind together with the connection's input and output ports
  ;; return 1 when done, or -1 if network error
  (define/contract (accept/tcp masterconn childconn)
    (VortexConnection*? VortexConnection*? . -> . integer?)
    (printf "in accept/tcp~n")
    (with-handlers ([exn:fail:network? -1])
      (let ([masterkey (vortex-connection-get-id masterconn)]
            [childkey (vortex-connection-get-id childconn)])
        (let-values ([(in out) (tcp-accept (hash-ref listeners masterkey))])
          (hash-set! inports childkey in)
          (hash-set! outports childkey out)
          (vortex-connection-ref childconn "accept/tcp")
          (printf "calling set-socket~n")
          (vortex-connection-set-socket childconn 1 "fake" "values")
          (printf "leaving accept/tcp~n")
          1))))
  
  ;; read the specified number of bytes in the tcp port to the input buffer.
  ; return the amount that was read, or 0 if the process only discovered an #<eof>
  (define/contract (read/tcp conn buffer buffer-len)
    (VortexConnection*? string? integer?  . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let* ([key (vortex-connection-get-id conn)]
             [res (read-string! buffer (hash-ref inports key) 0 buffer-len)])
        (if (eof-object? res) ; returns `#<eof>' or some number
            0 
            res))))
  
  ;; write the specified number of bytes in the string to the connection's
  ;; output tcp port.
  ;; return the amount that was written, or -1 if network error
  (define/contract (write/tcp conn buffer buffer-len)
    (VortexConnection*? string? integer? . -> . integer?)
    (printf "in listener write/tcp~n")
    (with-handlers ([exn:fail:network? -1])
      (let* ([key (vortex-connection-get-id conn)]
             [amt (write-string buffer (hash-ref outports key) 0 buffer-len)])
        (printf "wrote ~s~n" buffer)
        (flush-output (hash-ref outports conn))
        amt)))
  
  ;; close the connection's input ports, remove them from the hash table
  ;; and unref the connection so that we don't keep dead connections lying around
  ;; return 1 when done, or -1 if network error
  (define/contract (close/tcp conn)
    (VortexConnection*? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let ([key (vortex-connection-get-id conn)])
        (close-input-port (hash-ref inports key))
        (close-output-port (hash-ref outports key))
        (hash-remove inports key)
        (hash-remove outports key)
        (vortex-connection-unref conn "close/tcp")
        1)))
  
  ;; perform a wait-read operation on the connection's input port
  ;; if `timeout' is non-negative, allow timing out
  ;; return 1 if successful, or -1 on network error or timeout
  (define/contract (wait/read/tcp conn timeout)
    (VortexConnection*? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let* ([key (vortex-connection-get-id conn)]
             [sync-on (cond
                        [(eq? (vortex-connection-get-role conn) 'master-listener) 
                         (hash-ref listeners key)]
                        [else (hash-ref inports key)])])
        (cond
          [(> timeout -1)
           (let ([res (sync/timeout timeout sync-on)]) ; wait with a timeout
             (if (false? res)
                 -1
                 1))]
          [else
           (let ([res (sync sync-on)]) ; wait without a timeout
             1)]))))
  
  ;; perform a wait-to-write operation on the connection's output port
  ;; if `timeout' is non-negative, allow timing out
  ;; return 1 if successful, or -1 on network error or timeout
  (define/contract (wait/write/tcp conn timeout)
    (VortexConnection*? integer? . -> . integer?)
    (printf "in listener wait/write/tcp~n")
    (with-handlers ([exn:fail:network? -1])
      (let* ([key (vortex-connection-get-id conn)]
             [sync-on (cond
                        [(eq? (vortex-connection-get-role conn) 'master-listener) 
                         (hash-ref listeners key)]
                        [else (hash-ref outports key)])])
        (cond
          [(> timeout -1)
           (let ([res (sync/timeout timeout sync-on)]) ; wait with a timeout
             (if (false? res)
                 -1
                 1))]
          [else 
           (let ([res (sync sync-on)]) ; wait without a timeout
             1)]))))
  
  ;; take four char**, then write the connection's local and remote
  ;; addresses and ports into those char**
  ;; return 1 if successful, -1 if not
  (define/contract (getsockname/tcp conn local-addr* local-port* remote-addr* remote-port*)
    (VortexConnection*? cpointer? cpointer? cpointer? cpointer? . -> . integer?)
    (printf "in listener getsockname/tcp~n")
    (with-handlers ([exn:fail:network? -1])
      (let ([key (vortex-connection-get-id conn)])
        (let-values ([(locala localp remotea remotep) (tcp-addresses (hash-ref inports key) #t)])
          (ptr-set! local-addr* _string locala)
          (ptr-set! local-port* _string (number->string localp))
          (ptr-set! remote-addr* _string remotea)
          (ptr-set! remote-port* _string (number->string remotep))
          1))))
  
  ;; take a char** and an int* and write in the actual host address used for the listener
  ;; (NOT the connected input/output ports)
  (define/contract (gethostused/tcp conn local-addr* local-port*)
    (VortexConnection*? cpointer? cpointer? . -> . integer?)
    (printf "in listener gethostused/tcp~n")
    (with-handlers ([exn:fail:network? -1])
      (let ([key (vortex-connection-get-id conn)])
        (let-values ([(locala localp remotea remotep) (tcp-addresses (hash-ref listeners key) #t)])
          (ptr-set! local-addr* _string locala)
          (ptr-set! local-port* _int localp)
          1))))
  
  ;; transfer all of the above closures to the vortex side to be opaquely invoked
  (vortex-connection-set-listener-mode-closures this-connection
                                                listen/tcp accept/tcp read/tcp write/tcp 
                                                close/tcp getsockname/tcp gethostused/tcp 
                                                wait/read/tcp wait/write/tcp))