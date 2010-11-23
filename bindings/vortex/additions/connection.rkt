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
(define/contract (rkt:vortex-connection-set-client-mode-closures conn)
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
        (hash-set! inports conn in)
        (hash-set! outports conn out))
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
      (let ([amt (write-string buffer (hash-ref outports conn) 0 buffer-len)])
        (flush-output (hash-ref outports conn))
        amt)))
  
  ;; close the connection's input and output ports, remove from the map,
  ;; unref the connection and return 1 when done, or -1 on network error
  (define/contract (close/tcp conn)
    (VortexConnection*? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (close-input-port (hash-ref inports conn))
      (close-output-port (hash-ref outports conn))
      (hash-remove inports conn)
      (hash-remove outports conn)
      (vortex-connection-unref conn "close/tcp")
      1))
  
  ;; perform a wait-to-read operation on the connection input buffer
  ;; if `timeout' is non-negative, allow timing out
  ;; return 1 when done, or -1 on network error
  (define/contract (wait/read/tcp conn timeout)
    (VortexConnection*? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (if (> timeout -1)
          (let ([res (sync/timeout timeout (hash-ref inports conn))]) ; wait with a timeout
            (if (false? res)
                -1
                1))
          (let ([res (sync (hash-ref inports conn))]) ; wait without a timeout
            1))))
  
  ;; perform a wait-to-write operation on the connection's output buffer
  ;; if `timeout' is non-negative, allow timing out
  ;; return 1 when done, or -1 on network error
  (define/contract (wait/write/tcp conn timeout)
    (VortexConnection*? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (if (> timeout -1)
          (let ([res (sync/timeout timeout (hash-ref outports conn))]) ; wait with a timeout
            (if (false? res)
                -1
                1))
          (let ([res (sync (hash-ref outports conn))]) ; wait without a timeout
            1))))
    
  ;; take four char**, then write the connection's local and remote
  ;; addresses and ports into those char**
  ;; return 1 if successful, -1 if not
  (define/contract (getsockname/tcp conn local-addr* local-port* remote-addr* remote-port*)
    (VortexConnection*? cpointer? cpointer? cpointer? cpointer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let-values ([(locala localp remotea remotep) (tcp-addresses (hash-ref inports conn) #t)])
        (ptr-set! local-addr* _string locala)
        (ptr-set! local-port* _string (number->string localp))
        (ptr-set! remote-addr* _string remotea)
        (ptr-set! remote-port* _string (number->string remotep))
        1)))
  
  ;; transfer all of the above closures to the vortex side to be opaquely invoked
  (vortex-connection-set-client-mode-closures conn
                                              connect/tcp read/tcp write/tcp 
                                              close/tcp getsockname/tcp wait/read/tcp wait/write/tcp))
;;; -------------
;;; LISTENER MODE
;;; -------------

; given a new connection, turn it into listener mode by registering function pointers
; to closures that close over the tcp listener.
(define/contract (rkt:vortex-connection-set-listener-mode-closures conn)
  (VortexConnection*? . -> . void)
  (define inports (make-hash))
  (define outports (make-hash))
  (define listeners (make-hash))
 
  ;; listen on the given host/port (both strings) and set the
  ;; master listener for this connection
  ;; return 1 when done, or -1 if network error
  (define/contract (listen/tcp conn host port)
    (VortexConnection*? string? string? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (if (eq? host #f)
          (hash-set! listeners conn (tcp-listen (string->number port) 10000 #t))
          (hash-set! listeners conn (tcp-listen (string->number port) 10000 host)))
      (vortex-connection-ref conn "listen/tcp")
      1))
  
  ;; accept a connection request with the master listener, and the new child listener
  ;; to bind together with the connection's input and output ports
  ;; return 1 when done, or -1 if network error
  (define/contract (accept/tcp masterconn childconn)
    (VortexConnection*? VortexConnection*? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let-values ([(in out) (tcp-accept (hash-ref listeners masterconn))])
        (hash-set! inports childconn in)
        (hash-set! outports childconn out)
        (vortex-connection-ref childconn "accept/tcp")
        (vortex-connection-set-socket conn 1 #f #f)
        1)))
  
  ;; read the specified number of bytes in the tcp port to the input buffer.
  ; return the amount that was read, or 0 if the process only discovered an #<eof>
  (define/contract (read/tcp conn buffer buffer-len)
    (VortexConnection*? string? integer?  . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let ([res (read-string! buffer (hash-ref inports conn) 0 buffer-len)])
        (if (eof-object? res) ; returns `#<eof>' or some number
            0 
            res))))
  
  ;; write the specified number of bytes in the string to the connection's
  ;; output tcp port.
  ;; return the amount that was written, or -1 if network error
  (define/contract (write/tcp conn buffer buffer-len)
    (VortexConnection*? string? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let ([amt (write-string buffer (hash-ref outports conn) 0 buffer-len)])
        (flush-output outp)
        amt)))
  
  ;; close the connection's input ports, remove them from the hash table
  ;; and unref the connection so that we don't keep dead connections lying around
  ;; return 1 when done, or -1 if network error
  (define/contract (close/tcp conn)
    (VortexConnection*? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (close-input-port (hash-ref inports conn))
      (close-output-port (hash-ref outports conn))
      (hash-remove inports conn)
      (hash-remove outports conn)
      (vortex-connection-unref conn "close/tcp")
      1))
  
  ;; perform a wait-read operation on the connection's input port
  ;; if `timeout' is non-negative, allow timing out
  ;; return 1 if successful, or -1 on network error or timeout
  (define/contract (wait/read/tcp conn timeout)
    (VortexConnection*? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (if (> timeout -1)
          (let ([res (sync/timeout timeout (hash-ref inports conn))]) ; wait with a timeeout
            (if (false? res)
                -1
                1))
          (let ([res (sync (hash-ref inports conn))]) ; wait without a timeout
            1))))
  
  ;; perform a wait-to-write operation on the connection's output port
  ;; if `timeout' is non-negative, allow timing out
  ;; return 1 if successful, or -1 on network error or timeout
  (define/contract (wait/write/tcp conn timeout)
    (VortexConnection*? integer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (if (> timeout -1)
          (let ([res (sync/timeout timeout (hash-ref outports conn))]) ; wait with a timeout
            (if (false? res)
                -1
                1))
          (let ([res (sync (hash-ref outports conn))]) ; wait without a timeout
            1))))
  
  ;; take four char**, then write the connection's local and remote
  ;; addresses and ports into those char**
  ;; return 1 if successful, -1 if not
  (define/contract (getsockname/tcp conn local-addr* local-port* remote-addr* remote-port*)
    (VortexConnection*? cpointer? cpointer? cpointer? cpointer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let-values ([(locala localp remotea remotep) (tcp-addresses (hash-ref inports conn) #t)])
        (ptr-set! local-addr* _string locala)
        (ptr-set! local-port* _string (number->string localp))
        (ptr-set! remote-addr* _string remotea)
        (ptr-set! remote-port* _string (number->string remotep))
        1)))
  
  ;; take a char** and an int* and write in the actual host address used for the listener
  ;; (NOT the connected input/output ports)
  (define/contract (gethostused/tcp conn local-addr* local-port*)
    (VortexConnection*? cpointer? cpointer? . -> . integer?)
    (with-handlers ([exn:fail:network? -1])
      (let-values ([(locala localp remotea remotep) (tcp-addresses (hash-ref listeners conn) #t)])
        (ptr-set! local-addr* _string locala)
        (ptr-set! local-port* _int localp)
        1)))
  
  ;; transfer all of the above closures to the vortex side to be opaquely invoked
  (vortex-connection-set-listener-mode-closures conn
                                                listen/tcp accept/tcp read/tcp write/tcp 
                                                close/tcp getsockname/tcp gethostused/tcp 
                                                wait/read/tcp wait/write/tcp))