(define :client-connection-active:  (->jint 0))
(define :client-connection-closing: (->jint 1))
(define :client-connection-closed:  (->jint 2))

; Returns #t if connection c is open and #f it is closed.
(define (connection-alive? c) (eqv? (get-status c) :client-connection-active:))
