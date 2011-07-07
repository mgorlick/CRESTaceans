#lang racket/base

(require racket/contract)
(provide make-bufferpool-handler)

(struct bprequest (thread))
(struct bpfulfill (buffer))
(struct bpreturn (buffer))

(define return-thunk/c (-> void))
(define request-thunk/c (-> (values bytes? return-thunk/c)))

;; return two values: the handler thread (in case you need to kill it off)
;; and a thunk that lets you request a buffer.
;; this hides the messiness of communication since
;; applications just need to re-run the thunk every time they want a buffer
(define/contract (make-bufferpool-handler count size)
  (exact-nonnegative-integer? exact-nonnegative-integer? . -> . (values thread? request-thunk/c))
  
  (define (bufferpool pool pending)
    (define msg (thread-receive))
    (cond [(bpreturn? msg) ;; a buffer is returned.
           (cond [(null? pending)
                  ;; if a pending request is not found, requeue buffer onto pool
                  (bufferpool (cons (bpreturn-buffer msg) pool) pending)]
                 [else
                  ;; there's a pending request: send the new buffer out
                  (thread-send (bprequest-thread (car pending)) (bpfulfill (bpreturn-buffer msg)))
                  (bufferpool pool (cdr pending))])]
          
          [(bprequest? msg) ;; a request is here.
           (cond [(null? pool)
                  ;; if there's no buffer available to fulfill the request,
                  ;; delay fulfillment until something else arrive
                  (bufferpool pool (cons msg pending))]
                 [else
                  ;; the pool isn't empty: fulfill the request and stop keeping track
                  ;; of the buffer until it's returned by the bpreturn thunk
                  (thread-send (bprequest-thread msg) (bpfulfill (car pool)))
                  (bufferpool (cdr pool) pending)])]))
  
  (define (init)
    (define pool (for/list ([i (in-range count)]) (make-bytes size)))
    (define pending '())
    (bufferpool pool pending))
  
  (define handler (thread init))
  
  (define/contract (make-return buffer)
    (bytes? . -> . return-thunk/c)
    (λ ()
      (thread-send handler (bpreturn buffer) void)))
  
  (define (get-buffer)
      (thread-send handler (bprequest (current-thread)))
      (define the-buffer (thread-receive))
      (vector (bpfulfill-buffer the-buffer) (make-return (bpfulfill-buffer the-buffer))))
  
  (define/contract (request)
    request-thunk/c
    (vector->values (call-in-nested-thread get-buffer)))
  
  (values handler request))