#lang racket

(require racket/async-channel)

(provide make-bufferpool-handler
         request-buffer
         return?
         fulfill?
         request?)

;(define-type Fulfillment (List Bytes 'FulfillRequest))
;(define-type Request 'RequestBuffer)
;(define-predicate fulfill? Fulfillment)
;(define-predicate request? Request)

;(define-type BufferReturn (-> Void))
;(define-type Return (List Bytes 'Return))
;(define-predicate return? Return)

(define (return? a)
  (and (list? a)
       (= (length a) 2)
       (bytes? (car a))
       (equal? 'Return (cadr a))))

(define (fulfill? a)
  (and (list? a)
       (= (length a) 2)
       (bytes? (car a))
       (equal? 'FulfillRequest (cadr a))))

(define (request? a)
  (equal? 'RequestBuffer a))

;(: make-bufferpool-handler (Natural Natural -> Thread))
(define (make-bufferpool-handler count size)
  (define requestch (make-async-channel))
  (define fulfillch (make-async-channel))
  (define handler (thread
                   (λ ()
                     (define pool (for/list ([i (in-range count)]) (make-bytes size)))
                     (let loop ([pool pool])
                       (let ([m (async-channel-get requestch)])
                         (match m
                           [(? return? msg) (loop (cons (car msg) pool))]
                           [(? request? msg)
                            (cond [(empty? pool) (thread-rewind-receive (list msg))
                                                 (loop pool)]
                                  [else (async-channel-put fulfillch (list (car pool) 'FulfillRequest))
                                        (loop (cdr pool))])]
                           [m (printf "Buffer pool handler dropping misplaced message: ~a~n" m)
                              (loop pool)]))))))
  (values handler requestch fulfillch))

;(: request-buffer (Thread -> (values Bytes BufferReturn)))
(define (request-buffer requestch fulfillch)
  (async-channel-put requestch 'RequestBuffer)
  (let loop ()
    (let ([m (async-channel-get fulfillch)])
      (match m
        [(? fulfill? msg) (values (car msg) (make-return-thunk (car msg) requestch))]
        [other (printf "Buffer requester dropping misplaced message: ~a~n" other)]))))

;(: make-return-thunk (Bytes Thread -> BufferReturn))
(define (make-return-thunk buffer requestch)
  (λ ()
    (async-channel-put requestch (list buffer 'Return))
    (void)))