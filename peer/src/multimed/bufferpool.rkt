#lang typed/racket

(require "asynch-wrap.rkt")
(provide make-bufferpool-handler)

;; this implementation uses two channels to avoid messing with the pipeline component thread's
;; mailbox (which is effectively a work queue and should not be disturbted).
;; one channel is used by pipeline elements to request buffers and return them to the handler.
;; the other is used by the handler to fulfill buffer requests.
(define-type FulfillmentMessage (List Bytes 'FRQ))
(define-type RequestMessage 'RQB)
(define-predicate fulfill? FulfillmentMessage)
(define-predicate request? RequestMessage)

(define-type BufferReturn (-> Void))
(define-type ReturnMessage (List Bytes 'RTB))
(define-predicate return? ReturnMessage)
(define-predicate request/return? (U RequestMessage ReturnMessage))

(define-type RequestChannel (AsyncChannelof (U RequestMessage ReturnMessage)))
(define-type FulfillChannel (AsyncChannelof FulfillmentMessage))

;; return two values: the handler thread (in case you need to kill it off)
;; and a thunk that lets you request a buffer.
;; this hides the slight messiness of two channels since
;; applications just need to re-run the thunk every time they want a buffer
(: make-bufferpool-handler (Natural Natural -> (values Thread (-> (values Bytes BufferReturn)))))
(define (make-bufferpool-handler count size)
  (define: requestch : RequestChannel (make-async-channel* request/return?))
  (define: fulfillch : FulfillChannel (make-async-channel* fulfill?))
  (define: pool : (Listof Bytes) (for/list ([i (in-range count)]) (make-bytes size)))
  (define handler
    (thread
     (λ ()
       (let loop ([pool pool])
         (let ([msg (async-channel-get* requestch)])
           ;; a buffer is returned. attach it to the pool of tracked buffers and loop
           (cond [(return? msg) (loop (cons (car msg) pool))]
                 [(request? msg)
                  ;; a request is returned. if the pool is empty right now,
                  ;; put the request back into the request channel and get it
                  ;; later when the pool is non-empty
                  (cond [(empty? pool) (async-channel-put* requestch msg)
                                       (loop pool)]
                        ;; the pool isn't empty: fulfill the request and stop keeping track
                        ;; of the buffer until it's returned by the BufferReturn thunk
                        [else (async-channel-put* fulfillch (list (car pool) 'FRQ))
                              (loop (cdr pool))])]))))))
  (values handler (λ () (request-buffer requestch fulfillch))))

;; request-buffer: return the buffer requested, plus a thunk to return the buffer back to the pool
(: request-buffer (RequestChannel FulfillChannel -> (values Bytes BufferReturn)))
(define (request-buffer requestch fulfillch)
  (async-channel-put* requestch 'RQB)
  (let loop ()
    (let ([msg (async-channel-get* fulfillch)])
      (values (car msg) (make-return-thunk (car msg) requestch)))))

(: make-return-thunk (Bytes RequestChannel -> BufferReturn))
(define (make-return-thunk buffer requestch)
  (λ ()
    (async-channel-put* requestch (list buffer 'RTB))
    (void)))