#lang racket

(require "../util.rkt"
         "../../../../bindings/theora/theora.rkt")
(provide make-theora-decoder)

(define state/c (vector/c (or/c #f bytes?) (or/c #f bytes?) (or/c #f bytes?)))

(define/contract (make-theora-decoder signaller [localstate (vector #f #f #f)])
  ([thread?] [state/c] . ->* . (-> void))
  (define d (theoradec-new))
  (define is-signaller? (make-thread-id-verifier signaller))
  (vector-map (λ (b) (when (bytes? b) (handle-theora-buffer! d localstate b))) localstate)
  (λ ()
    (printf "it's safe to run encoder now~n")
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? bytes? data) (handle-theora-buffer! d localstate data)
                         (loop)]
        [(? die? sig) (theoradec-delete d)
                      (reply/state-report signaller localstate)]))))

;; the return value here is not yet meaningful.
(define/contract (handle-theora-buffer! d localstate data)
  (theoradec? state/c bytes? . -> . boolean?)
  (match* ((packet-type data) (theoradec-ready-for-data? d))
    [('data #t) (theoradec-data-in d data)]
    [('header #f) (store-header! localstate data)
                  (theoradec-header-in d data)]
    [((or 'data 'empty) #f) #f]
    [((or 'header 'empty) #t) #t]))

(define/contract (packet-type data)
  (bytes? . ->  . (or/c 'empty 'header 'data))
  (cond [(zero? (bytes-length data)) 'empty]
        [(= #x80 (bytes-ref data 0)) 'header]
        [(= #x81 (bytes-ref data 0)) 'header]
        [(= #x82 (bytes-ref data 0)) 'header]
        [else 'data]))

(define/contract (store-header! localstate data)
  (state/c bytes? . -> . void)
  (match (bytes-ref data 0)
    [#x80 (vector-set! localstate 0 data)]
    [#x81 (vector-set! localstate 1 data)]
    [#x82 (vector-set! localstate 2 data)]))