#lang racket

(require "../util.rkt"
         "../../../../bindings/theora/theora.rkt")
(provide make-theora-decoder)

(define/contract (make-theora-decoder signaller)
  ([thread?] . ->* . (-> void))
  (let ([d (theoradec-new)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (printf "it's safe to run encoder now~n")
      (let loop ()
        (match (receive-killswitch/whatever is-signaller?)
          [(? bytes? bytes) (cond [(not (theoradec-ready-for-data d)) (theoradec-header-in d bytes)]
                                  [else (theoradec-data-in d bytes)])
                            (loop)]
          [(? die? sig) (theoradec-delete d)
                        (reply/state-report signaller #f)])))))