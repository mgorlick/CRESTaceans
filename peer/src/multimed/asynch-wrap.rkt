#lang typed/racket

(require/typed racket/async-channel
               [opaque AsyncChannel async-channel?]
               [make-async-channel (-> AsyncChannel)]
               [async-channel-get (AsyncChannel -> Any)]
               [async-channel-put (AsyncChannel Any -> Void)])

(struct: (a) AsyncChannelof ([ch : AsyncChannel]
                             [pr : (Any -> Boolean : a)]))

(: make-async-channel* (All (a) (Any -> Boolean : a) -> (AsyncChannelof a)))
(define (make-async-channel* pred)
  (AsyncChannelof (make-async-channel) pred))

(: async-channel-get* (All (a) (AsyncChannelof a) -> a))
(define (async-channel-get* as)
  (let ([v (async-channel-get (AsyncChannelof-ch as))])
    (cond [((AsyncChannelof-pr as) v) v]
          [else (raise (make-exn:fail:contract (format "Value ~a not of type ~a" v (AsyncChannelof-pr as))
                                               (current-continuation-marks)))])))

(: async-channel-put* (All (a) (AsyncChannelof a) a -> Void))
(define (async-channel-put* as v)
  (async-channel-put (AsyncChannelof-ch as) v))

(provide make-async-channel*
         async-channel-get*
         async-channel-put*
         AsyncChannelof)