#lang typed/racket

(require/typed "util.rkt"
               [command/killswitch (Thread Thread -> Void)]
               [reply/state-report (Thread Any -> Void)]
               [die? (Any -> Boolean)]
               [no-message? (Any -> Boolean)]
               [make-thread-id-verifier (Thread -> (Any -> Boolean))])
(provide (all-defined-out))



(: rev-params (All (a b c) (a b -> c) -> (b a -> c)))
(define (rev-params f) (λ (b a) (f a b)))