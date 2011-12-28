#lang racket/base

(require "curl.rkt"
         "promise.rkt"
         "locative.rkt"
         "delivery.rkt"
         "../compile/serialize.rkt")

(provide 
 curl/send
 curl/send/promise
 curl/forward
 set-inter-island-router!)

; x - subject of type test
; type? - type predicate for x
; where - symbol (typially name of function containing the type assertion)
; expectation - string giving expected type
(define-syntax-rule (assert/type x type? where expectation)
  (when (not (type? x))
    (raise-type-error 'where expectation x)))

#|;; Transmit a generic message m to the actor denoted by CURL c.
;; reply - an optional argument, if given, is the CURL to which a reply message is sent.
(define (! c m . reply)
  (curl/send c (vector 'memora c m (if (null? reply) #f (car reply)))))|#

;; Using CURL c as a destination address send an arbitrary message m to the actor denoted by c.
;; If the sender desires a reply then the CURL metadata should contain an item reply/x where
;; reply is the symbol reply and x is the CURL to which the reply should be directed.
;; If CURL c is local then curl/send returns #t on successful delivery and #f otherwise.
;; If CURL c is an off-island reference 
(define (curl/send c m)
  (when (not (curl? c))
    (raise-type-error 'curl/send "<curl>" c))
  
  (let ((x (curl/id c)))
    (cond
      ((symbol? x)   (curl/send/inter c m))         ; An off-island CURL that contains a locative id.
      ((locative? x) (locative/send x (delivery c m)))  ; All on-island CURLs carry a locative in the id slot.
      ((pair? x)     (curl/send/inter c m))         ; An off-island "durable" CURL.
      ; The curl/id is #f. This occurs if CURL c was:
      ;   * Issued by this island, exported to another island, and then used as the target for a message transmission
      ;     back to this island, AND
      ;   * The locative referenced by CURL c has expired in the meantime
      ; Life is a bitch.
      (else #f))))

(define (curl/send/promise c m lifespan)
  (assert/type c curl? 'curl/send "<curl>")
  (assert/type lifespan number? 'curl/send "<curl>")
  (let ([x (curl/id c)]
        [p (promise/new lifespan)])
    (cond
      [(or (symbol? x) 
           (pair? x)) (curl/send/inter/promise c m (promise/to-fulfill p))
                      (promise/result p)]
      [(locative? x)  (locative/send x (vector c m (promise/to-fulfill p)))
                      (promise/result p)]
      [else #f])))

;; only for Chieftains to forward to Actors.
;; do NOT expose to actors themselves!
(define (curl/forward del)
  (assert/type del delivery? 'curl/forward "<delivery>")
  (assert/type (curl/id (delivery/curl-used del)) locative? 'curl/forward "<locative>")
  (locative/send (curl/id (delivery/curl-used del))
                 del))

(define inter-island-router (box #f))
(define (set-inter-island-router! thd) (set-box! inter-island-router thd))

(define (curl/send/inter c m)
  (thread-send (unbox inter-island-router) (delivery->serialized (delivery c m))))
(define (curl/send/inter/promise c m r)
  (thread-send (unbox inter-island-router) (delivery->serialized (delivery c m r))))

(define (delivery->serialized d)
  (vector (curl/island (delivery/curl-used d)) (motile/serialize d)))