#lang scheme
(require "chipmunk-ffi.rkt"
         (except-in scheme/foreign
                    ->)
         (prefix-in c: scheme/contract))
(unsafe!)


;; Do init
(cpInitChipmunk)
;; Be determinitic
(cpResetShapeIdCounter)

(define collision-type/c
  exact-nonnegative-integer?)
(define gen-collision-type
  (local [(define i 1)]
    (lambda ()
      (begin0 i
              (set! i (add1 i))))))

#;(define (wrap:CollisionPairFunc func)
  (lambda (sh1 sh2 contacts-p num-contacts normal-coef _data)
    (define contacts (cblock->vector contacts-p _cpContact num-contacts))
    (func sh1 sh2 contacts normal-coef)))

#;(define (wrap:cpSpaceAddCollisionPairFunc space typ1 typ2 func)
  (cpSpaceAddCollisionPairFunc 
   space typ1 typ2
   (wrap:CollisionPairFunc func)
   #f))

#;(define (wrap:cpSpaceSetDefaultCollisionPairFunc space func)
  (cpSpaceSetDefaultCollisionPairFunc
   space
   (wrap:CollisionPairFunc func)
   #f))

(define (cpvadd* . vs)
  (for/fold ([v1 cpvzero])
    ([v2 (in-list vs)])
    (cpvadd v1 v2)))

(provide/contract
 [gen-collision-type (-> (and/c collision-type/c positive?))]

 [rename cpvadd* cpvadd
         (->* () () #:rest (listof cpVect?)
              cpVect?)])

(provide (except-out (all-from-out "chipmunk-ffi.rkt")
                     cpvadd))