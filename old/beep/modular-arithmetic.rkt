#lang racket

(provide define-modular-arithmetic)

(define-for-syntax (pre/base/post pre base post)
  (string->symbol (string-append pre (number->string (syntax->datum base)) post)))

(define-syntax (define-modular-arithmetic stx)
  (syntax-case stx ()
    [(k base #f)
     (with-syntax ([m+ (datum->syntax #'k (pre/base/post "+/u" #'base ""))]
                   [m- (datum->syntax #'k (pre/base/post "-/u" #'base ""))]
                   [m* (datum->syntax #'k (pre/base/post "*/u" #'base ""))]
                   [m= (datum->syntax #'k (pre/base/post "=/u" #'base ""))]
                   [+1m (datum->syntax #'k (pre/base/post "add1/u" #'base ""))]
                   [-1m (datum->syntax #'k (pre/base/post "sub1/u" #'base ""))]
                   [tp (datum->syntax #'k (pre/base/post "top/u" #'base ""))]
                   [bt (datum->syntax #'k (pre/base/post "bottom/u" #'base ""))]
                   [c/c (datum->syntax #'k (pre/base/post "uint" #'base "/c"))])
       #'(begin
           (define/contract modulus exact-nonnegative-integer? (expt 2 base))
           (define tp (sub1 modulus))
           (define bt 0)
           (define c/c (opt/c (and/c exact-integer? (between/c bt tp))))
           (define/contract (m+ x y)
             (exact-nonnegative-integer? exact-nonnegative-integer? . -> . c/c)
             (let ([s (+ x y)])
               (if (> s tp)
                   (modulo s modulus)
                   s)))
           (define/contract (m- x y)
             (exact-nonnegative-integer? exact-nonnegative-integer? . -> . c/c)
             (let ([d (- x y)])
               (if (< d bt)
                   (modulo d modulus)
                   d)))
           (define/contract (m* x y)
             (exact-nonnegative-integer? exact-nonnegative-integer? . -> . c/c)
             (let ([p (* x y)])
               (if (> p tp)
                   (modulo p modulus)
                   p)))
           (define/contract (m= x y)
             (exact-nonnegative-integer? exact-nonnegative-integer? . -> . boolean?)
             (= (modulo x modulus) (modulo y modulus)))
           (define/contract (+1m x)
             (exact-nonnegative-integer? . -> . c/c)
             (m+ x 1))
           (define/contract (-1m x)
             (exact-nonnegative-integer? . -> . c/c)
             (m- x 1))))]))

#|(define-modular-arithmetic 8 #f)

(+/u8 255 1) ; should be 0
(*/u8 64 4) ; should be 0
(-/u8 255 255) ; should be 0
(-/u8 255 513) ; should be 254
(add1/u8 255) ; should be 0
(sub1/u8 0) ; should be 255|#