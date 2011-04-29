#lang typed/racket

(provide (all-defined-out))

(define & bitwise-and)
(define ~ bitwise-not)
(define ^ bitwise-xor)
(define $ bitwise-ior)
(define << arithmetic-shift)
(: >> (Integer Integer -> Integer))
(define (>> n m) (arithmetic-shift n (- m)))

;; turn bit m (from 0) on in integer n
(: biton (Integer Integer -> Integer))
(define (biton n m) ($ m (<< 1 n)))

;; turn bit m (from 0) off in integer m
(: bitoff (Integer Integer -> Integer))
(define (bitoff n m) (& m (~ (<< 1 n))))

(: make-timestamp (-> Natural))
(define (make-timestamp) 0)

;; the biggest unsigned int that can fit into n bits
(: biggest (Natural -> Natural))
(define biggest
  (let: ([h : (HashTable Natural Natural) (make-hash '())])
    (λ (n)
      (hash-ref h n (λ ()
                      (define v (abs (sub1 (expt 2 n))))
                      (hash-set! h n v)
                      v)))))

;; commonly used sizes
(define top32 (biggest 32))
(define top31 (biggest 31))
(define top29 (biggest 29))

;; for an integer i of size n bits increase by 1 but only if i+1 <= maxsize(n)
(: wrappedSucc (Natural Natural -> Natural))
(define (wrappedSucc n bits)
  (if (> (add1 n) (biggest bits))
      0
      (add1 n)))

;; calls parameters of f in reverse order
(: fromR (All (a b c) (a b -> c) -> (b a -> c)))
(define (fromR f) (λ: ([e2 : b] [e1 : a]) (f e1 e2)))

;; session managers should catch exn:fail when they
;; try to parse control packets
(: raise-parse-error (String -> Nothing))
(define (raise-parse-error str) (raise (make-exn:fail str (current-continuation-marks))))

;; make32: make 32 bits of byte data from an int
(: make32 (Integer -> Bytes))
(define (make32 i)
  (cond [(>= i 0) (integer->integer-bytes i 4 #f #t)]
        [else (integer->integer-bytes i 4 #t #t)]))

;; bytes/32bit: bytes-appends the byte representation of
;; all of the naturals supplied
;; after converting to unsigned 4-byte numbers
;; (: bytes/32bit (Natural * -> Bytes))
(define-syntax-rule (bytes/32bit n ...) (bytes-append (make32 n) ...))

;; this function's chief purpose is to trick the compiler
;; into typechecking successfully, but it also yells at us
;; at runtime if the compiler's paranoia was valid! amazing!
(: natcheck (Number -> Natural))
(define (natcheck v)
  (if (exact-nonnegative-integer? v)
      v
      (raise-parse-error "Found negative number where natural should have been")))

;; take32: take 32 bits of data starting at `s' and 
;; convert data to natural number. raise exn if not natural number
(: take32 (Bytes Natural -> Natural))
(define (take32 b s) (natcheck (integer-bytes->integer b #f #t s (+ 4 s))))