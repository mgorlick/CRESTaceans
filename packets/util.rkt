#lang typed/racket

(provide (all-defined-out))

(define & bitwise-and)
(define ~ bitwise-not)
(define ^ bitwise-xor)
(define $ bitwise-ior)
(define << arithmetic-shift)
(: >> (Integer Integer -> Integer))
(define (>> n m)
  (arithmetic-shift n (- m)))

(: 16thbiton (Integer -> Integer))
(define (16thbiton n) ($ n (<< 1 15)))

(: 16thbitoff (Integer -> Integer))
(define (16thbitoff n) (& n (~ (<< 1 15))))

(define i->b
  (case-lambda:
   {([n : Integer] [size : Natural])
    (integer->integer-bytes n size #f #t)}
   {([n : Integer] [size : Natural] [signed? : Boolean])
    (integer->integer-bytes n size signed? #t)}))

(define b->i
  (case-lambda:
   {([b : Bytes])
    (integer-bytes->integer b #f #t)}
   {([b : Bytes] [signed? : Boolean])
    (integer-bytes->integer b signed? #t)}))

;; calls parameters of f in reverse order
(: fromR (All (a b) (a a -> b) -> (a a -> b)))
(define (fromR f) (λ: ([a1 : a] [a2 : a]) (f a2 a1)))

;; bytes/32bit: bytes-appends the byte representation of
;; all of the naturals supplied
;; after converting to unsigned 4-byte numbers
(: bytes/32bit (Natural * -> Bytes))
(define (bytes/32bit . ns) (foldl (fromR bytes-append) #"" (map make32 ns)))

;; session managers should catch exn:fail when they
;; try to parse control packets
(: raise-parse-error (String -> Nothing))
(define (raise-parse-error str)
  (raise (make-exn:fail str (current-continuation-marks))))

;; this function's chief purpose is to trick the compiler
;; into typechecking successfully, but it also yells at us
;; at runtime if the compiler's paranoia was valid! amazing!
(: natcheck (Number -> Natural))
(define (natcheck v)
  (if (exact-nonnegative-integer? v) v
      (raise-parse-error "Found negative number where natural should have been")))

;; make32: make 32 bits of byte data from an unsigned int
(: make32 (Natural -> Bytes))
(define (make32 i)
  (i->b i 4))

;; take32: take 32 bits of data starting at `s' and 
;; convert data to natural number. raise exn if not natural number
(: take32 (Bytes Natural -> Natural))
(define (take32 b s) (natcheck (b->i (subbytes b s (+ 4 s)))))