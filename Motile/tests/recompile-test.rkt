#lang racket/base

(require
 rackunit
 rackunit/text-ui
 (only-in "../compile/recompile.rkt" motile/recompile)

 racket/pretty

 (only-in "../persistent/environ.rkt" environ/ref)

 (only-in "../compile/compile.rkt"   motile/compile)
 (only-in "../generate/baseline.rkt" motile/call)
 (only-in "../compile/serialize.rkt" motile/serialize)
 (only-in "../generate/baseline.rkt" motile/decompile)
 (only-in "../baseline.rkt" BASELINE ENVIRON/TEST))

;; Convenient helper routines.
(define (decompile x) (motile/decompile (motile/compile x)))
(define (recompile/start x) (motile/call (motile/recompile x) ENVIRON/TEST))

(define-test-suite environ
  (test-case
   "environ/cons and environ/ref #1"
   (check-equal?
    (let ((code
           (decompile
            '(let ((silly 1951))
               (environ/ref (environ/cons environ/null silly) silly #f)))))
      (recompile/start code))
    1951))
  
  (test-case
   "environ/cons and environ/ref #2"
   (check-equal?
    (let ((code
           (decompile
            '(let ((E (let ((a 100) (b 200) (c 700)) (environ/cons environ/null a b c)))
                   (x 0))
               (list (environ/ref E c #f) (environ/ref E a #f) (environ/ref E b #f) (environ/ref E x "not there"))))))
      (recompile/start code))
    '(700 100 200 "not there")))

  (test-case
   "environ/remove"
   (check-equal?
    (let ((code
           (decompile
            '(let* ((E (let ((a 100) (b 200) (c 700)) (environ/cons environ/null a b c)))
                    (F (environ/remove E a)))
               (list (environ/ref F c #f) (environ/ref F a #f) (environ/ref F b #f))))))
      (recompile/start code))
    '(700 #f 200)))

  (test-case
   "environ/reflect"
   (check-eqv?
    (let ((code
           (decompile
            '(let ((E (let ((a 100) (b 200) (c 700) (plus +)) (environ/cons environ/null plus a b c))))
               (environ/reflect E (plus a b c))))))
      (recompile/start code))
    1000))
  
  (test-case
   "environ/capture and environ/merge"
   (check-eqv?
    (let ((code
           (decompile
            '(let* ((E (let ((a 100) (b 200) (c 700)) (environ/cons environ/null a b c)))
                    (F (environ/merge (environ/capture) E))) ; ENVIRON/TEST + a/100, b/200, and c/700.
               (+ (environ/ref F c 19) (environ/ref F a 17) (environ/ref F b 18))))))
      (recompile/start code))
    1000))

  (test-case
   "environ/reflect"
   (check-eqv?
    (let ((code
           (decompile
            '(let ((E (let ((a 100) (b 200) (c 700) (+ +)) (environ/cons environ/null + a b c))))
               (environ/reflect E (+ a b c))))))
      (recompile/start code))
    1000)))

(define-test-suite record
  (test-case
   "record/new"
   (check-equal?
    (let ((code (decompile '(let () (record/new sample a 1 b 2 c "silly" d 'foobar)))))
      (recompile/start code))
    #(<record> sample #(a b c d) 1 2 "silly" foobar)))
  
  (test-case
   "record/cons"
   (check-equal?
    (let ((code
           (decompile
            '(let ((r (record/new sample a 1 b 2 c "silly" d 'foobar)))
               (record/cons r d 1 c 2 b 'foobar a "nuts")))))
      (recompile/start code))
    #(<record> sample #(a b c d) "nuts" foobar 2 1)))
  
  (test-case
   "record/ref without failure option"
   (check-equal?
    (let ((code
           (decompile
            '(let ((r (record/new sample a 1 b 2 c "silly" d 'foobar)))
               (list (record/ref r d) (record/ref r c) (record/ref r b) (record/ref r a))))))
      (recompile/start code))
    '(foobar "silly" 2 1)))

  (test-case
   "record/ref with failure option"
   (check-equal?
    (let ((code
           (decompile
            '(let ((r (record/new sample a 1 b 2 c "silly" d 'foobar))
                   (glue (lambda (x y) (string-append x y))))
               (list (record/ref r d) (record/ref r X "no such field") (record/ref r b) (record/ref r XX (glue "U" "nix")))))))
      (recompile/start code))
    '(foobar "no such field" 2 "Unix")))

  (test-case
   "record/ref with failure option"
   (check-exn
    exn:fail?
    (lambda ()
      (let ((code
             (decompile
              '(let ((r (record/new sample a 1 b 2 c "silly" d 'foobar)))
                 (list (record/ref r d)
                       (record/ref r X) ; This field reference will throw an error.
                       (record/ref r b)
                       (record/ref r a))))))
        (recompile/start code))))))

(define-test-suite letrec*
  (test-case
   "Simple letrec* with two definitions"
   (check-equal?
    (let ((code (decompile '(letrec* ((a 11) (b (+ a 13))) (* a b)))))
      (recompile/start code))

    264)) ; Expected.

  (test-case
   "Letrec* with one function definition"
   (check-equal?
    (let ((code
           (decompile 
            '(letrec* ((f (lambda (n) (if (= n 1) 1 (* n (f (sub1 n))))))) ; Factorial.
               (f 5)))))
      (recompile/start code))

    120)) ; Expected.

  (test-case
   "Recursive definition of two functions"
   (check-equal? 
    (let ((code
           (decompile
            '(letrec* ; Mutually recursive functions.
                 ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                  (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
               (list (even? 12) (even? 3) (odd? 17) (odd? 8))))))
      (recompile/start code))

    '(#t #f #t #f))) ; Expected.

  (test-case
   "Mutually recursive function definitions with multiple (> 1) closed variables per function"
   (check-equal? 
    (let ((code
           (decompile
            '(letrec*
                 ((even? (lambda (n) (if (= n zero) #t (odd? (- n one)))))
                  (odd?  (lambda (n) (if (= n zero) #f (even? (- n one)))))
                  (zero 0)
                  (one 1))
               (list (even? 12) (even? 3) (odd? 17) (odd? 8))))))
      (recompile/start code))

    '(#t #f #t #f))) ; Expected.

  (test-case
   "Recursive definition of three functions"
   (check-equal? 
    (let ((code
           (decompile
            '(letrec*
                 ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                  (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                  (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n)))))))
               (list (even? 12) (even? 3) (odd? 17) (odd? 8) (factorial 5))))))
      (recompile/start code))

    '(#t #f #t #f 120))) ; Expected.  

  (test-case
   "Recursive definition of three functions with inline calls in later bindings"
   (check-equal?
    (let ((code
           (decompile
            '(letrec*
                 ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                  (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                  (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n))))))
                  (a (even? 12))
                  (b (even? 3))
                  (c (odd? 17))
                  (d (odd? 8))
                  (e (factorial 5)))
               (list a b c d e)))))
      (recompile/start code))

    '(#t #f #t #f 120))) ; Expected.


  (test-case
   "Recursive definitions in three bindings + dependent definitions in two"
   (check-equal?  
    (let ((code
           (decompile
            '(letrec* ; Mutually recursive functions.
                 ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                  (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                  (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n))))))
                  (a (add1 (factorial 4)))
                  (b (+ a 13)))
               (list (even? 12) (even? 3) (odd? a) (odd? 8) (factorial 5) b)))))
      (recompile/start code))

    '(#t #f #t #f 120 38))) ; Expected.

  (test-case
   "Self-recursive definition"
   (check-true
    (let ((code
           (decompile
            '(letrec* ((foo (lambda (x) (eq? x foo))))
               (foo foo)))))
      (recompile/start code))))
  
  (test-case
   "Letrec* with define in the body"
   (check-equal?
    (let ((code
           (decompile
            '(letrec* ; Mutually recursive functions.
                 ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                  (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                  (factorial (lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n)))))))
               ; Definitions in the letrec* body.
               (define a 11)
               (define b (lambda () (+ a 13)))
               (list (even? 12) (even? 3) (odd? a) (odd? 8) (factorial 5) (b))))))
      (recompile/start code))

    '(#t #f #t #f 120 24))) ; Expected.


  (test-case 
   "Takeuchi functions"
   (check-equal?
    (let ((code
           (decompile
            '(letrec* ((tak
                        (lambda (x y z)
                          (if (not (< y x))
                              z
                              (tak (tak (- x 1) y z)
                                   (tak (- y 1) z x)
                                   (tak (- z 1) x y))))))
               (tak 18 12 6)))))
      (recompile/start code))

    7)) ; Expected.  
  
  )






