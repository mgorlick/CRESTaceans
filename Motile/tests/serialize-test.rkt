#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/pretty
         "../serialize.rkt"
         "../compile.rkt"
         "../baseline.rkt"
         "../persistent/vector.rkt"
         "../persistent/hash.rkt"
         "../persistent/tuple.rkt"
         "../persistent/set.rkt")
(require/expose "../compile.rkt"
                (rtk/RETURN))

(define (should-be id expected outcome)
  (if (equal? expected outcome)
      (display (format "PASS id: ~a expected: ~s outcome: ~s\n" id expected outcome))
      (display (format "FAIL id: ~a expected: ~s outcome: ~s\n" id expected outcome))))

 ; Regression tests.

(define (test/serialize)
  (define (test/serialize/1)
    (let ((e (motile/compile '(lambda () 13))))
      (display "test/serialize/1\n")
      (pretty-display (serialize e))))

  
  (define (test/serialize/2a)
    (let* ((e (motile/compile
              '(let ((a 33))
                 (lambda (x) (+ x a x)))))
           (code (motile/start e ENVIRON/TEST)))
      (display "test/serialize/2a\n")
      (pretty-display (serialize code))))
      
  (define (test/serialize/2b)
    (let* ((e (motile/compile
              '(let ((a (list 11 22 33)))
                 (lambda (x) (append x a x)))))
           (code (motile/start e ENVIRON/TEST)))
      (display "test/serialize/2b\n")
      (pretty-display (serialize code))))

  (define (test/serialize/3)
    (let* ((e (motile/compile
               '(let ()
                  (define (factorial n) (if (= n 1) 1 (n * (factorial (sub1 n)))))
                  factorial)))
           (code (motile/start e ENVIRON/TEST)))
      (display "test/serialize/3\n")
      (pretty-display (serialize code))))

  (test/serialize/1)
  (test/serialize/2a)
  (test/serialize/2b)
  (test/serialize/3))

(define (test/serialize/vector/persist)
  (define (test/serialize/vector/persist/1)
    (let ((e (motile/compile
              '(let ((v (vector/build 100 (lambda (i) i))))
                 v))))
      (display "serialize/vector/persist/1\n")
      (pretty-display (serialize (motile/start e ENVIRON/TEST)))
      (display "\n")))

  (define (test/serialize/vector/persist/2)
    (let ((e (motile/compile
              '(let* ((a (vector/build 100 (lambda (i) i)))
                      (b (vector/update a 50 5000)))
                 (list a b)))))
      (display "serialize/vector/persist/2\n")
      (pretty-display (serialize (motile/start e ENVIRON/TEST)))
      (display "\n")))

  (test/serialize/vector/persist/1)
  (test/serialize/vector/persist/2))

(define (test/deserialize/vector/persist)
  (define (test/deserialize/vector/persist/1)
    (let* ((v (vector/build 100 (lambda (i) i)))
           (e (motile/compile
               '(let ((x (vector/build 100 (lambda (i) i))))
                  x)))
           (flat (serialize (motile/start e ENVIRON/TEST))))
      (should-be 'deserialize/vector/persist/1 v (deserialize flat ENVIRON/TEST #f))))

  (define (test/deserialize/vector/persist/2)
    (let* ((a (vector/build 100 (lambda (i) i)))
           (b (vector/update a 50 5000))
           (e (motile/compile
                '(let* ((a (vector/build 100 (lambda (i) i)))
                      (b (vector/update a 50 5000)))
                 (list a b))))
           (flat (serialize (motile/start e ENVIRON/TEST))))
      (should-be 'deseriailze/vector/persist/2 (list a b) (deserialize flat ENVIRON/TEST #f))))


  (test/deserialize/vector/persist/1)
  (test/deserialize/vector/persist/2))

(define (test/serialize/hash/persist)
  (define (test/serialize/hash/persist/1)
    (let ((e (motile/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26))))
                 h/26))))
      (display "serialize/hash/persist/1\n")
      (pretty-display (serialize (motile/start e ENVIRON/TEST)))
      (display "\n")))
  
  (define (test/serialize/hash/persist/2)
    (let ((e (motile/compile
              '(let* ((h/26
                       (list/hash
                        hash/eq/null
                        '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                            k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                            u 21 v 22 w 23 x 24 y 25 z 26)))
                      (h/21
                       (let loop ((h h/26) (vowels '(a e i o u)))
                         (if (null? vowels)
                             h
                             (loop (hash/remove h (car vowels)) (cdr vowels))))))
                 (list h/26 h/21 h/26)))))
      (display "serialize/hash/persist/2\n")
      (pretty-display (serialize (motile/start e ENVIRON/TEST)))
      (display "\n")))

  (test/serialize/hash/persist/1)
  (test/serialize/hash/persist/2))

(define (test/deserialize/hash/persist)
  (define (test/deserialize/hash/persist/1)
    (let* ((gold (list/hash
                  hash/eq/null
                  '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                      k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                      u 21 v 22 w 23 x 24 y 25 z 26)))
           (e (motile/compile
               '(let ((h/26
                       (list/hash
                        hash/eq/null
                        '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                            k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                            u 21 v 22 w 23 x 24 y 25 z 26))))
                  h/26)))
           (flat (serialize (motile/start e ENVIRON/TEST)))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/hash/persist/1 gold plump)))

  (define (test/deserialize/hash/persist/2)
    (let* ((e (motile/compile
              '(let* ((h/26
                       (list/hash
                        hash/eq/null
                        '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                            k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                            u 21 v 22 w 23 x 24 y 25 z 26)))
                      (h/21
                       (let loop ((h h/26) (vowels '(a e i o u)))
                         (if (null? vowels)
                             h
                             (loop (hash/remove h (car vowels)) (cdr vowels))))))
                 (list h/26 h/21 h/26))))
           (flat (serialize (motile/start e ENVIRON/TEST)))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be
       'deserialize/hash/persist/2
       '(26 21 26 #t)
       (list
        (hash/length (car plump))
        (hash/length (cadr plump))
        (hash/length (caddr plump))
        (eq? (car plump) (caddr plump))))))

  (test/deserialize/hash/persist/1)
  (test/deserialize/hash/persist/2))

(define (test/serialize/set/persist)
  (define (test/serialize/set/persist/1)
    (let ((e (motile/compile
              '(let ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))
                alphabet))))
      (display "serialize/set/persist/1\n")
      (pretty-display (serialize (motile/start e ENVIRON/TEST)))
      (display "\n")))
  
  (define (test/serialize/set/persist/2)
    (let ((e (motile/compile
              '(let* ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                      (consonants
                       (let loop ((s alphabet) (vowels '(a e i o u)))
                         (if (null? vowels)
                             s
                             (loop (set/remove s (car vowels)) (cdr vowels))))))
                 (list alphabet consonants alphabet)))))
      (display "serialize/set/persist/2\n")
      (pretty-display (serialize (motile/start e ENVIRON/TEST)))
      (display "\n")))

  (test/serialize/set/persist/1)
  (test/serialize/set/persist/2))

(define (test/deserialize/set/persist)
  (define (test/deserialize/set/persist/1)
    (let* ((gold (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
           (e (motile/compile
               '(let ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))
                  alphabet)))
           (flat (serialize (motile/start e ENVIRON/TEST)))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/set/persist/1 gold plump)))

  (define (test/deserialize/set/persist/2)
    (let* ((e (motile/compile
              '(let* ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                      (consonants
                       (let loop ((s alphabet) (vowels '(a e i o u)))
                         (if (null? vowels)
                             s
                             (loop (set/remove s (car vowels)) (cdr vowels))))))
                 (list alphabet consonants alphabet))))
           (flat (serialize (motile/start e ENVIRON/TEST)))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be
       'deserialize/set/persist/2
       '(26 21 26 #t)
       (list
        (set/length (car plump))
        (set/length (cadr plump))
        (set/length (caddr plump))
        (eq? (car plump) (caddr plump))))))

  (test/deserialize/set/persist/1)
  (test/deserialize/set/persist/2))

(define (test/deserialize/tuple)
  (define (test/deserialize/tuple/1)
    (let* ((gold (list/tuple '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
           (e (motile/compile
               '(let ((alphabet (list/tuple '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))
                  alphabet)))
           (flat (serialize (motile/start e ENVIRON/TEST)))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/tuple/1 gold plump)))
  
    (define (test/deserialize/tuple/2)
    (let* ((gold (cons (tuple 'a 'e 'i 'o 'u) (list/tuple '(b c d f g h j k l m n p q r s t v w x y z))))
           (e (motile/compile
               '(let ((alphabet (list/tuple '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))
                  (tuple/partition alphabet (lambda (letter) (memq letter '(a e i o u)))))))
           (flat (serialize (motile/start e ENVIRON/TEST)))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/tuple/2 gold plump)))
  
  (test/deserialize/tuple/1)
  (test/deserialize/tuple/2))

(define (test/deserialize)
  (define (test/deserialize/1a)
    (let* ((e (motile/compile
               '(let ((a (list 11 22 33)))
                  (lambda (x) (append x a x)))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/1a '(a b 11 22 33 a b) (plump rtk/RETURN '(a b)))))

  (define (test/deserialize/1b)
    (let* ((e (motile/compile
               '(let ((a (lambda () (list 11 22 33)))
                      (b '#(lambda 0 #(constant/generate 99)))) ; A fake piece of motile code.
                  (lambda (x) (append (list b) (a) x)))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump+ (deserialize flat ENVIRON/TEST #t)))
      ;(pretty-display (cdr plump+))
      (should-be
       'deserialize/1b
       '(#(lambda 0 #(constant/generate 99)) 11 22 33 a b)
       ((car plump+) rtk/RETURN '(a b)))))

  (define (test/deserialize/2)
    (let* ((e (motile/compile
               '(let ()
                  (define (factorial n) (if (= n 1) 1 (* n (factorial (sub1 n)))))
                  factorial)))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      ;(pretty-display flat)
      (should-be 'deserialize/2 120 (plump rtk/RETURN 5))))

  (define (test/deserialize/3)
    (let* ((e (motile/compile
              '(lambda (a b) (+ a b))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/3 28 (plump rtk/RETURN 19 9))))

 (define (test/deserialize/4)
    (let* ((e (motile/compile
              '(lambda (a b c) (list a c b))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/4 '(19 12 9) (plump rtk/RETURN 19 9 12))))

  ; Deserializing a lambda expression with > 3 arguments.
  (define (test/deserialize/5)
    (let* ((e (motile/compile
              '(lambda (a b c d e f) (list a c e b d f))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/5 '(10 30 50 20 40 60) (plump rtk/RETURN 10 20 30 40 50 60))))
  
  ; Closures embedded in data structures.
  (define (test/deserialize/6)
    (let* ((e (motile/compile
               '(lambda ()
                  (define (even? n)
                    (if (= n 0) #t (odd? (- n 1))))
                  (define (odd? n)
                    (if (= n 0) #f (even? (- n 1))))
                  (let ((v (vector even? odd?)))
                    (list
                     ((vector-ref v 0) 12) ; even?
                     ((vector-ref v 0) 3)  ; even?
                     ((vector-ref v 1) 17) ; odd?
                     ((vector-ref v 1) 8)  ; odd?
                     )))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/6 '(#t #f #t #f) (plump rtk/RETURN))))

  ; Closure with a single rest argument.
  (define (test/deserialize/7a)
    (let* ((e (motile/compile
               '(let ((a 12) (b 13) (c (vector 'a 'b)))
                  (lambda rest (list b c a rest)))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/7a '(13 #(a b) 12 (100 200 300)) (plump rtk/RETURN 100 200 300))))

  ; Closure with two arguments, one being a rest argument.
  (define (test/deserialize/7b)
    (let* ((e (motile/compile
               '(let ((a 12) (b 13) (c (vector 'a 'b)))
                  (lambda (x . rest) (list x b c a rest)))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/7b '(100 13 #(a b) 12 (200 300)) (plump rtk/RETURN 100 200 300))))
    
  ; Closure with three arguments, one being a rest argument.
  (define (test/deserialize/7c)
    (let* ((e (motile/compile
               '(let ((a 12) (b 13) (c (vector 'a 'b)))
                  (lambda (x y . rest) (list x y b c a rest)))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/7c '(100 200 13 #(a b) 12 (300)) (plump rtk/RETURN 100 200 300))))

  ; Closure with four arguments, one being a rest argument.
  (define (test/deserialize/7d)
    (let* ((e (motile/compile
               '(let ((a 12) (b 13) (c (vector 'a 'b)))
                  (lambda (x y z . rest) (list x y z b c a rest)))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/7d '(100 200 300 13 #(a b) 12 (750)) (plump rtk/RETURN 100 200 300 750))))
  
  ; Closure with five arguments, one being a rest argument.
  (define (test/deserialize/7e)
    (let* ((e (motile/compile
               '(let ((a 12) (b 13) (c (vector 'a 'b)))
                  (lambda (w x y z . rest) (list w x y z b c a rest)))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/7e '(100 200 300 750 13 #(a b) 12 (99 88)) (plump rtk/RETURN 100 200 300 750 99 88))))


  ; lambda with a single rest argument.
  (define (test/deserialize/8a)
    (let* ((e (motile/compile
               '(lambda rest rest)))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/8a '(100 200 300) (plump rtk/RETURN 100 200 300))))

  ; lambda with two arguments, one being a rest argument.
  (define (test/deserialize/8b)
    (let* ((e (motile/compile
               '(lambda (x . rest) (list x rest))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/8b '(100 (200 300)) (plump rtk/RETURN 100 200 300))))
  
  ; lambda with three arguments, one being a rest argument.
  (define (test/deserialize/8c)
    (let* ((e (motile/compile
               '(lambda (x y . rest) (list y x rest))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/8c '(200 100 (300)) (plump rtk/RETURN 100 200 300))))
 
  ; lambda with four arguments, one being a rest argument.
  (define (test/deserialize/8d)
    (let* ((e (motile/compile
               '(lambda (x y z . rest) (list y z x rest))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/8d '(200 300 100 (750 200)) (plump rtk/RETURN 100 200 300 750 200))))

  ; lambda with five arguments, one being a rest argument.
  (define (test/deserialize/8e)
    (let* ((e (motile/compile
               '(lambda (w x y z . rest) (list w y z x rest))))
           (code (motile/start e ENVIRON/TEST))
           (flat (serialize code))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (should-be 'deserialize/8e '(100 300 750 200 (900 baseball)) (plump rtk/RETURN 100 200 300 750 900 'baseball))))   

  (test/deserialize/1a)
  (test/deserialize/1b)
  (test/deserialize/2)
  (test/deserialize/3)
  (test/deserialize/4)
  (test/deserialize/5)
  (test/deserialize/6)
  (test/deserialize/7a)
  (test/deserialize/7b)
  (test/deserialize/7c)
  (test/deserialize/7d)
  (test/deserialize/7e)
  (test/deserialize/8a)
  (test/deserialize/8b)
  (test/deserialize/8c)
  (test/deserialize/8d)
  (test/deserialize/8e))

(define (test/global/values)
  (define (test/global/values/1)
    (let* ((e (motile/compile
               '(let ((h (hash/new hash/eq/null 'cons cons '+ +))
                      (f (lambda (table) ((hash/ref table 'cons #f) 33 99))))
                  (f h))))
           (flat (serialize e))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (pretty-display flat)
      (pretty-display (motile/start plump ENVIRON/TEST))))

  (define (test/global/values/2)
    (let* ((table (motile/compile
                   '(let () (hash/new hash/eq/null 'cons cons '+ +))))
           (flat (serialize (motile/start table ENVIRON/TEST))))
      (pretty-display flat)))

  (define (test/global/values/3)
    (let* ((e (motile/compile '(let () (lambda (table) ((hash/ref table 'cons #f) 33 99)))))
           (f (motile/start e ENVIRON/TEST))
           (flat '((2) 0 () 0 () () (H eq (v! 256 (v! 132 (c cons M v! reference/global cons) (c + M v! reference/global +))))))
           (plump (deserialize flat ENVIRON/TEST #f)))
      (pretty-display plump)
      (pretty-display (f #f #f))
      (pretty-display (f rtk/RETURN plump))))

  (test/global/values/1)
  (test/global/values/2)
  (test/global/values/3))

(define (test/global)
  (define (test/global/1)
    (let ((e (motile/compile '(let ((x cons)) (x 33 99)))))
      (pretty-display (motile/start e ENVIRON/TEST))))

  (test/global/1))