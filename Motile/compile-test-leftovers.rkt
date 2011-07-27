#lang racket/base


;; this file contains old unit tests for compile.rkt that need to be converted to RackUnit somehow,
;; without necessarily testing the intricacies of the MAL format

;; setter is used internally to implement letrec. It is not available as a motile primitive and is not correct
;; with respect to motile closure construction (the side effect of setting a variable in an
;; outer scope A from within an inner scope B will NOT be seen within the inner scope B). The implementation
;; is sufficient to implement letrec and NOTHING MORE!
(define (test/setter)
  (define (test/setter/1)
    (let ((e (motile/compile `(let ((a 1))
                                (,(setter/tag) a 2)
                                a))))
      #;(pretty-display (decompile e))
      (should-be 'setter/1 2 (start e))))
  
  (define (test/setter/2)
    (let ((e (motile/compile `(let ((a 1))
                                (list
                                 (let ((b 17))
                                   (,(setter/tag) a 12) ; This side effect is invisible to this inner scope.
                                   (,(setter/tag) b 15)
                                   b)
                                 a))))) ; The reset of the value of a is seen here.
      
      #;(pretty-display (decompile e))
      (should-be 'test/setter/2 '(15 12) (start e))))
  
  (test/setter/1)
  (test/setter/2)
  )

(define (test/decompile)
  ; Helper routine.
  ; Show the original source and then the decompilation.
  (define (show title source)
    (display title) (newline)
    (pretty-display source) (newline)
    (pretty-display (decompile (motile/compile source))) (newline))
  
  (define (test/decompile/1)
    (let ((source '((lambda (x y z . rest)  ; Three positional arguments + rest argument.
                      (cons (+ x y z) rest)) 3 17 22 87 127)))
      (show 'decompile/1 source)))
  
  (define (test/decompile/2a)
    (let ((source '((lambda (a)
                      ((lambda (b) (+ a b)) 33))
                    11)))
      (show 'decompile/2a source)))
  
  (define (test/decompile/2b)
    (let* ((source '((lambda (a)
                       (lambda (b) (+ a b)))
                     11))
           (f (motile/compile source))
           (inner (start f)))
      (display "decompile/2b (innner closure)\n")
      #;(pretty-display source)
      (newline)
      #;(pretty-display (decompile inner))
      (newline)))
  
  (define (test/decompile/2c)
    (let* ((source '((lambda (a)
                       (lambda (b) (list a b)))
                     (vector 'constant/generate 11)))
           (f (motile/compile source))
           (inner (start f)))
      (display "decompile/2c (innner closure)\n")
      #;(pretty-display source)
      (newline)
      #;(pretty-display (decompile inner))
      (newline)))             
  
  (define (test/decompile/3a)
    (let* ((source '((lambda (u v) (lambda (w x) (+ u v x))) 19 1))
           (f (motile/compile source))
           (inner (start f)))
      (display "decompile/3a (innner closure)\n")
      #;(pretty-display source)
      (newline)
      #;(pretty-display (decompile inner))
      (newline)))
  
  (define (test/decompile/3b)
    (let* ((source '((lambda (u v) (lambda (w x) (+ u v x) (* u v w))) 19 1))
           (f (motile/compile source))
           (inner (start f)))
      (display "decompile/3b (innner closure)\n")
      #;(pretty-display source)
      (newline)
      #;(pretty-display (decompile inner))
      (newline))) 
  
  (define (test/decompile/4a)
    (let ((source
           '((lambda (a) ; Deeply nested lexical scope.
               ((lambda (b)
                  ((lambda (c)
                     ((lambda (d)
                        ((lambda (e)
                           ((lambda (f) (list a b c d e f))
                            6))
                         5))
                      4))
                   3))
                2))
             1)))
      (show 'decompile/4a source)))
  
  (test/decompile/1)
  (test/decompile/2a)
  (test/decompile/2b)
  (test/decompile/2c)
  (test/decompile/3a)
  (test/decompile/3b)
  (test/decompile/4a)
  )

#;(define (test/closure/inner)
    ;; Definition of two mutually recursive functions.
    (define (closure/inner/1)
      (let ((e (motile/compile
                '(letrec
                     ((fact (lambda (n) (if (= n 1) 1 (* n (fact (sub1 n)))))))
                   fact))))
        (display "closure/inner/1\n")
        (display "decompile of factorial\n")
        #;(pretty-display (motile/decompile (motile/start e ENVIRON/TEST)))))
    
    (define (closure/inner/2)
      (let ((e (motile/compile
                '(letrec ; Mutually recursive functions.
                     ((even?
                       (lambda (n)
                         (if (= n 0) #t (odd? (- n 1)))))
                      (odd?
                       (lambda (n)
                         (if (= n 0) #f (even? (- n 1))))))
                   even?))))
        
        (display "closure/inner/2\n")
        (display "decompile of complete letrec\n")
        #;(pretty-display (motile/decompile e))))
    
    (define (closure/inner/3)
      (let ((e (motile/compile
                '(letrec ; Mutually recursive functions.
                     ((even?
                       (lambda (n)
                         (if (= n 0) #t (odd? (- n 1)))))
                      (odd?
                       (lambda (n)
                         (if (= n 0) #f (even? (- n 1))))))
                   (list even? odd?)))))
        
        (display "closure/inner/3\n")
        (display "decompile of even? and odd\n")
        (let* ((x (motile/start e ENVIRON/TEST))
               (even? (list-ref x 0))
               (odd?  (list-ref x 1)))
          #;(pretty-display (motile/decompile even?))
          #;(pretty-display (motile/decompile odd?)))))
    
    (closure/inner/1)
    (closure/inner/2)
    (closure/inner/3))



(define (test/reglobal)
  
  (let ((e (motile/compile
            '(let ((x (lambda () foobar)))
               x)))
        (alpha (make-immutable-hasheq '((foobar . alpha))))
        (beta  (make-immutable-hasheq '((foobar . beta)))))
    #;(pretty-display (motile/decompile e))
    (display alpha) (display "\n")
    (display beta) (display "\n")
    (let ((a (e  (vector #f alpha)))
          (b (e  (vector #f beta))))
      (display (eq? a b)) (display "\n")
      (display (a ))
      (display "\n")
      (display (b ))
      (thread
       (lambda ()
         (let loop ()
           (display "a:")
           (display (a ))
           (display "\n")
           (sleep 0.2)
           (loop))))
      
      (thread
       (lambda ()
         (let loop ()
           (display "b:")
           (display (b ))
           (display "\n")
           (sleep 0.2)
           (loop)))))))

(define (test/environ)
  
  (define (test/environ/1)
    (let* ((e (motile/compile '(lambda () (random))))
           (E (environ/cons ENVIRON/TEST 'random (lambda (k _rte _global) (k (random 1000)))))
           (f (motile/start e E)))
      
      (display 'test/environ/1) (newline)
      #;(pretty-display (motile/decompile e)) (newline) (newline)
      #;(pretty-display (motile/decompile f)) (newline) (newline)
      (display (motile/start f E)) (newline)))
  
  (define (test/environ/2)
    (let ((e (motile/compile
              '((lambda (x) (random x)) 17)))
          (E (environ/cons ENVIRON/TEST 'random (lambda (k _rte _global x) (k (random x))))))
      (display 'test/environ/2) (newline)
      (display (motile/decompile e)) (newline) (newline)
      (display (motile/start e E)) (newline)))
  
  (define (test/environ/3)
    (let ((e (motile/compile
              '((lambda (n) (add1 n)) 33))))
      (display 'test/environ/3) (newline)
      (display (motile/decompile e)) (newline) (newline)
      (display (motile/start e ENVIRON/TEST)) (newline)))
  
  (test/environ/1)
  (test/environ/2)
  (test/environ/3))