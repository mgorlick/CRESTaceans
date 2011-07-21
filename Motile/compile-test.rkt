#lang racket/base

(require
 rackunit "compile.rkt"
 racket/pretty
 "baseline.rkt"
 "persistent/environ.rkt"
 "persistent/hash.rkt")


(test-case
 "Binding Environments"
 
 (check-eq?
  (let ((f (motile/compile '(begin environ/null))))
    (motile/start f BASELINE))
  environ/null
  "environ/null is available in the Motile BASELINE enviroment")
 
 (check-exn
  exn:fail?
  (lambda ()
    (let ((f (motile/compile
              '(let ((a "first")
                     (b "second")
                     (c 3))
                 (environ/cons environ/null a 1951 x))))) ; 1951 is not a legal argument.
      "The Motile compiler should issue an error exception"))
  "environ/cons requires that all arguments after environ expression are symbols")
 
 (check-exn
  exn:fail?
  (lambda ()
    (let ((f (motile/compile
              '(let ((a "first")
                     (b "second")
                     (c 3))
                 (environ/cons environ/null a b cons c))))) ; cons is not in lexical scope.
      "The Motile compiler should issue an error exception"))
  "environ/cons requires that all identifiers following the environ expression are in lexical scope")
 
 
 (check-equal?
  (let ((f (motile/compile
            '(let* ((silly 1951)
                    (E (environ/cons environ/null silly))) ; Create an environ with a single binding silly/1951.
               E))))
    (environ/value (motile/start f BASELINE) 'silly #f))
  1951
  "environ/cons adds a binding taken from lexical scope")
 
 (check-equal?
  (let* ((f (motile/compile
             '(let ((a "first")
                    (b "second")
                    (c 3))
                (environ/cons environ/null a b c))))
         (E (motile/start f BASELINE)))
    ; Here we use the version of environ/value available to Racket.
    (list (environ/value E 'c #f) (environ/value E 'b #f) (environ/value E 'a #f)))

  '(3 "second" "first")
  "Add multiple bindings at once to a binding environment")
 
 (check-equal?
  (let ((f (motile/compile
            '(let ((a 100) (b 200) (c 300))
               (let ((E (environ/cons environ/null a b c)))
                 (list (environ/value E c #f) (environ/value E a #f) (environ/value E b #f)))))))
    (motile/start f BASELINE))
  '(300 100 200)
  "Query an environ using environ/value")
 
 (check-equal?
  (let ((f (motile/compile
            '(let ((a 100) (b 200) (c 300))
               (let ((E (environ/cons environ/null a b c)))
                 (list (environ/value E c #f)
                       (environ/value E a #f)
                       (environ/value E x #f) ; x is not a binding in E.
                       (environ/value E b #f)))))))
    (motile/start f BASELINE))
  
  '(300 100 #f 200) ; The value #f comes from the query for symbol x.
  
  "environ/value using a symbol not in the binding environ")
 
 (check-equal?
  (let ((f (motile/compile
            '(let ((a 100) (b 200) (c 300))
               (let* ((E_0 (environ/cons environ/null a b c))
                      (E_1 (environ/remove E_0 b)))
                 (list (environ/value E_1 c #f)
                       (environ/value E_1 a #f)
                       (environ/value E_1 b #f)))))))
    (motile/start f BASELINE))
  '(300 100 #f) ; The value #f comes from the query for symbol b in E_1.
  "environ/remove")
 
 (check-eq?
  (let ((f (motile/compile '(let () (environ/capture)))))
    (motile/start f BASELINE))
  BASELINE
  "environ/capture")
 
 (check-equal?
  (let ((f (motile/compile
            '(let ((global (let ((a 100) (b 200) (c 300) (plus +))
                             (environ/cons environ/null a b c plus))))
               (environ/reflect global (plus a b c))))))
    (motile/start f BASELINE))
  600
  "environ/reflect"))
 
(let ((f (motile/compile
          '(let ((a 100) (b 200))
             (let ((c 300) (d 400))
               (let ((e 500) (f 600))
                 (environ/cons environ/null a c e)))))))
  (pretty-display (motile/decompile f)))

(let ((f (motile/compile
            '(let ((silly 1951))
               (environ/cons environ/null silly))))) ; Create an environ with a single binding silly/1951.
  (pretty-display (motile/decompile f)))

;; >>>>>>>>>>>> START of original compiler regression tests <<<<<<<<<<<<<<
;; These will eventually all be converted to rackunit tests.
      
(define (should-be id expected outcome)
  (if (equal? expected outcome)
      (display (format "PASS id: ~a expected: ~s outcome: ~s\n" id expected outcome))
      (display (format "FAIL id: ~a expected: ~s outcome: ~s\n" id expected outcome))))

(define (test/constants)
  (define (test/constant/0)
    (let ((f (compile 0)))
      (should-be 'constant/0 0 (start f))))
  
  (define (test/constant/1)
    (let ((f (compile 1)))
      (should-be 'constant/1 1 (start f)))) 
  
  (define (test/constant/2)
    (let ((f (compile 2)))
      (should-be 'constant/2 2 (start f))))
  
  (define (test/constant/true)
    (let ((f (compile #t)))
      (should-be 'constant/true #t (start f))))
  
  (define (test/constant/false)
    (let ((f (compile #f)))
      (should-be 'constant/false #f (start f))))
  
  (define (test/constant/nil)
    (let ((f (compile null)))
      (should-be 'constant/nil '() (start f))))
  
  (define (test/constant/string)
    (let ((f (compile "foobar")))
      (should-be 'constant/string "foobar" (start f))))
  
  (define (test/constant/symbol)
    (let ((f (compile ''redondo)))
      (should-be 'constant/symbol 'redondo (start f))))

  (define (test/constant/list)
    (let ((f (compile ''(1 2 3))))
      (should-be 'constant/list '(1 2 3) (start f))))

  (define (test/constant/vector)
    (let ((f (compile #(1 2 3))))
      (should-be 'constant/vector #(1 2 3) (start f))))

  (test/constant/0)
  (test/constant/1)
  (test/constant/2)
  (test/constant/true)
  (test/constant/false)
  (test/constant/nil)
  (test/constant/string)
  (test/constant/symbol)
  (test/constant/list)
  (test/constant/vector))

;; The individual functions in the global baseline namespace.
(define (test/base)
  (define (test/base/cons)
    (let ((f (compile '((lambda () (cons 29 82))))))
      (should-be 'base/cons (cons 29 82) (start f))))
  
  (define (test/base/car)
    (let ((f (compile '((lambda ()(car (cons 29 82)))))))
      (should-be 'base/car 29 (start f))))
  
  (define (test/base/cdr)
    (let ((f (compile '((lambda () (cdr (cons 29 82)))))))
      (should-be 'base/cdr 82 (start f))))

  (define (test/base/null?)
    (let ((f (compile '(lambda () (null? '()))))
          (g (compile '((lambda () (null? '(1 2 3)))))))
      (should-be 'base/null?/1 #t (start f))
      (should-be 'base/null?/2 #f (start g))))
  
  (define (test/base/not)
    (let ((f (compile '(not #f)))
          (g (compile '(not #t))))
      (should-be 'base/not/1 #t (start f))
      (should-be 'base/not/2 #f (g rtk/RETURN RTE))))
  
  (define (test/base/<)
    (let ((f (compile '(< 8 31)))
          (g (compile '(< -1 -7))))
      (should-be 'base/</1 #t (start f))
      (should-be 'base/</2 #f (g rtk/RETURN RTE))))

  (define (test/base/=)
    (let ((f (compile '(= 8 8.0)))
          (g (compile '(= -1 -7))))
      (should-be 'base/=/1 #t (start f))
      (should-be 'base/=/2 #f (g rtk/RETURN RTE))))

  (define (test/base/add1)
    (let ((f (compile '(add1 93051))))
      (should-be 'base/add1 93052 (start f))))

  (define (test/base/sub1)
    (let ((f (compile '(sub1 93051))))
      (should-be 'base/sub1 93050 (start f))))
  
  (define (test/base/+)
    (let ((f (compile '(+ 93051)))
          (g (compile '(+ 812 23)))
          (h (compile '(+ 812 23 -10)))
          (i (compile '(+ 812 23 -10 17))))
      (should-be 'base/+/1 93051 (start f))
      (should-be 'base/+/2 835 (g rtk/RETURN RTE))
      (should-be 'base/+/3 825 (h rtk/RETURN RTE))
      (should-be 'base/+/4 842 (i rtk/RETURN RTE))))
      
  (define (test/base/*)
    (let ((f (compile '(* 93051)))
          (g (compile '(* 812 23)))
          (h (compile '(* 812 23 -10)))
          (i (compile '(* 812 23 -10 17))))
      (should-be 'base/*/1 93051 (start f))
      (should-be 'base/*/2 18676 (g rtk/RETURN RTE))
      (should-be 'base/*/3 -186760 (h rtk/RETURN RTE))
      (should-be 'base/*/4 -3174920 (i rtk/RETURN RTE))))
  
  (define (test/global/display)
    (let ((f (compile '(display "hello world\n"))))
      (start f)
      (should-be 'global/display "hello world" "hello world"))) ; Bogus but suggests what the tester should see.

  (define (test/global/format)
    (let ((f (compile '(format "~a ~a ~a ~s" 99 "hello" '(1 2 3) '|a weird name|))))
      (should-be 'global/format "99 hello (1 2 3) |a weird name|" (start f))))          
  
  (test/base/cons)
  (test/base/car)
  (test/base/cdr)
  (test/base/null?)
  (test/base/not)
  (test/base/<)
  (test/base/=)
  (test/base/add1)
  (test/base/sub1)
  (test/base/+)
  (test/base/*)
  (test/global/display)
  (test/global/format))


(define (test/lambda)
  (define (test/lambda/1)
    (let ((f (motile/compile '((lambda () 1))))) ; Trivial zero argument lambda expression.
      (should-be 'lambda/1 1 (motile/start f ENVIRON/TEST))))

  (define (test/lambda/2)
    (let ((f (motile/compile '((lambda (x) x) 17)))) ; The identity function.
      (should-be 'lambda/2 17 (motile/start f ENVIRON/TEST))))

  (define (test/lambda/3)
    (let ((f (motile/compile '((lambda (x) (1+ x)) 99)))) ; Single argument invoking a primitive in the global namespace.
      (should-be 'lambda/3 100 (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/4)
    (let ((f (motile/compile '((lambda (x y) (+ x y)) 99 33)))) ; Two arguments.
      (display 'lambda/4) (newline)
      (pretty-display (motile/decompile f))
      (should-be 'lambda/4 132 (motile/start f ENVIRON/TEST))))

  (define (test/lambda/5)
    (let ((f (motile/compile '((lambda (x y z) (+ x y z)) 1 2 3)))) ; Three arguments.
      (display 'lambda/4) (newline)
      (pretty-display (motile/decompile f))
      (should-be 'lambda/5 6 (motile/start f ENVIRON/TEST))))

  (define (test/lambda/6)
    (let ((f (motile/compile '((lambda (w x y z) (+ w x y z)) 1 2 3 4)))) ; Four arguments.
      (should-be 'lambda/6 10 (motile/start f ENVIRON/TEST))))

  (define (test/lambda/7)
    (let ((f (motile/compile '((lambda (v w x y z) (+ v w x y z)) 1 2 3 4 5)))) ; Five arguments.
      (should-be 'lambda/7 15 (motile/start f ENVIRON/TEST))))    
  
  (define (test/lambda/8)
    (let ((f (motile/compile
              '((lambda (x y z) ; In-order evaluation of a series of expressions in a lambda body.
                  (display (format "~a\n" x))
                  (display (format "~a\n" y))
                  (display (format "~a\n" z))
                  (+ x y z))
                10 20 30))))
      (should-be 'lambda/8 60 (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/9)
    (let ((f (motile/compile '((lambda rest rest) 3 17 22 87)))) ; Rest argument.
      (should-be 'lambda/9 '(3 17 22 87) (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/10a)
    (let ((f (motile/compile '((lambda (x . rest) (cons x rest)) 3 17 22 87)))) ; One positional argument + rest argument.
      (should-be 'lambda/10a '(3 17 22 87) (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/10b)
        (let ((f (motile/compile '((lambda (x y . rest) (cons (+ x y) rest)) 3 17 22 87)))) ; Two positional arguments + rest argument.
      (should-be 'lambda/10b '(20 22 87) (motile/start f ENVIRON/TEST))))

  (define (test/lambda/11a)
    (let ((f (motile/compile
              '((lambda (x y z . rest)  ; Three positional arguments + rest argument.
                  (cons (+ x y z) rest)) 3 17 22 87 127))))
      (should-be 'lambda/11a '(42 87 127) (motile/start f ENVIRON/TEST))))
  
    (define (test/lambda/11b)
    (let ((f (motile/compile
              '((lambda (a b c d . rest)  ; Three positional arguments + rest argument.
                  (cons (+ a b c d) rest)) 3 17 22 87 127 999))))
      (should-be 'lambda/11b '(129 127 999) (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/12a)
    (let ((f (motile/compile
              '(((lambda (a) (lambda () (add1 a))) 33)) ; One closed variable + zero parameters.
              )))
      (display 'lambda/12a) (newline)
      (pretty-display (motile/decompile f))
      (should-be 'lambda/12a 34 (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/12b)
    (let ((f (motile/compile
              '((lambda (a)
                  ((lambda (b) (+ a b)) 33)) ; One closed variable + one parameter.
                11))))
      (pretty-display (motile/decompile f))
      (should-be 'lambda/12b 44 (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/12c)
    (let ((f (motile/compile
              '((lambda (u v) ; Lexical scope.
                  ((lambda (w x) (+ u v x)) 300 400)) ; This lambda has two closed variables, u and v and two parameters, w and x.
                19 1))))
      (should-be 'lambda/12c 420 (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/13)
    (let ((f (motile/compile
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
                1))))
      (should-be 'lambda/13 '(1 2 3 4 5 6) (motile/start f ENVIRON/TEST))))
           
  (define (test/lambda/14)
    (let ((f (motile/compile
              '((lambda (a b) ; Inner lexical scope shadows outer lexical scope.
                 ((lambda (b)
                    ((lambda (a) (+ a b)) 85))
                  22))
               5 10))))
      (should-be 'lambda/14 107 (motile/start f ENVIRON/TEST))))

    
  ; Combining rest arguments with closed variables.
  (define (test/lambda/15a)
    (let ((f (motile/compile '((lambda rest ((lambda (a) (cons a rest)) 33)) 'alpha 'beta 'gamma))))
      (should-be 'lambda/15 '(33 alpha beta gamma) (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/15b)
    (let ((f (motile/compile '((lambda (a b) ((lambda (x y . rest) (list* (+ a x) (+ b y) rest)) 100 200 300 400 500)) 11 13))))
      (display 'lambda/15b) (newline)
      (pretty-display (motile/decompile f)) (newline)
      (should-be 'lambda/15b '(111 213 300 400 500) (motile/start f ENVIRON/TEST))))

  (define (test/lambda/99)
    (let ((f (motile/compile '((lambda lambda lambda) 7 8 9 10)))) ; lambda must not be a reserved symbol.
      (should-be 'lambda/99 '(7 8 9 10) (motile/start f ENVIRON/TEST))))  
    
  (test/lambda/1)
  (test/lambda/2)
  (test/lambda/3)
  (test/lambda/4)
  (test/lambda/5)
  (test/lambda/6)
  (test/lambda/7)
  (test/lambda/8)
  (test/lambda/9)
  (test/lambda/10a)
  (test/lambda/10b)
  (test/lambda/11a)
  (test/lambda/11b)
  (test/lambda/12a)
  (test/lambda/12b)
  (test/lambda/12c)
  (test/lambda/13)
  (test/lambda/14)
  (test/lambda/15a)
  (test/lambda/15b)
  (test/lambda/99))

(define (test/decompile)
  ; Helper routine.
  ; Show the original source and then the decompilation.
  (define (show title source)
    (display title) (newline)
    (pretty-display source) (newline)
    (pretty-display (decompile (compile source))) (newline))
    
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
           (f (compile source))
           (inner (start f)))
      (display "decompile/2b (innner closure)\n")
      (pretty-display source)
      (newline)
      (pretty-display (decompile inner))
      (newline)))

  (define (test/decompile/2c)
    (let* ((source '((lambda (a)
                       (lambda (b) (list a b)))
                     (vector 'constant/generate 11)))
           (f (compile source))
           (inner (start f)))
      (display "decompile/2c (innner closure)\n")
      (pretty-display source)
      (newline)
      (pretty-display (decompile inner))
      (newline)))             
             
  (define (test/decompile/3a)
    (let* ((source '((lambda (u v) (lambda (w x) (+ u v x))) 19 1))
           (f (compile source))
           (inner (start f)))
      (display "decompile/3a (innner closure)\n")
      (pretty-display source)
      (newline)
      (pretty-display (decompile inner))
      (newline)))

  (define (test/decompile/3b)
    (let* ((source '((lambda (u v) (lambda (w x) (+ u v x) (* u v w))) 19 1))
           (f (compile source))
           (inner (start f)))
      (display "decompile/3b (innner closure)\n")
      (pretty-display source)
      (newline)
      (pretty-display (decompile inner))
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

(define (test/let)
  (define (test/let/1)
    (let ((e (motile/compile '(let ((a 1) (b 2) (c 3)) (list a b c)))))
      (should-be 'let/1 '(1 2 3) (motile/start e ENVIRON/TEST))))
  
  ; The lexical scope established by a let is seen only by the expressions in the body of the let.
  ; Here the cons in the body of the lambda should resolve to cons in the global namespace and
  ; the cons in the let body should resolve to the lambda expression.
  (define (test/let/2)
    (let ((e (motile/compile '(let ((cons (lambda (x y)
                                     (cons (cons x '()) (cons y '()))))) ; Rebinding of cons in body of let.
                         (cons 'foo 'bar))))) ; Body of let
      (should-be 'let/2 '((foo) bar) (motile/start e ENVIRON/TEST))))
  
  (define (test/let/3)
    (let ((e (motile/compile '(let ((a 1) (b 2) (c 3) (d "silly") (e #(8 9 10)) (f (list 'x 'y)))
                         (list a b c d e f)))))
      (should-be 'let/3 '(1 2 3 "silly" #(8 9 10) (x y)) (motile/start e ENVIRON/TEST))))
  
  (define (test/let/4)
    (let ((e (motile/compile (let () (display "alpha\n") (display "beta\n") "gamma")))) ; Let with no definitions.
      (should-be 'let/4 "gamma" (motile/start e ENVIRON/TEST))))
  
  (test/let/1)
  (test/let/2)
  (test/let/3)
  (test/let/4))

(define (test/foo)
  (let ((e (compile '(let*
                         ((a 11)
                          (b (lambda ()
                               (display "INSIDE b\n\n")
                               (display (format "\nRTE inside b ~a\n\n" (rte/reveal)))
                               (+ a 13))))
                       (display (format "\nRTE before b ~a\n\n" (rte/reveal)))
                       (b)))))
    (start e)))

;; setter is used internally to implement letrec. It is not available as a Mischief primitive and is not correct
;; with respect to Mischief closure construction (the side effect of setting a variable in an
;; outer scope A from within an inner scope B will NOT be seen within the inner scope B). The implementation
;; is sufficient to implement letrec and NOTHING MORE!
(define (test/setter)
  (define (test/setter/1)
    (let ((e (compile `(let ((a 1))
                         (,(setter/tag) a 2)
                         a))))
      (pretty-display (decompile e))
      (should-be 'setter/1 2 (start e))))

  (define (test/setter/2)
    (let ((e (compile `(let ((a 1))
                         (list
                          (let ((b 17))
                            (,(setter/tag) a 12) ; This side effect is invisible to this inner scope.
                            (,(setter/tag) b 15)
                            b)
                          a))))) ; The reset of the value of a is seen here.

      (pretty-display (decompile e))
      (should-be 'test/setter/2 '(15 12) (start e))))

  (test/setter/1)
  (test/setter/2)
  )

(define (test/letrec)
  (define (test/letrec/1)
    (let ((e (motile/compile '(letrec
                           ((a 11)
                            (b (lambda () (+ a 13))))
                         (b)))))
      (should-be 'letrec/1 24 (motile/start e ENVIRON/TEST))))

  (define (test/letrec/2a)
    (let* ((source '(letrec
                           ((f (lambda (n)
                                 (if (= n 1) 1 (* n (f (sub1 n))))))) ; Factorial.
                         (f 5)))
           (e (motile/compile source)))
      (should-be 'letrec/2a 120 (motile/start e ENVIRON/TEST))))
  
  (define (test/letrec/2b)
    (let ((e (motile/compile
              '(letrec
                   ((f (lambda (n)
                         (if (= n 1) 1 (* n (f (sub1 n))))))) ; Factorial.
                 f))))
      (display "letrec/2b\n")
      (pretty-display (motile/decompile e))))
  
  (define (test/letrec/2c)
    (let ((e (motile/compile
              '(letrec
                   ((f (lambda (n)
                         (if (= n 1) 1 (* n (f (sub1 n))))))) ; Factorial.
                 f))))
      (display "letrec/2c\n")
      (motile/decompile (motile/start e ENVIRON/TEST))))
  
  ;; Recursive definition of two functions.
  (define (test/letrec/3a)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1))))))
                 (list
                  (even? 12) (even? 3)
                  (odd? 17)  (odd? 8))))))

      (should-be 'letrec/3a '(#t #f #t #f) (motile/start e ENVIRON/TEST))))
  
  ;; Recursive definition of two functions.
  (define (test/letrec/3b)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1))))))
                 (list
                  (even? 12) (even? 3)
                  (odd? 17)  (odd? 8))))))

      (display "letrec/3b\n")
      (pretty-display (motile/decompile e))))
  
  ;; Test of decompilation combining recursive function definitions with multiple (> 1) closed variables per function.
  (define (test/letrec/3c)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n zero) #t (odd? (- n one)))))
                    (odd?
                     (lambda (n)
                       (if (= n zero) #f (even? (- n one)))))
                    (zero 0)
                    (one  1))
                 (list
                  (even? 12) (even? 3)
                  (odd? 17)  (odd? 8))))))

      (display "letrec/3c\n")
      (pretty-display (motile/decompile e))))


  ;; Recursive definition of three functions.
  (define (test/letrec/4)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1)))))
                    (factorial
                     (lambda (n)
                       (if (= n 1) 1 (* n (factorial (sub1 n)))))))

                 (list
                  (even? 12) (even? 3)
                  (odd? 17)  (odd? 8)
                  (factorial 5))))))

      (should-be 'letrec/4 '(#t #f #t #f 120) (motile/start e ENVIRON/TEST))))


  ;; Recursive definition of five bindings.
  (define (test/letrec/5)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1)))))
                    (factorial
                     (lambda (n)
                       (if (= n 1) 1 (* n (factorial (sub1 n))))))
                    (a 11)
                    (b (lambda () (+ a 13))))

                 (list
                  (even? 12) (even? 3)
                  (odd?  a)  (odd? 8)
                  (factorial 5)
                  (b))))))

      (should-be 'letrec/5 '(#t #f #t #f 120 24) (motile/start e ENVIRON/TEST))))
  
  (define (test/letrec/6)
    (let ((e (motile/compile
              '(letrec ((foo (lambda (x) (eq? x foo))))
                 (foo foo)))))
      (should-be 'letrec/6 #t (motile/start e ENVIRON/TEST))))
  
  ; Letrec with (define ...) in the body.
  (define (test/letrec/7)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1)))))
                    (factorial
                     (lambda (n)
                       (if (= n 1) 1 (* n (factorial (sub1 n)))))))

                 ; Definitions in the letrec body.
                 (define a 11)
                 (define b (lambda () (+ a 13)))
                 (list
                  (even? 12) (even? 3)
                  (odd?  a)  (odd? 8)
                  (factorial 5)
                  (b))))))
      (should-be 'letrec/7 '(#t #f #t #f 120 24) (motile/start e ENVIRON/TEST))))

  (define (test/letrec/tak) ; The (in)famous Takeuchi function.
    (let ((e (motile/compile
              '(letrec
                   ((tak
                     (lambda (x y z)
                       (if (not (< y x))
                           z
                           (tak (tak (- x 1) y z)
                                (tak (- y 1) z x)
                                (tak (- z 1) x y))))))
                 (tak 18 12 6)))))
      (should-be 'letrec/tak 7 (motile/start e ENVIRON/TEST))))
  
  (test/letrec/1)
  (test/letrec/2a)
  (test/letrec/2b)
  (test/letrec/2c)
  (test/letrec/3a)
  (test/letrec/3b)
  (test/letrec/3c)
  (test/letrec/4)
  (test/letrec/5)
  (test/letrec/6)
  (test/letrec/7)
  (test/letrec/tak))

(define (test/closure/inner)
  ;; Definition of two mutually recursive functions.
  (define (closure/inner/1)
    (let ((e (motile/compile
           '(letrec
                ((fact (lambda (n) (if (= n 1) 1 (* n (fact (sub1 n)))))))
              fact))))
      (display "closure/inner/1\n")
      (display "decompile of factorial\n")
      (pretty-display (motile/decompile (motile/start e ENVIRON/TEST)))))
  
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
      (pretty-display (motile/decompile e))))
  
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
        (pretty-display (motile/decompile even?))
        (pretty-display (motile/decompile odd?)))))
  
  (closure/inner/1)
  (closure/inner/2)
  (closure/inner/3))

(define (test/let*)
  (define (test/let*/1)
    (let ((e (motile/compile
              '(let* ((a 1)
                      (b (+ a 13))
                      (c (+ a b 20)))
                 (+ a b c)))))
      (should-be 'let*/1 50 (motile/start e ENVIRON/TEST))))
  
    (define (test/let*/2)
    (let ((e (motile/compile
              '(let* ((a 1)
                      (b (+ a 13))
                      (c (lambda () (+ a b 20))))
                 (c)))))
      (should-be 'let*/2 35 (motile/start e ENVIRON/TEST))))
  
  (test/let*/1)
  (test/let*/2))

(define (test/named-let)
  (define (test/named-let/1)
    (let ((e (motile/compile
              '(let loop ((numbers '(3 -2 1 6 -5))
                          (nonnegative '())
                          (negative '()))
                 (cond ((null? numbers)
                        (list nonnegative negative))
                       ((>= (car numbers) 0)
                        (loop (cdr numbers)
                              (cons (car numbers) nonnegative)
                              negative))
                       ((< (car numbers) 0)
                        (loop (cdr numbers)
                              nonnegative
                              (cons (car numbers) negative))))))))
      (should-be 'named-let/1 '((6 1 3) (-5 -2)) (motile/start e ENVIRON/TEST))))
  
  (test/named-let/1))

(define (test/define)
  (define (test/define/1) ; A lambda may contain one or more definitions.
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a+) (add1 a))
                  (define (b-) (sub1 b))
                  (* (a+) (b-)))
                8 -1))))
      (should-be 'define/1 -18 (motile/start f ENVIRON/TEST))))
  
  (define (test/define/2a) ; A definition may contain nested definitions.
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a+) ; Define within lambda body.
                    (define (b-) (sub1 b)) ; Nested definition within definition of a+
                    (* (add1 a) (b-))) ; Body of a+
                  (a+))))))
      (display "\ndefine/2a\n")
      (pretty-display (motile/decompile f))))  
  

   (define (test/define/2b) ; A definition may contain nested definitions.
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a+) ; Define within lambda body.
                    (define (f) b) ; Nested definition within definition of a+
                    (+ a (f))) ; Body of a+
                  (a+))))))
      (display "\ndefine/2b\n")
      (pretty-display (motile/decompile f))))
  
  (define (test/define/2c) ; A definition may contain nested definitions.
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a*) ; Define within lambda body.
                    (define (b-) (sub1 b)) ; Nested definition within definition of a*
                    (* a (b-))) ; Body of a*
                  (a*))
                8 -1))))
      (display "\ndefine/2c\n")
      (pretty-display (motile/decompile f))
      (should-be 'define/2c -16 (motile/start f ENVIRON/TEST))))
  
  (define (test/define/3) ; A definition may be recursive.
    (let ((f (motile/compile
              '((lambda (a)
                  (define (fib n)
                    (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
                  (fib a))
                13))))
      (should-be 'define/3 233 (motile/start f ENVIRON/TEST))))
  
  (define (test/define/4) ; A let may contain one or more definitions.
    (let ((f (motile/compile
              '(let ((a 8) (b -1))
                  (define (a+) (add1 a))
                  (define (b-) (sub1 b))
                  (* (a+) (b-))))))
      (should-be 'define/4 -18 (motile/start f ENVIRON/TEST))))
  
  (define (test/define/5) ; A let* may contain one or more definitions.
    (let ((f (motile/compile
              '(let* ((a 8) (b (+ a 4)))
                 (define (fib n)
                   (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
                 (list a (fib a) b (fib b))))))
      (should-be 'define/5 '(8 21 12 144) (motile/start f ENVIRON/TEST))))
  
  (define (test/define/6) ; A letrec may contain one or more definitions.
    (let ((f (motile/compile
              '(letrec ((a 8) (b (lambda () (+ a 4))))
                 (define (fib n)
                   (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
                 (list a (fib a) (b) (fib (b)))))))
      (should-be 'define/6 '(8 21 12 144) (motile/start f ENVIRON/TEST))))
  
  (define (test/define/7) ; A letrec may contain one or more definitions.
    (let ((f (motile/compile
              '(letrec ((a 8) (b -1))
                  (define (a+) (add1 a))
                  (define (b-) (sub1 b))
                  (* (a+) (b-))))))
      (should-be 'define/7 -18 (motile/start f ENVIRON/TEST))))  
  
  (test/define/1)
  (test/define/2a)
  (test/define/2b)
  (test/define/2c)
  (test/define/3)
  (test/define/4)
  (test/define/5)
  (test/define/6)
  (test/define/7)
  )

(define (test/begin)
  (define (test/begin/1)
    (let ((f (compile
              '(begin 88))))
      (should-be 'begin/1 88 (start f))))
  
  (define (test/begin/2)
    (let ((f (compile
              '(begin
                 (display "begin/2 first\n")
                 (display "begin/2 second\n")
                 (display "begin/2 third\n")
                 99))))
      (should-be 'begin/2 99 (start f))))
  
  (test/begin/1)
  (test/begin/2)
  )

(define (test/when)
  (define (test/when/1)
    (let ((e (compile '(when #t 'hello))))
      (should-be 'when/1 'hello (start e))))
  
  (define (test/when/2)
    (let ((e (compile '(when #f 'wrong))))
      (should-be 'when/2 (void) (start e))))
  
  (define (test/when/3)
    (let ((e (compile
              '((lambda (n)
                  (when (> n 3) (display "when/3 first\n") (display "when/3 second\n") n))
                17))))
      (should-be 'when/3 17 (start e))))
  
  (test/when/1)
  (test/when/2)
  (test/when/3))

(define (test/unless)
  (define (test/unless/1)
    (let ((e (compile '(unless #f 'hello))))
      (should-be 'unless/1 'hello (start e))))
  
  (define (test/unless/2)
    (let ((e (compile '(unless #t 'wrong))))
      (should-be 'unless/2 (void) (start e))))
  
  (define (test/unless/3)
    (let ((e (compile
              '((lambda (n)
                  (unless (> n 3) (display "unless/3 first\n") (display "unless/3 second\n") n))
                2))))
      (should-be 'unless/3 2 (start e))))
  
  (test/unless/1)
  (test/unless/2)
  (test/unless/3))

(define (test/cond)
  (define (test/cond/1)
    (let* ((source 
            '(lambda (a b)
               (define (fib n)
                 (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
               (cond
                 ((> a 9) (fib a))
                 ((> (+ a b) 11) (fib (+ a b)))
                 (else (+ (fib a) (fib b))))))
           (a (compile `(,source 10 0))) ; Exercise first clause of cond.
           (b (compile `(,source 8 5)))  ; Exercise second clause of cond.
           (c (compile `(,source 5 6)))) ; Exercise third clause of cond.

      (should-be 'cond/1a 55  (start a))
      (should-be 'cond/1b 233 (start b))
      (should-be 'cond/1c 13  (start c))))
           
  (define (test/cond/2) ; Exercises (test => procedure) clause.
    (let* ((source 
            '(lambda (a b)
               (define (fib n)
                 (cond
                   ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (fib (sub1 n)) (fib (- n 2))))))
               (cond
                 ((> a 9) (fib a))
                 ((> (+ a b) 11) (fib (+ a b)))
                 ((+ a b) => (lambda (n)  (* 2 (fib n)))))))
           (a (compile `(,source 10 0))) ; Exercise first clause of cond.
           (b (compile `(,source 8 5)))  ; Exercise second clause of cond.
           (c (compile `(,source 5 6)))) ; Exercise third clause of cond.

      (should-be 'cond/2a 55  (start a))
      (should-be 'cond/2b 233 (start b))
      (should-be 'cond/2c 178 (start c))))  

  (define (test/cond/3) ; Exercises the trivial (test) clause.
    (let* ((source 
            '(lambda (a b)
               (define (fib n)
                 (cond
                   ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (fib (sub1 n)) (fib (- n 2))))))
               (cond
                 ((> a 9) (fib a))
                 ((> (+ a b) 11) (fib (+ a b)))
                 ((+ a b)))))
           (a (compile `(,source 10 0))) ; Exercise first clause of cond.
           (b (compile `(,source 8 5)))  ; Exercise second clause of cond.
           (c (compile `(,source 5 6)))) ; Exercise third clause of cond.

      (should-be 'cond/3a 55  (start a))
      (should-be 'cond/3b 233 (start b))
      (should-be 'cond/3c 11  (start c))))

  (define (test/cond/4) ; Exercises (test) clause.
    (let* ((source 
            '(lambda (a b)
               (define (fib n)
                 (cond
                   ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (fib (sub1 n)) (fib (- n 2))))))
               (cond
                 ((> a 9) (fib a))
                 ((+ a b)) ; If the prior clause fails then this clause should ALWAYS execute.
                 ((> (+ a b) 11) (fib (+ a b))))))
           (a (compile `(,source 10 0))) ; Exercise first clause of cond.
           (b (compile `(,source 8 5)))  ; Exercise second clause of cond.
           (c (compile `(,source 5 6)))) ; Exercise third clause of cond.

      (should-be 'cond/4a 55  (start a))
      (should-be 'cond/4b 13 (start b))
      (should-be 'cond/4c 11  (start c))))

  (test/cond/1)
  (test/cond/2)
  (test/cond/3)
  (test/cond/4))

(define (test/case)
  (define (test/case/1)
    (let ((e (compile
              '(case (* 2 3)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8 9) 'composite)))))
      (should-be 'case/1 'composite (start e))))
  
  (define (test/case/2)
    (let ((e (compile
              '(case (car '(c d))
                 ((a) 'a)
                 ((b) 'b)))))
      (should-be 'case/2 (void) (start e))))
  
  (define (test/case/3)
    (let ((e (compile
              '(case (car '(c d))
                 ((a e i o u) 'vowel)
                 ((w y) 'semivowel)
                 (else 'consonant)))))
      (should-be 'case/3 'consonant (start e))))

  (test/case/1)
  (test/case/2)
  (test/case/3))
                
(define (test/do)
  (define (test/do/1)
    (let* ((source
           '(do ((vec (make-vector 5))
                 (i 0 (+ i 1)))
              ((= i 5) vec)
              (vector-set! vec i i)))
           (e (compile source)))
      (display "do/1\n")
      (pretty-display source)
      (do/translate source)
      (should-be 'do/1 #(0 1 2 3 4) (start e))))
  
  (define (test/do/2)
    (let* ((source
            '(do ((vec (make-vector 5))
                  (i 0 (+ i 1)))
               ((= i 5)) ; No expressions following <test>.
               (vector-set! vec i i)))
           (e (compile source)))
      (display "do/2\n")
      (pretty-display source)
      (pretty-display (do/translate source))
      (should-be 'do/2 (void) (start e)))) 

  (define (test/do/3)
    (let* ((source
            '(let ((x '(1 3 5 7 9)))
               (do ((x x (cdr x))
                    (sum 0 (+ sum (car x))))
                 ((null? x) sum)))) ; No commands in do body.
           (e (compile source)))
      (display "do/3\n")
      (pretty-display source)
      (should-be 'do/3 25 (start e))))
  
  (test/do/1)
  (test/do/2)
  (test/do/3))

(define (test/and)
  (define (test/and/1)
    (let ((e (compile '(and))))
      (should-be 'test/and/1 #t (start e))))
  
  (define (test/and/2)
    (let ((e (compile '(if (and) "good" "bad"))))
      (should-be 'test/and/2 "good" (start e))))
  
  (define (test/and/3)
    (let ((e (compile '((lambda (a b) (and (< a b)))
                        3 5))))
      (should-be 'test/and/3 #t (start e))))
  
  (define (test/and/4)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (and (element 1 3 5) (element 2 5 3) (element 3 7 8)))))) ; Only elements 1 and 2 should execute.
      (should-be 'test/and/4 #f (start e))))

  
  (define (test/and/5)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (and (element 1 3 5) (element 2 2 3) (element 3 11 17)))))) ; All elements, 1 thru 3, should execute.
      (should-be 'test/and/5 #t (start e))))

  (define (test/and/6)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (define (happy flag)
                   (if flag "happy" "unhappy"))
                 
                 (happy (and (element 1 3 5) (element 2 5 3) (element 3 7 8)))))))
      (should-be 'test/and/6 "unhappy" (start e))))  
  
  (test/and/1)
  (test/and/2)
  (test/and/3)
  (test/and/4)
  (test/and/5)
  (test/and/6))

(define (test/or)
  (define (test/or/1)
    (let ((e (compile '(or))))
      (should-be 'test/or/1 #f (start e))))
  
  (define (test/or/2)
    (let ((e (compile '(if (or) "bad" "good"))))
      (should-be 'test/or/2 "good" (start e))))
  
  (define (test/or/3)
    (let ((e (compile '((lambda (a b) (or (< a b)))
                        3 5))))
      (should-be 'test/or/3 #t (start e))))
  
  (define (test/or/4)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (or (element 1 5 3) (element 2 3 5) (element 3 7 8)))))) ; Only elements 1 and 2 should execute.
      (should-be 'test/or/4 #t (start e))))

  
  (define (test/or/5)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (or (element 1 5 3) (element 2 3 2) (element 3 11 17)))))) ; All elements, 1 thru 3, should execute.
      (should-be 'test/or/5 #t (start e))))

  (define (test/or/6)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (define (happy flag)
                   (if flag "happy" "unhappy"))
                 
                 (happy (or (element 1 5 3) (element 2 3 2) (element 3 7 8)))))))
      (should-be 'test/or/6 "happy" (start e))))  
  
  (test/or/1)
  (test/or/2)
  (test/or/3)
  (test/or/4)
  (test/or/5)
  (test/or/6))

(define (test/sort)
  (let ((e
         (compile
          '(let ()
             (define (sort-list obj pred)
               
               (define (loop l)
                 (if (and (pair? l) (pair? (cdr l)))
                     (split l '() '())
                     l))
               
               (define (split l one two)
                 (if (pair? l)
                     (split (cdr l) two (cons (car l) one))
                     (merge (loop one) (loop two))))
               
               (define (merge one two)
                 (cond
                   ((null? one) two)
                   ((pred (car two) (car one))
                    (cons (car two)
                          (merge (cdr two) one)))
                   (else
                    (cons (car one)
                          (merge (cdr one) two)))))
               
               (loop obj))
             
             (sort-list '("one" "two" "three" "four" "five" "six"
                                "seven" "eight" "nine" "ten" "eleven" "twelve")
                        string<?)))
         ))
    (should-be 'test/sort
               '("eight" "eleven" "five" "four" "nine" "one" "seven" "six" "ten" "three" "twelve" "two")
               (start e))))


(define (test/quasiquotation)
  (define (test/quasiquotation/1)
    (let ((e (compile '`(1 (+ 2 3) 17))))
      (should-be 'quasiquotation/1 '(1 (+ 2 3) 17) (start e))))

  (define (test/quasiquotation/2)
    (let ((e (compile '`(1 ,(+ 2 3) 17))))
      (should-be 'quasiquotation/2 '(1 5 17) (start e))))

  (define (test/quasiquotation/3)
    (let ((e (compile '`(1 ,@'(a b c) 17))))
      (should-be 'quasiquotation/3 '(1 a b c 17) (start e))))
  
  (define (test/quasiquotation/4)
    (let ((e (compile 
              '(let ((a 99)) `(1 ,a 17)))))
      (should-be 'quasiquotation/4 '(1 99 17) (start e))))

   (define (test/quasiquotation/5)
    (let ((e
           (compile '(let ((a 13)
                           (b 19))
                       `(front ,(+ a b) ,(list a b (* 2 a) (* 2 b)) rear)))))
      (should-be 'quasiquotation/5 '(front 32 (13 19 26 38) rear) (start e)))) 

  (define (test/quasiquotation/6)
    (let ((e
           (compile '(let ((a 13)
                           (b 19))
                       `(front ,(+ a b) ,@(list a b (* 2 a) (* 2 b)) rear)))))
      (should-be 'quasiquotation/6 '(front 32 13 19 26 38 rear) (start e))))

  (define (test/quasiquotation/7)
    (let ((e (compile '`(1 `,(+ 1 ,(+ 2 3)) 4))))
      (should-be 'quasiquotation/7 '(1 `,(+ 1 5)  4) (start e))))

  (define (test/quasiquotation/8)
    (let ((e (compile '`(1 ```,,@,,@(list (+ 1 2)) 4))))
      (should-be 'quasiquotation/8 '(1 ```,,@,3    4) (start e))))
  
  (test/quasiquotation/1)
  (test/quasiquotation/2)
  (test/quasiquotation/3)
  (test/quasiquotation/4)
  (test/quasiquotation/5)
  (test/quasiquotation/6)
  (test/quasiquotation/7)
  (test/quasiquotation/8))

(define (test/macro)
  (define (test/macro/1)
    (let ((e (compile
              '(let ()
                 (define-macro (eleven) 11)
                 (eleven)))))
      (pretty-display (decompile e))
      (should-be 'macro/1 11 (start e))))

  ; Repeated application of a single macro.
  (define (test/macro/2)
    (let ((e (compile
              '(let ()
                 (define-macro (eleven) 11)
                 (+ (eleven) (eleven))))))
      (pretty-display (decompile e))
      (should-be 'macro/2 22 (start e))))

  ; Repeated nested application of macros.
  (define (test/macro/3)
    (let ((e (compile
              '(let ()
                 (define-macro (eleven) '(ten+1))
                 (define-macro (ten+1)  '(add1 10))
                 (+ (eleven) (eleven))))))
      (pretty-display (decompile e))
      (should-be 'macro/3 22 (start e))))

  ; Lexical scoping of macros.
  (define (test/macro/4)
    (let ((e (compile
              '(let ()
                 (define-macro (eleven) '(ten+1))
                 (define-macro (ten+1)  '(add1 10))
                 (let ()
                   (define-macro (ten+1) '(+ 10 2))
                   (+ (eleven) (eleven)))))))
      (pretty-display (decompile e))
      (should-be 'macro/4 24 (start e))))
  
  ; Macro contains a local function.
  (define (test/macro/5)
    (let ((e (compile
              '(let ()
                 (define-macro (<let> bindings . body)
                   ; Local function inside macro definition body.
                   (define (unzip bindings variables values)
                     (if (null? bindings)
                         (cons variables values)
                         (let ((binding (car bindings)))
                           (unzip (cdr bindings)
                                  (cons (car binding) variables)
                                  (cons (cadr binding) values)))))

                   (let* ((unzipping (unzip bindings null null))
                          (variables (car unzipping))
                          (values    (cdr unzipping)))
                     `((lambda ,variables ,@body) ,@values)))
                 
                 (<let> ((a 3) (b 7)) (* a b)))))) ; This should be macro translated.
                 
      (pretty-display (decompile e))
      (should-be 'macro/5 21 (start e))))

  (test/macro/1)
  (test/macro/2)
  (test/macro/3)
  (test/macro/4)
  (test/macro/5))

(define (test/call/cc)
  (define (test/call/cc/1)
    (let ((e (compile
              '(call/cc
                (lambda (k)
                  (* 5 4))))))
      (should-be 'call/cc/1 20 (start e))))
  
  (define (test/call/cc/2)
    (let ((e (compile
              '(call/cc
                (lambda (k)
                  (* 5 (k 4)))))))
      (should-be 'call/cc/2 4 (start e))))

  (define (test/call/cc/3)
    (let ((e (compile
              '(+ 2
                  (call/cc
                   (lambda (k)
                     (* 5 (k 4))))))))
      (should-be 'call/cc/3 6 (start e))))

  (define (test/call/cc/4)
    (let ((e (compile
              '(let ((product
                      (lambda (ls)
                        (call/cc
                         (lambda (break)
                           (let f ([ls ls])
                             (cond
                               [(null? ls) 1]
                               [(= (car ls) 0) (break 999)]
                               [else (* (car ls) (f (cdr ls)))])))))))
                 (list
                  (product '(1 2 3 4 5))
                  (product '(7 3 8 0 1 9 5)))))))
      (should-be 'call/cc/4 '(120 999) (start e))))

  (define (test/call/cc/5)
    (let ((e (compile
              '(let ([x (call/cc (lambda (k) k))])
                 (x (lambda (ignore) "hi"))))))
      (should-be 'call/cc/5 "hi" (start e))))
    

(define (test/call/cc/6a)
  (let ((e (compile
            '(let ((saved (box #f))
                   (f (lambda (a b c) (+ a b c)))) ; Trivial function.

               (display
                (format "\tcall/cc/6a: ~a\n"
                        (f 3 
                           (call/cc (lambda (k) (set-box! saved k) 7)) ; Continuation capture in argument of f.
                           11)))

               (let ((k (unbox saved)))
                 (when k
                   (set-box! saved #f)
                   (k 11)))))))
    (display "call/cc/6a: output should be 21 25\n")
    (start e)))


  (define (test/call/cc/6b)
    (let ((e (compile
              '(let ((saved (box #f)))
                 (let ()
                   (define (factorial n)
                     (if (= n 0)
                         (call/cc
                          (lambda (k) (set-box! saved k) 1))
                         (* n (factorial (sub1 n)))))

                   (display (format "\tcall/cc/6b: ~a\n" (factorial 4))))
                 
                 (let ((k (unbox saved)))
                   (when k
                     (set-box! saved #f)
                     (k 2)))))))

      (display "call/cc/6b: output should be 24 48\n")
      (start e)))

  (define (test/call/cc/7)
    (let ((e (compile
              '(let ((saved (box #f))
                     (boxes (vector #f #f)))
                 (letrec ((x (box (call/cc (lambda (c) (set-box! saved c) 0))))
                          (y (box (call/cc (lambda (c) (set-box! saved c) 0)))))
                   (if (unbox saved)
                       (let ((k (unbox saved)))
                         (set-box! saved #f)
                         (vector-set! boxes 0 x)
                         (vector-set! boxes 1 y)
                         (set-box! x 1)
                         (set-box! y 1)
                         (k 0))
                       (begin
                         (display (format "x: ~a y: ~a\n"
                                          (eq? x (vector-ref boxes 0))
                                          (eq? y (vector-ref boxes 1))))
                         (+ (unbox x) (unbox y)))))))))
      (pretty-display (decompile e))
      (should-be 'call/cc/7 1 (start e))))
  
  
  ;; This test is the brainchild of Al Petrofsky (May 20, 2001 in comp.lang.scheme).
  ;; In an implementation which evaluates the letrec initializers prior to the assignments
  ;; (as required by R5RS) it returns #t. In letrec implementations in which initializer evaluation
  ;; is intermingled with assignment it returns #f.
  (define (test/call/cc/8)
    (let ((e (compile
              '(letrec ((x (call/cc list)) (y (call/cc list)))
                 (cond ((procedure? x) (x (pair? y)))
                       ((procedure? y) (y (pair? x))))
                 (let ((x (car x)) (y (car y)))
                   (and (call/cc x) (call/cc y) (call/cc x)))))))
      (should-be 'call/cc/8 #t (start e))))

  (test/call/cc/1)
  (test/call/cc/2)
  (test/call/cc/3)
  (test/call/cc/4)
  (test/call/cc/5)
  (test/call/cc/6a)
  (test/call/cc/6b)
  (test/call/cc/7)
  (test/call/cc/8))

(define (test/higher)
  (define (test/higher/map)
    (let ((e (compile
              '(let () (map (lambda (x) (* 3 x)) '(0 1 2 3 4 5))))))
      (should-be 'higher/map/1 '(0 3 6 9 12 15) (start e))))

  (define (test/higher/apply/1)
    (let ((e (compile
              '(let () (apply + 1 2 3 4 5)))))
      (should-be 'higher/apply/1 15 (start e))))

  (define (test/higher/apply/2)
    (let ((e (compile
              '(let () (apply (lambda (x y) (+ x y 13)) 4 5)))))
      (should-be 'higher/apply/2 22 (start e))))

  (define (test/higher/for-each)
    (let ((e (compile
              '(let () (for-each (lambda (x) (display (add1 x)) (display "\n")) '(0 1 2 3 4))))))
      (start e)))

  (test/higher/map)
  (test/higher/apply/1)
  (test/higher/apply/2)
  (test/higher/for-each))
        
(define (test/scope)
  (let ((e (compile
            '(let ((f cons)) (f 19 22)))))
    (pretty-display (mischief/decompile e))))

(define (test/all)
  (test/constants)
  (test/base)
  (test/lambda)
  (test/let)
  ;(test/setter)
  (test/letrec)
  (test/let*)
  (test/named-let)
  (test/define)
  (test/begin)
  (test/when)
  (test/unless)
  (test/cond)
  (test/case)
  (test/do)
  (test/and)
  (test/or)
  (test/sort)
  (test/quasiquotation)
  (test/macro))

;; A few tests for the integration of persistent vectors into Mischief.
(define (test/vector)
  (define (test/list/vector)
    (let ((e (compile
              '(let ((v (list/vector vector/null '(2 4 6 8 10))))
                 (vector/list v)))))
      (should-be 'list/vector '(2 4 6 8 10) (mischief/start e))))

  (define (test/vector/build)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
                 (vector/list v)))))
      (should-be 'vector/build '(1 2 3 4 5 6 7) (mischief/start e))))

  (define (test/vector/fold/left)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
                 (vector/fold/left v (lambda (x seed) (cons x seed)) null)))))
      (should-be 'vector/fold/left '(7 6 5 4 3 2 1) (mischief/start e))))

  (define (test/vector/fold/right)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
                 (vector/fold/right v (lambda (x seed) (cons x seed)) null)))))
      (should-be 'vector/fold/right '(1 2 3 4 5 6 7) (mischief/start e))))    

  (define (test/vepersist?)
    (let ((e (compile
              '(let ((v (list/vector vector/null '(2 4 6 8))))
                 (list (vepersist? v) (vepersist? (vector/list v)))))))
      (should-be 'vector/is? '(#t #f) (mischief/start e))))

  (define (test/vector/length)
    (let ((e (compile
              '(let* ((a (list/vector vector/null '(1 2 3)))
                      (b (list/vector a           '(4 5)))
                      (c (list/vector b           '(6 7 8 9))))
                 (list (vector/length vector/null) (vector/length a) (vector/length b) (vector/length c))))))
      (should-be 'vector/length '(0 3 5 9) (mischief/start e))))


  (define (test/vector/null?)
    (let ((e (compile
              '(let* ((a (list/vector vector/null '(1 2 3)))
                      (b (list/vector a           '(4 5)))
                      (c (list/vector b           '(6 7 8 9))))
                 (list (vector/null? vector/null) (vector/null? a) (vector/null? b) (vector/null? c))))))
      (should-be 'vector/length '(#t #f #f #f) (mischief/start e))))   

  (define (test/vector/cons)
    (let ((e (compile
              '(let* ((a (vector/cons vector/null 33))
                      (b (vector/cons a 44))
                      (c (vector/cons b 55)))
                 (vector/list c)))))
      (should-be 'vector/cons '(33 44 55) (mischief/start e))))

  (define (test/vector/cdr)
    (let ((e (compile
              '(let* ((a (list/vector vector/null '(33 44)))
                      (b (vector/cdr a))
                      (c (vector/cdr b)))
                 (vector/null? c)))))
      (should-be 'vector/cdr #t (mischief/start e))))

  (define (test/vector/filter)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
                 (vector/list (vector/filter v (lambda (x) (odd? x))))))))
      (should-be 'vector/filter '(1 3 5 7) (mischief/start e))))     

  (define (test/vector/map)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
                 (vector/list (vector/map v (lambda (x) (* 2 x))))))))
      (should-be 'vector/map '(2 4 6 8 10 12 14) (mischief/start e))))

  (define (test/vector/ref)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) i))))
                 (+ (vector/ref v 1) (vector/ref v 3) (vector/ref v 6))))))
      (should-be 'vector/ref 10 (mischief/start e))))
  
  (define (test/vector/subvector)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) i))))
                 (list (vector/list (vector/subvector v 3)) (vector/list (vector/subvector v 0 3)))))))
      (should-be 'vector/subvector '((3 4 5 6) (0 1 2)) (mischief/start e))))
      
  (define (test/vector/update)
    (let ((e (compile
              '(let* ((a (vector/build 5 (lambda (i) i)))
                      (b (vector/update a 3 33))
                      (c (vector/update b 4 44)))
                 (list (vector/list a) (vector/list b) (vector/list c))))))
      (should-be 'vector/subvector '((0 1 2 3 4) (0 1 2 33 4) (0 1 2 33 44)) (mischief/start e))))


  (test/list/vector)
  (test/vector/build)
  (test/vector/fold/left)
  (test/vector/fold/right)
  (test/vepersist?)
  (test/vector/length)
  (test/vector/null?)
  (test/vector/cons)
  (test/vector/cdr)
  (test/vector/filter)
  (test/vector/map)
  (test/vector/ref)
  (test/vector/subvector)
  (test/vector/update))

;; A suite of tests for Mischief persistent hash tables.
(define (test/hash)
  (define (test/hash/pairs)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
                 (sort (hash/pairs h/26) less?)))))
      
      (should-be 'hash/pairs
                 '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8) (i . 9) (j . 10)
                   (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                   (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))
                 (mischief/start e))))

  (define (test/hash/remove)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
                 ; Remove the vowels.
                 (let loop ((h h/26) (vowels '(a e i o u)))
                   (if (null? vowels)
                       (sort (hash/pairs h) less?) ; Sorted pairs but without the vowels.
                       (loop (hash/remove h (car vowels)) (cdr vowels))))))))

          (should-be 'hash/remove
                     '((b . 2) (c . 3) (d . 4) (f . 6) (g . 7) (h . 8) (j . 10)
                           (k . 11) (l . 12) (m . 13) (n . 14) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                           (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))
                     (mischief/start e))))

  (define (test/hash/cons)
    (let ((e (mischief/compile
              '(let ((h/21 ; The alphabet without the vowels.
                      (pairs/hash
                       hash/eq/null
                       '((b . 2) (c . 3) (d . 4) (f . 6) (g . 7) (h . 8) (j . 10)
                         (k . 11) (l . 12) (m . 13) (n . 14) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                         (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
                 ; Add the vowels.
                 (let loop ((h h/21) (vowels '(a e i o u)))
                   (if (null? vowels)
                       (sort (hash/pairs h) less?)
                       (loop (hash/cons h (car vowels) #t) (cdr vowels))))))))
          (should-be 'hash/cons
                     '((a . #t) (b . 2) (c . 3) (d . 4) (e . #t) (f . 6) (g . 7) (h . 8) (i . #t) (j . 10)
                       (k . 11) (l . 12) (m . 13) (n . 14) (o . #t) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                       (u . #t) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))
                     (mischief/start e))))
                   


  (define (test/hash/merge)
    (let ((e (mischief/compile
              '(let ((h/21 ; The alphabet without the vowels.
                      (pairs/hash
                       hash/eq/null
                       '((b . 2) (c . 3) (d . 4) (f . 6) (g . 7) (h . 8) (j . 10)
                         (k . 11) (l . 12) (m . 13) (n . 14) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                         (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))
                     (h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
                 (sort (hash/pairs (hash/merge h/21 h/vowels)) less?)))))

          (should-be 'hash/merge
                     '((a . #t) (b . 2) (c . 3) (d . 4) (e . #t) (f . 6) (g . 7) (h . 8) (i . #t) (j . 10)
                       (k . 11) (l . 12) (m . 13) (n . 14) (o . #t) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                       (u . #t) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))
                     (mischief/start e))))

  (define (test/hash/keys)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 (sort (hash/keys h/26) less?)))))
      (should-be 'hash/keys '(a b c d e f g h i j k l m n o p q r s t u v w x y z) (mischief/start e))))


  ; Reference every key/value pair in a hash table.
  (define (test/hash/ref)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26))))
                 (let loop ((alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
                            (values null))
                   (if (null? alphabet)
                       (reverse values)
                       (loop (cdr alphabet) (cons (hash/ref h/26 (car alphabet) #f) values))))))))
      (should-be 'hash/ref '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26) (mischief/start e))))


  ; Using hash/car and hash/cdr desconstruct a hash table.
  (define (test/hash/car)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
                 (let loop ((h h/26) (pairs null))
                   (if (hash/empty? h)
                       (sort pairs less?)
                       (loop (hash/cdr h) (cons (hash/car h) pairs))))))))
      (should-be 'hash/car
                 '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8) (i . 9) (j . 10)
                   (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                   (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))
                 (mischief/start e))))
                 
  (define (test/hash/length)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t))
                     (h/1 (hash/new hash/eq/null 'foo 'bar)))
                 (list (hash/length (hash/cdr h/1)) (hash/length h/1) (hash/length h/vowels) (hash/length h/26))))))
      (should-be 'hash/length '(0 1 5 26) (mischief/start e))))

  (define (test/hash/empty?)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t))
                     (h/1 (hash/new hash/eq/null 'foo 'bar)))
                 (list (hash/empty? (hash/cdr h/1)) (hash/empty? h/1) (hash/empty? h/vowels) (hash/empty? h/26))))))
      (should-be 'hash/empty? '(#t #f #f #f) (mischief/start e))))

  (define (test/hash/contains?)
    (let ((e (mischief/compile
              '(let ((h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t)))
                 (list
                  (hash/contains? h/vowels 'a)
                  (hash/contains? h/vowels 'e)
                  (hash/contains? h/vowels 'i)
                  (hash/contains? h/vowels 'o)
                  (hash/contains? h/vowels 'u)
                  (hash/contains? h/vowels 'z))))))
      (should-be 'hash/contains? '(#t #t #t #t #t #f) (mischief/start e))))

  (define (test/hash/fold)
        (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26))))
                 (hash/fold
                  h/26
                  (lambda (pair seed)
                    (if (< (cdr pair) seed)
                        (cdr pair)
                        seed))
                  9999)))))
          (should-be 'hash/fold 1 (mischief/start e))))

  (define (test/hash/map)
    (let ((e (mischief/compile
              '(let* ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                      (map (hash/map
                            h/26
                            (lambda (pair)
                              (cons
                               (string-append (symbol->string (car pair)) "." (number->string (cdr pair)))
                               #t)))))
                 (sort (hash/keys map) string<?)))))
      (should-be 'hash/map 
                 '("a.1" "b.2" "c.3" "d.4" "e.5" "f.6" "g.7" "h.8" "i.9" "j.10"
                   "k.11" "l.12" "m.13" "n.14" "o.15" "p.16" "q.17" "r.18" "s.19" "t.20"
                   "u.21" "v.22" "w.23" "x.24" "y.25" "z.26")
                 (mischief/start e))))
  

  (define (test/hash/filter)
    (let ((e (mischief/compile
              '(let* ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                      (h/odd (hash/filter h/26 (lambda (pair) (odd? (cdr pair)))))
                      (less? (lambda (alpha beta) (< (cdr alpha) (cdr beta)))))
                 (sort (hash/pairs h/odd) less?)))))
      (should-be 'hash/filter
                 '((a . 1) (c . 3) (e . 5)  (g . 7) (i . 9)
                   (k . 11) (m . 13) (o . 15) (q . 17) (s . 19)
                   (u . 21) (w . 23) (y . 25))
                 (mischief/start e))))
  
  (define (test/hash/partition)
    (let ((e (mischief/compile
              '(let* ((h/26
                       (list/hash
                        hash/eq/null
                        '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                            k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                            u 21 v 22 w 23 x 24 y 25 z 26)))
                      (partition (hash/partition h/26 (lambda (pair) (odd? (cdr pair)))))
                      (less? (lambda (alpha beta) (< (cdr alpha) (cdr beta))))
                      (odd  (sort (hash/pairs (car partition)) less?))
                      (even (sort (hash/pairs (cdr partition)) less?)))
                 (list odd even)))))
      (should-be 'hash/partition
                 '(((a . 1) (c . 3) (e . 5)  (g . 7) (i . 9)
                            (k . 11) (m . 13) (o . 15) (q . 17) (s . 19)
                            (u . 21) (w . 23) (y . 25))
                   
                   ((b . 2) (d . 4) (f . 6) (h . 8) (j . 10)
                            (l . 12) (n . 14) (p . 16) (r . 18) (t . 20)
                            (v . 22) (x . 24) (z . 26)))
                 (mischief/start e))))

  (test/hash/pairs)
  (test/hash/remove)
  (test/hash/cons)
  (test/hash/merge)
  (test/hash/keys)
  (test/hash/ref)
  (test/hash/car)
  (test/hash/length)
  (test/hash/empty?)
  (test/hash/contains?)
  (test/hash/fold)
  (test/hash/map)
  (test/hash/filter)
  (test/hash/partition))

(define (test/set)
  (define (test/set/new)
    (let ((e (mischief/compile
              '(let ((s/alphabet
                      (set/new set/eq/null 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 (sort (set/list s/alphabet) less?)))))
      
      (should-be 'set/new
                 '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
                 (mischief/start e))))
  
  (define (test/set/remove)
    (let ((e (mischief/compile
              '(let ((s/alphabet
                      (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 ; Remove the vowels.
                 (let loop ((s s/alphabet) (vowels '(a e i o u)))
                   (if (null? vowels)
                       (sort (set/list s) less?) ; Sorted alphabet but without the vowels.
                       (loop (set/remove s (car vowels)) (cdr vowels))))))))

          (should-be 'set/remove
                     '(b c d f g h j k l m n p q r s t v w x y z)
                     (mischief/start e))))
  
  
  (define (test/set/cons)
    (let ((e (mischief/compile
              '(let ((consonants
                      (list/set set/eq/null '(b c d f g h j k l m n p q r s t v w x y z)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 ; Add the vowels.
                 (let loop ((s consonants) (vowels '(a e i o u)))
                   (if (null? vowels)
                       (sort (set/list s) less?) ; Sorted complete alphabet.
                       (loop (set/cons s (car vowels)) (cdr vowels))))))))

          (should-be 'set/cons
                     '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
                     (mischief/start e))))    

 (define (test/set/union)
    (let ((e (mischief/compile
              '(let ((consonants
                      (list/set set/eq/null '(b c d f g h j k l m n p q r s t v w x y z)))
                     (vowels (list/set set/eq/null '(a e i o u)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 (sort (set/list (set/union consonants vowels)) less?)))))

          (should-be 'set/union
                     '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
                     (mischief/start e))))

 (define (test/set/intersection)
    (let ((e (mischief/compile
              '(let ((alphabet
                      (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                     (other (list/set set/eq/null '(a 1 e 5 i 9 o 15 u 21)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 (sort (set/list (set/intersection alphabet other)) less?)))))

          (should-be 'set/intersection '(a e i o u) (mischief/start e))))

 (define (test/set/difference)
    (let ((e (mischief/compile
              '(let ((alphabet
                      (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                     (other (list/set set/eq/null '(a 1 e 5 i 9 o 15 u 21)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 (sort (set/list (set/difference alphabet other)) less?)))))

          (should-be 'set/difference '(b c d f g h j k l m n p q r s t v w x y z) (mischief/start e))))

  (define (test/set/contains?)
    (let ((e (mischief/compile
              '(let* ((alphabet
                      (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                      (other '(a 1 e 5 i 9 o 15 u 21))
                      (outside ; Collect all members of other not appearing in alphabet (namely, the digits).
                       (let loop ((other other) (contains null))
                         (if (null? other)
                             contains
                             (if (not (set/contains? alphabet (car other)))
                                 (loop (cdr other) (cons (car other) contains))
                                 (loop (cdr other) contains))))))
                 (sort outside <)))))
      (should-be 'set/contains? '(1 5 9 15 21) (mischief/start e))))

  (define (test/set/car)
    (let ((e (mischief/compile
              '(let loop ((digits (list/set set/eq/null '(1 5 9 15 21)))
                          (outcome null))
                 (if (set/empty? digits)
                     (sort outcome <)
                     (loop (set/cdr digits) (cons (set/car digits) outcome)))))))
      (should-be 'set/car '(1 5 9 15 21) (mischief/start e))))

  (define (test/set/length)
    (let ((e (mischief/compile
              '(let ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                     (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
                     (empty    (set/new set/equal/null)))
                 (list (set/length alphabet) (set/length digits) (set/length empty))))))
      (should-be 'set/length '(26 10 0) (mischief/start e))))
  
  (define (test/set/subset?)
    (let ((e (mischief/compile
              '(let ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                     (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
                     (empty    (set/new set/equal/null))
                     (vowels   (list/set set/eq/null '(a e i o u)))
                     (mixed    (list/set set/eq/null '(a e 7))))
                 (list
                  (set/subset? alphabet set/eq/null) ; The empty set is a subset of every set.
                  (set/subset? alphabet digits)      ; Nope.
                  (set/subset? alphabet vowels)      ; Yup.
                  (set/subset? alphabet mixed))))))  ; Nope.
      (should-be 'set/subset? '(#t #f #t #f) (mischief/start e))))


  (define (test/set/fold)
    (let ((e (mischief/compile
               '(let* ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                       (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
                       (empty    (set/new set/equal/null))
                       (vowels   (list/set set/eq/null '(a e i o u)))
                       (mixed    (list/set set/eq/null '(a e 7)))
                       (all      (set/new set/equal/null alphabet digits empty vowels mixed)))
                  ; Sum the lengths of all sets whose cardinality > 3.
                  (set/fold
                   all
                   (lambda (x seed)
                     (if (> (set/length x) 3)
                         (+ (set/length x) seed)
                         seed))
                   0)))))
      (should-be 'set/fold 41 (mischief/start e))))
                  

  (define (test/set/map)
    (let ((e (mischief/compile
               '(let* ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                       (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
                       (empty    (set/new set/equal/null))
                       (vowels   (list/set set/eq/null '(a e i o u)))
                       (mixed    (list/set set/eq/null '(a e 7)))
                       (colors   (list/set set/eq/null '(red white blue)))
                       (all      (set/new set/equal/null alphabet digits empty vowels mixed colors)))
                  
                  (sort
                   (set/list
                    ; Map all into the set lengths of its members.
                    (set/map all (lambda (x) (set/length x))))
                   <)))))
      (should-be 'set/map '(0 3 5 10 26) (mischief/start e))))

  (define (test/set/filter)
    (let ((e (mischief/compile
              '(let ((s (set/new set/eq/null 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26)))
                 (sort (set/list (set/filter s odd?)) <)))))
      (should-be 'set/filter '(1 3 5 7 9 11 13 15 17 19 21 23 25) (mischief/start e))))

  (define (test/set/partition)
    (let ((e (mischief/compile
              '(let* ((all  (set/new set/eq/null 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
                      (odd  (set/filter all odd?))
                      (even (set/filter all even?))
                      (both (set/partition all odd?))) ; Should be (odd . even).
                 (list
                  (set/empty? (set/intersection odd even))
                  (set/subset? (car both) odd)
                  (set/subset? odd (car both))
                  (set/subset? (cdr both) even)
                  (set/subset? even (cdr both)))))))
      (should-be 'set/partition '(#t #t #t #t #t) (mischief/start e))))
  
  (test/set/new)
  (test/set/remove)
  (test/set/cons)
  (test/set/union)
  (test/set/intersection)
  (test/set/difference)
  (test/set/contains?)
  (test/set/car)
  (test/set/length)
  (test/set/subset?)
  (test/set/fold)
  (test/set/map)
  (test/set/filter)
  (test/set/partition))

(define (test/tuple)
  (define (test/tuple/tuple)
    (let ((e (mischief/compile
              '(let ((t (tuple 'a 'b 'c 'd 'e)))
                 (tuple/list t)))))
      (should-be 'tuple '(a b c d e) (mischief/start e))))


  (define (test/tuple/build)
    (let ((e (mischief/compile
              '(let ((t (tuple/build 5 (lambda (i) (* i 3)))))
                 (tuple/list t)))))
      (should-be 'tuple/build '(0 3 6 9 12) (mischief/start e))))

  (define (test/tuple/filter)
    (let ((e (mischief/compile '(tuple/list (tuple/filter (tuple 1 2 3 4 5)  (lambda (x) (even? x)))))))
      (should-be 'tuple/filter '(2 4) (mischief/start e))))

  (define (test/tuple/map)
    (let ((e (mischief/compile '(tuple/list (tuple/map (tuple 1 2 3 4 5) (lambda (n) (1- n)))))))
      (should-be 'test/tuple/map '(0 1 2 3 4) (mischief/start e))))

  (define (test/tuple/partition)
    (let ((e (mischief/compile
              '(let ((p (tuple/partition (tuple 1 2 3 4 5) (lambda (n) (even? n)))))
                 (cons (tuple/list (car p)) (tuple/list (cdr p)))))))
      (should-be 'test/tuple/partition '((2 4) 1 3 5) (mischief/start e))))

  (test/tuple/tuple)
  (test/tuple/build)
  (test/tuple/filter)
  (test/tuple/map)
  (test/tuple/partition))

(define (test/box)
  (let ((e (mischief/compile
            '(let* ((a (box 99))
                    (b (box "foobar"))
                    (c (string=? (unbox b) "foobar")))
               (box! b "zzz")
               (list (box? a) (unbox a) c (unbox b))))))
    (should-be 'test/box '(#t 99 #t "zzz") (mischief/start e))))
               
(define (test/reglobal)

  (let ((e (mischief/compile
            '(let ((x (lambda () foobar)))
               x)))
        (alpha (make-immutable-hasheq '((foobar . alpha))))
        (beta  (make-immutable-hasheq '((foobar . beta)))))
    (pretty-display (mischief/decompile e))
    (display alpha) (display "\n")
    (display beta) (display "\n")
    (let ((a (e rtk/RETURN (vector #f alpha)))
          (b (e rtk/RETURN (vector #f beta))))
      (display (eq? a b)) (display "\n")
      (display (a rtk/RETURN))
      (display "\n")
      (display (b rtk/RETURN))
      (thread
       (lambda ()
         (let loop ()
           (display "a:")
           (display (a rtk/RETURN))
           (display "\n")
           (sleep 0.2)
           (loop))))
      
      (thread
       (lambda ()
         (let loop ()
           (display "b:")
           (display (b rtk/RETURN))
           (display "\n")
           (sleep 0.2)
           (loop)))))))

(define (test/environ)

  (define (test/environ/1)
    (let* ((e (mischief/compile '(lambda () (random))))
           (E (environ/cons ENVIRON/TEST 'random (lambda (k _rte _global) (k (random 1000)))))
           (f (motile/start e E)))
      
      (display 'test/environ/1) (newline)
      (pretty-display (motile/decompile e)) (newline) (newline)
      (pretty-display (motile/decompile f)) (newline) (newline)
      (display (motile/start f E)) (newline)))
  
  (define (test/environ/2)
    (let ((e (mischief/compile
               '((lambda (x) (random x)) 17)))
          (E (environ/cons ENVIRON/TEST 'random (lambda (k _rte _global x) (k (random x))))))
      (display 'test/environ/2) (newline)
      (display (motile/decompile e)) (newline) (newline)
      (display (motile/start e E)) (newline)))
      
  (define (test/environ/3)
    (let ((e (mischief/compile
             '((lambda (n) (add1 n)) 33))))
      (display 'test/environ/3) (newline)
      (display (motile/decompile e)) (newline) (newline)
      (display (motile/start e ENVIRON/TEST)) (newline)))

  (test/environ/1)
  (test/environ/2)
  (test/environ/3))


;; There is a serious bug in the compiler with regard to nested defines (and hence nested letrecs) that is illustrated
;; by some of the tests in test/xdefine.
;; I've decided to let it go for now (2011.06.23) since it is possible to write around it by avoiding nested defines
;; but intend to come back to it when time permits.
(define (test/xdefine)
  (define (test/xdefine/1)
    (let ((f (motile/compile
              '((lambda ()
                  (define (fact n) (if (= n 1) 1 (* n (fact (sub1 n)))))
                  (fact 5))))))
      (display "test/xdefine/1\n")
      (display (motile/start f ENVIRON/TEST))))
  
  ;; Hand compilation of the nested define's in a lambda body.
  (define (test/xdefine/2a)
    (let ((f (motile/compile
              '((lambda (a b)
                  (letrec ((a* (lambda () (* a (b-))))
                           (b- (lambda () (sub1 b))))
                    (a*)))
                8 -1))))
      (should-be 'test/xdefine/2a -16 (motile/start f ENVIRON/TEST))))
  
  (define (test/xdefine/2b)
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a*) (* a (b-)))
                  (define (b-) (sub1 b))
                  (a*))
                8 -1))))
      (should-be 'test/xdefine/2b -16 (motile/start f ENVIRON/TEST))))

  
  ; !!! This test exposes a bug in the code generation for nested defines. !!!
  (define (test/xdefine/2d.1) 
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a*)
                    (define (b-) -2)
                    (* a (b-)))
                  (a*))
                8 -1))))
      (should-be 'test/xdefine/2d.1 -16 (motile/start f ENVIRON/TEST))))
  

  ; !!! This test fails as well---returning a compiler-generated closure---indicating a problem with stack accounting
  ; and address generation for closed variables.
  (define (test/xdefine/2d.2) 
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a*)
                    (define (b-) -2)
                    a)
                  (a*))
                8 -1))))
      (should-be 'test/xdefine/2d.2 8 (motile/start f ENVIRON/TEST))))
  
  ; !!! This test fails catastrophically with an exception in stack addressing.
  (define (test/xdefine/2d.3) 
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a*)
                    (define (b-) -2)
                    b)
                  (a*))
                8 -1))))
      (should-be 'test/xdefine/2d.3 -1 (motile/start f ENVIRON/TEST))))

  
  ; This test is a hand-compilation of test 2d above into the letrec representation used by the compiler.
  ; It fails in exactly the same way as test 2d.
  ; There is an error in the stack accounting and address generation.
  (define (test/xdefine/2c)
    (let ((f (motile/compile
              '((lambda (a b)
                  (letrec
                      ((a* (lambda ()
                             (letrec ((b- (lambda () (sub1 b))))
                               (* a (b-))))))
                    (a*)))
                8 -1))))
      (should-be 'test/xdefine/2c -16 (motile/start f ENVIRON/TEST))))
    
  (test/xdefine/1)
  (test/xdefine/2a)
  (test/xdefine/2b)
  (test/xdefine/2d.2)
  (test/xdefine/2d.3))

