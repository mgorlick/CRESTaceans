#lang racket/base

(require rackunit
         rackunit/text-ui)

(require "../free.rkt")
(require/expose "../free.rkt"
                (lambda/variables/closed))

;; Helper for defining a lexical stack for testing purposes.
(define (lexical/new outlines)
  (let loop ((outlines outlines) (lexical #f))
    (if (null? outlines)
        lexical
        (loop (cdr outlines) (lexical/push/parameters lexical (car outlines))))))

(define lexical (lexical/new '((u) (x a y) () (d) (e f))))
(define-test-suite lexical-address-tests
  (test-case "Lexical address check 1"
             (check-equal? '(0 . 0) (lexical/address lexical 'e)))
  (test-case "Lexical address check 2" 
             (check-equal? '(2 . 1) (lexical/address lexical 'a)))
  (test-case "Lexical address check 3"
             (check-equal? '(2 . 2) (lexical/address lexical 'y)))
  (test-case "Lexical address check 4" 
             (check-equal? '(3 . 0) (lexical/address lexical 'u)))
  (test-case "Lexical address check 5"
             (check-exn exn? (λ () (lexical/address lexical 'z)))) ; No variable z in lexical stack.
  )


(define-test-suite closed-variable-tests  
  (test-case "Closed variable check 1"
             (check-equal? '() (lambda/variables/closed '(lambda () a) #f)))
  
  (test-case "Closed variable check 2" 
             (check-equal? '(x y)
                           ; (lambda (x y) (lambda (a) (+ a y x)))
                           (lambda/variables/closed
                            '(lambda (a) (+ a y x))
                            (lexical/new '((x y))))))
  
  (test-case "Closed variable check 3"
             (check-equal?'(x y)
                          ; (lambda (x y)
                          ;   (lambda (a) (+ a y) (* x a a)))
                          (lambda/variables/closed
                           '(lambda (a) (+ a y) (* x a a))
                           (lexical/new '((x y))))))
  
  (test-case "Closed variable check 4"
             (check-equal? '(a x y)
                           ; (lambda (x a y)
                           ;   (lambda ()
                           ;     (+ a y)
                           ;     (* x 33 a)))
                           (lambda/variables/closed
                            '(lambda ()
                               (+ a y)
                               (* x 33 a))
                            (lexical/new '((x a y))))))
  
  (test-case "Closed variable check 5"
             (check-equal? '(a u x y)
                           (lambda/variables/closed
                            '(lambda ()
                               (if y (+ a x) (* u 19)))
                            (lexical/new '((x a y) (u))))))
  
  (test-case "Closed variable check 6"
             (check-equal? '(a u x y)
                           (lambda/variables/closed
                            '(lambda ()
                               (when (> y 0) (+ a x) (* u 19)))
                            (lexical/new '((x a y) (u))))))
  
  (test-case "Closed variable check 7"
             (check-equal? '(a u x y)
                           (lambda/variables/closed 
                            '(lambda ()
                               (unless (> y 0)
                                 (if (< u 19) #t x)
                                 (* a x x)))
                            (lexical/new '((x a y) (u))))))
  
  
  (test-case "Closed variable check 8"
             (check-equal? '(a e f u x y)
                           ; (lambda (u)
                           ;  (lambda (x a y)
                           ;    (lambda (d)
                           ;      (lambda (e f)
                           ;        (lambda ()
                           ;          (and (> u 19) (= x a) f (or (= y 0) (< y e))))))))    
                           (lambda/variables/closed
                            '(lambda ()
                               (and (> u 19) (= x a) f (or (= y 0) (< y e))))
                            (lexical/new '((u) (x a y) (d) (e f))))))
  
  (test-case "Closed variable check 9"
             (check-equal? '(a e f u x y)
                           ; (lambda (u)
                           ;   (lambda (x a y)
                           ;     (lambda ()
                           ;      (lambda (d)
                           ;        (lambda (e f)
                           ;          (lambda ()
                           ;            (and (> u 19) (= x a) f (or (= y 0) (< y e)))))))))   
                           (lambda/variables/closed
                            '(lambda ()
                               (and (> u 19) (= x a) f (or (= y 0) (< y e))))
                            (lexical/new '((u) (x a y) (d) (e f))))))
  
  (test-case "Closed variable check 10" 
             (check-equal? '(a f)
                           ; (lambda (u)
                           ;  (lambda (x a y)
                           ;    (lambda (d)
                           ;      (lambda (e f)
                           ;        (lambda ()
                           ;          `(u ,f d ,a))))))
                           (lambda/variables/closed
                            '(lambda ()
                               `(u ,f d ,a))
                            (lexical/new '((u) (x a y) (d) (e f))))))
  
  (test-case "Closed variable check 11"
             (check-equal? '(d e f u y)
                           
                           ; (lambda (u)
                           ;  (lambda (x a y)
                           ;    (lambda (d)
                           ;      (lambda (e f)
                           ;        (lambda ()
                           ;          `(u ,f ,@(list (+ d e) (> y u))))))))          
                           (lambda/variables/closed
                            '(lambda ()
                               å`(u ,f ,@(list (+ d e) (> y u))))
                            (lexical/new '((u) (x a y) (d) (e f))))))
  
  ;  (let ((u 1) (x 'xx) (a 'aa) (y 33) (d '7) (e 8) (f 'ff))
  ;    `(u ,f ,@(list `(+ ,d ,e) (> y u))))
  ;  =>
  ;  (u ff (+ 7 8) #t)
  (test-case "Closed variable check 12"
             (check-equal? '(d e f u y)
     
     ; (lambda (u)
     ;  (lambda (x a y)
     ;    (lambda (d)
     ;      (lambda (e f)
     ;        (lambda ()
     ;          `(u ,f ,@(list `(+ ,d ,e) (> y u))))))))     
     (lambda/variables/closed
      '(lambda ()
         `(u ,f ,@(list `(+ ,d ,e) (> y u))))
      (lexical/new '((u) (x a y) (d) (e f))))))
  
  ;  (let ((u 1) (x 'xx) (a 'aa) (y 33) (d '7) (e 8) (f 'ff))
  ;    `(u ,f ,@(list ``(+ ,d ,e) (> y u))))
  ;  =>
  ;  (u ff `(+ ,d ,e) #t)
  (test-case "Closed variable check 13"
             (check-equal? '(f u y)
     
     ; (lambda (u)
     ;  (lambda (x a y)
     ;    (lambda (d)
     ;      (lambda (e f)
     ;        (lambda ()
     ;          `(u ,f ,@(list ``(+ ,d ,e) (> y u))))))))
     (lambda/variables/closed
      '(lambda ()
         `(u ,f ,@(list ``(+ ,d ,e) (> y u))))
      (lexical/new '((u) (x a y) (d) (e f))))))
  
  ;  (let ((u 1) (x 'xx) (a 'aa) (y 33) (d '7) (e 8) (f 'ff))
  ;    `(u ,f ,@(list `(+ d e) (> y u))))
  ;  =>
  ;  (u ff (+ d e) #t)
  (test-case "Closed variable check 14"
             (check-equal? '(f u y)
     
     ; (lambda (u)
     ;  (lambda (x a y)
     ;    (lambda (d)
     ;      (lambda (e f)
     ;        (lambda ()
     ;          `(u ,f ,@(list `(+ d e) (> y u))))))))     
     (lambda/variables/closed
      '(lambda ()
         `(u ,f ,@(list `(+ d e) (> y u))))
      (lexical/new '((u) (x a y) (d) (e f)))))))


(run-tests lexical-address-tests)
(run-tests closed-variable-tests)