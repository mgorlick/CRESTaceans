#lang racket/base

(require rackunit
         rackunit/text-ui)

(require "../free.rkt")
(require/expose "../free.rkt"
                (lambda/variables/closed))

(define (should-be id expected outcome)
  (if (equal? expected outcome)
      (display (format "PASS id: ~a expected: ~s outcome: ~s\n" id expected outcome))
      (display (format "FAIL id: ~a expected: ~s outcome: ~s\n" id expected outcome))))

;; Helper for defining a lexical stack for testing purposes.
(define (lexical/new outlines)
  (let loop ((outlines outlines) (lexical #f))
    (if (null? outlines)
        lexical
        (loop (cdr outlines) (lexical/push/parameters lexical (car outlines))))))

(define (test/lexical/address)
  (define lexical (lexical/new '((u) (x a y) () (d) (e f))))

  (define (test/lexical/address/1)
    (should-be
     'lexical/address/1 '(0 . 0)
     (lexical/address lexical 'e)))

  (define (test/lexical/address/2)
    (should-be
     'lexical/address/2 '(2 . 1)
     (lexical/address lexical 'a)))

  (define (test/lexical/address/3)
    (should-be
     'lexical/address/2 '(2 . 2)
     (lexical/address lexical 'y)))

  (define (test/lexical/address/4)
    (should-be
     'lexical/address/4 '(3 . 0)
     (lexical/address lexical 'u)))
  
  ; This should throw an error.
  (define (test/lexical/address/5)
    (should-be
     'lexical/address/4 '(3 . 0)
     (lexical/address lexical 'z))) ; No variable z in lexical stack.
  
  (test/lexical/address/1)
  (test/lexical/address/2)
  (test/lexical/address/3)
  (test/lexical/address/4)
  (test/lexical/address/5))

(define (test/variables/closed)  
  (define (test/variables/closed/1)
    (should-be
     'variables/closed/1 '()
     (lambda/variables/closed '(lambda () a) #f)))

  (define (test/variables/closed/2)
    (should-be
     'variables/closed/2
     '(x y)
     ; (lambda (x y) (lambda (a) (+ a y x)))
     (lambda/variables/closed
      '(lambda (a) (+ a y x))
      (lexical/new '((x y))))))
  
  (define (test/variables/closed/3)
    (should-be
     'variables/closed/3 '(x y)
     ; (lambda (x y)
     ;   (lambda (a) (+ a y) (* x a a)))
     (lambda/variables/closed
      '(lambda (a) (+ a y) (* x a a))
      (lexical/new '((x y))))))

  (define (test/variables/closed/4)
    (should-be
     'variables/closed/4 '(a x y)
     ; (lambda (x a y)
     ;   (lambda ()
     ;     (+ a y)
     ;     (* x 33 a)))
     (lambda/variables/closed
      '(lambda ()
         (+ a y)
         (* x 33 a))
      (lexical/new '((x a y))))))
  
  (define (test/variables/closed/5)
    (should-be
     'variables/closed/5 '(a u x y)

     (lambda/variables/closed
      '(lambda ()
         (if y (+ a x) (* u 19)))
      (lexical/new '((x a y) (u))))))

  (define (test/variables/closed/6)
    (should-be
     'variables/closed/6 '(a u x y)
     
     (lambda/variables/closed
      '(lambda ()
         (when (> y 0) (+ a x) (* u 19)))
      (lexical/new '((x a y) (u))))))
  
  (define (test/variables/closed/7)
    (should-be
     'variables/closed/7 '(a u x y)

     (lambda/variables/closed 
      '(lambda ()
         (unless (> y 0)
           (if (< u 19) #t x)
           (* a x x)))
      (lexical/new '((x a y) (u))))))


  (define (test/variables/closed/8a)
    (should-be
     'variables/closed/8a
     '(a e f u x y)

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

  (define (test/variables/closed/8b)
    (should-be
     'variables/closed/8b
     '(a e f u x y)

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

  (define (test/variables/closed/9)
    (should-be
     'variables/closed/9
     '(a f)

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

  (define (test/variables/closed/10)
    (should-be
     'variables/closed/10
     '(d e f u y)
     
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
  (define (test/variables/closed/11)
    (should-be
     'variables/closed/11
     '(d e f u y)

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
  (define (test/variables/closed/12)
    (should-be
     'variables/closed/12
     '(f u y)

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
  (define (test/variables/closed/13)
    (should-be
     'variables/closed/13
     '(f u y)

     ; (lambda (u)
     ;  (lambda (x a y)
     ;    (lambda (d)
     ;      (lambda (e f)
     ;        (lambda ()
     ;          `(u ,f ,@(list `(+ d e) (> y u))))))))     
     (lambda/variables/closed
      '(lambda ()
                 `(u ,f ,@(list `(+ d e) (> y u))))
      (lexical/new '((u) (x a y) (d) (e f))))))
  
  (test/variables/closed/1)
  (test/variables/closed/2)
  (test/variables/closed/3)
  (test/variables/closed/4)
  (test/variables/closed/5)
  (test/variables/closed/6)
  (test/variables/closed/7)
  (test/variables/closed/8a)
  (test/variables/closed/8b)
  (test/variables/closed/9)
  (test/variables/closed/10)
  (test/variables/closed/11)
  (test/variables/closed/12)
  (test/variables/closed/13))