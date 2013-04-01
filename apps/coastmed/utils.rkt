#lang racket/base

(require COAST
         racket/date)
(provide (all-defined-out))

;;; this should be in the infrastructure
(define-syntax-rule (motile/procedure id body)
  (define id (motile/call (motile/compile (quote body)) BASELINE)))

(define (spawn-actor nickname chieftain cons-auth-list curl-auth-list)
  (let-values ([(actor locative) (actor/new chieftain nickname)])
    
    ;sets the authority to constrain locatives for new actor 
    (locative/cons/authority! locative (cons actor cons-auth-list)) 
    (locative/curl/authority! locative (cons actor curl-auth-list)) 
    
    (define curl (curl/new locative null #f)) ; creates a generic CURL for the new actor 
    (values actor locative curl))) ;;returns actor and locative


;returns an specific date in milliseconds 
(define (expiration/date->milliseconds day month year hour minute tmz)
  
  (define (leap? y)
    (if (= 0 (modulo y 4)) 
        (if (= 0 (modulo y 100))
            (if (= 0 (modulo y 400)) #t #f)
            #t)
        #f))
  
  (define (get-week-day day month year)
    (let* ((monthValueTable (vector 0 3 3 6 1 4 6 2 5 0 3 5))
           (week-day-calc1 (+ day (vector-ref monthValueTable (- month 1))))
           (week-day-calc2 (if (> week-day-calc1 6) 
                               (- week-day-calc1 (* 7 (floor (/ week-day-calc1 7)))) 
                               week-day-calc1))
           (year-last2 (string->number (substring (number->string year) 2 4)))
           (week-day-calc3 (if (> year-last2 28) 
                               (- year-last2 (* 28 (floor (/ year-last2 28)))) 
                               year-last2))
           (week-day-calc4 (floor (+ week-day-calc3 (/ week-day-calc3 4))))
           (week-day-calc5 (if (and (or (= month 1) (= month 2)) (leap? year))
                               (- week-day-calc4 1)
                               week-day-calc4))
           (week-day-calc6 (+ week-day-calc5 week-day-calc2))
           (week-day-calc7 (if (> week-day-calc6 6) 
                               (- week-day-calc6 (* 7 (floor (/ week-day-calc6 7))))
                               week-day-calc6)))
      week-day-calc7))
  
  (define (get-year-day d m y)
    (let ((days-month (if (leap? y) 
                          (vector 31 29 31 30 31 30 31 31 30 31 30 31)
                          (vector 31 28 31 30 31 30 31 31 30 31 30 31))))
      (define (add-up-to x) 
        (cond
          ((= x 0) 0)
          (else (+ (vector-ref days-month x) (add-up-to (- x 1))))))
      (- (+ d (add-up-to (- m 1)))) 1))
  
  ;;tmz needs to be in format -8, 0, 8 (up to -12 and 12)
  (let ((dateStruct (make-date 0 minute hour day month year (get-week-day day month year) (get-year-day day month year) #t (* tmz 3600))))
    (* 1000 (date->seconds dateStruct))))


