#lang racket

(provide (all-defined-out))

(struct prebuffer (do-more? q))
(define (make-prebuffer size)
  (prebuffer (λ (p)
               (< (length (prebuffer-q p)) size))
             '()))

(define useless-prebuffer (prebuffer (λ (_) #f) '()))

(define (prebuffer-more? p)
  ((prebuffer-do-more? p) p))

(define (prebuffer-do p data fun)
  (let ([p* (prebuffer-add p data)])
    (if (prebuffer-more? p*)
        p*
        (prebuffer-do-for-each p fun))))

(define (prebuffer-add p data)
  (match p
    [(prebuffer f l) (prebuffer f (cons data l))]))

(define (prebuffer-do-for-each p fun)
  (match p
    [(prebuffer f l) (for-each fun (reverse l))
                     useless-prebuffer]))