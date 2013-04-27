#lang racket/base

(require COAST)

(provide (all-defined-out))

(define MESSAGES/RECEIVE
  (pairs/environ
   environ/null
   (list
   
    (define/global/0 'thread-receive          thread-receive)
    (define/global/1 'delivery?               delivery?)
    (define/global/1 'delivery/contents-sent  delivery/contents-sent))))
    
