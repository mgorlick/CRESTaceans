#lang racket/base

(require COAST)

(provide (all-defined-out))

(define (environs/merge env . envs)
  (foldr environ/merge env envs)) 
  

(define MESSAGES/RECEIVE
  (pairs/environ
   environ/null
   (list
    (define/global/0 'thread-receive          thread-receive)
    (define/global/1 'delivery?               delivery?)
    (define/global/1 'delivery/contents-sent  delivery/contents-sent)
    (define/global/1 'delivery/promise-fulfillment delivery/promise-fulfillment))))

(define MESSAGES/SEND
  (pairs/environ
   environ/null
   (list
    (define/global/2 'curl/send curl/send)
    (define/global/3 'curl/send/promise curl/send/promise))))
    

(define DISPLAYING
  (pairs/environ
   environ/null
   (list
    (define/global/N 'format format)
    (define/global/1 'display display))))

  
    
