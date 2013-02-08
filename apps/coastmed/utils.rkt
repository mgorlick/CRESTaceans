#lang racket/base

(require COAST)
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


