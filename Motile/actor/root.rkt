#lang racket/base

(require 
 (only-in "jumpstart.rkt" actor/root/new))

(provide ROOT ROOT/LOCATIVE)

(define-values (ROOT ROOT/LOCATIVE) (actor/root/new))

 
