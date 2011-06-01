#lang racket

(require xml)

(define a-greeting "<start number='1' />")
(string->xexpr a-greeting)