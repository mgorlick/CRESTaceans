#lang racket/base

(require racket/match
         racket/list
         racket/function
         racket/dict)

; XPath-like functionality for selecting on xexprs
; taken from Racket's
; collects/tests/web-server/util.rkt

(define keyword->symbol (compose string->symbol keyword->string))
(define (simple-xpath/xexpr p x)
  (match p
    [(list)
     (list x)]
    [(list-rest (? symbol? s) r)
     (match x
       [(list-rest (? (curry equal? s)) rs)
        (simple-xpath/tag-body r rs)]
       [_
        empty])]
    [_
     empty]))
(define (simple-xpath/tag-body p x)
  (match p
    [(list)
     (match x
       [(list) empty]
       [(list-rest (list (list (? symbol?) (? string?)) ...) rs)
        (simple-xpath/tag-body p rs)]
       [(? list?)
        x]
       [_ 
        empty])]
    [(list-rest (? symbol?) _)
     (match x
       [(list-rest (list (list (? symbol?) (? string?)) ...) rs)
        (simple-xpath/tag-body p rs)]
       [(? list?)
        (append-map (curry simple-xpath/xexpr p) x)]
       [_
        empty])]
    [(list (? keyword? k))
     (match x
       [(list-rest (and attrs (list (list (? symbol?) (? string?)) ...)) rs)
        (simple-xpath/attr (keyword->symbol k) attrs)]
       [_
        empty])]
    [_
     empty]))
(define (simple-xpath/attr k attrs)
  (dict-ref attrs k empty))
(define (simple-xpath*/list p x)
  (append (simple-xpath/xexpr p x)
          (match x
            [(list-rest (list (cons (? symbol?) (? string?)) ...) rs)
             (simple-xpath*/list p rs)]
            [(? list?)
             (append-map (curry simple-xpath*/list p) x)]
            [_
             empty])))
(define (simple-xpath* p x)
  (match (simple-xpath*/list p x)
    [(list) #f]
    [(list-rest f rs) f]))

; (test
; (simple-xpath*/list '(p) '(html (body (p "Hey") (p "Bar")))) => (list "Hey" "Bar")
; (simple-xpath* '(p) '(html (body (p "Hey")))) => "Hey"
; (simple-xpath* '(p #:bar) '(html (body (p ([bar "Zog"]) "Hey")))) => "Zog")    

(provide simple-xpath*
         simple-xpath*/list)