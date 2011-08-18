#lang racket/base

(require (for-syntax racket/base racket/list racket/syntax syntax/parse)
         racket/match
         racket/function
         "../../../Motile/persistent/tuple.rkt")

(define-for-syntax (string->stx k s)
  (datum->syntax k (string->symbol s)))
(define-for-syntax (stx->string s)
  (symbol->string (syntax->datum s)))

(define-syntax (define-tuple-type stx)
  (syntax-parse stx
                [(k (namepath ...) (~seq mandatory:id) ... [optional:id => option-default-val:expr] ...)
                 ;; suppose this were invoked as (define-tuple-type '(message foo bar) baz quux):
                 (let* (; produce a list of strings like '("message foo bar")
                        [namepath-strings (map stx->string (syntax->list #'(namepath ...)))]
                        ; produce a string like "message/foo/bar"
                        [name/space/path/string (apply string-append (add-between namepath-strings "/"))]
                        ; produce a list of symbols like '(":message/foo/bar/baz" ":message/foo/bar/quux")
                        [accessors
                         (map (λ (field)
                                (string-append ":" name/space/path/string "/" (stx->string field)))
                              (append (syntax->list #'(mandatory ...)) (syntax->list #'(optional ...))))]
                        ; the total number of accessors specified, mandatory + optional
                        [acclen (length (append (syntax->list #'(mandatory ...)) (syntax->list #'(optional ...))))])
                   (with-syntax*
                    ([constructor          (string->stx #'k (string-append name/space/path/string "/new"))]
                     [constructor?         (string->stx #'k (string-append name/space/path/string "?"))]
                     [expander             (string->stx #'k name/space/path/string)]
                     [prefix               (string->stx #'k (string-upcase name/space/path/string))]
                     [mlen                 (datum->syntax #'k acclen)]
                     [(mandatory-args ...) (generate-temporaries #'(mandatory ...))]
                     [(optional-args ...)  (generate-temporaries #'(optional ...))]
                     [(accessor-ref ...)   (build-list acclen add1)] ; (1 2 3 4) for a record with 4 accessors
                     [(accessor-name ...)  (map (λ (acc) (string->stx #'k acc)) accessors)]
                     [(setter-name ...)    (map (λ (acc) (string->stx #'k (string-append "!" acc))) accessors)])
                    #'(begin
                        (define prefix '(namepath ...))
                        (define constructor
                          (case-lambda
                            [(mandatory-args ...)
                             (tuple prefix mandatory-args ... option-default-val ...)]
                            [(mandatory-args ... optional-args ...)
                             (tuple prefix mandatory-args ... optional-args ...)]))
                        (define (constructor? m)
                          (and (tuple? m) (= (tuple/length m) (add1 mlen)) (andmap eq? prefix (tuple/ref m 0))))
                        (define (accessor-name m)
                          (tuple/ref m accessor-ref))
                        ...
                        (define (setter-name m v)
                          (if (= accessor-ref (tuple/length m))
                              (tuple/append (tuple/take/left m accessor-ref) (tuple v))
                              (tuple/append (tuple/take/left m accessor-ref) (tuple v) 
                                            (tuple/drop/left m (add1 accessor-ref)))))
                        ...            
                        (define-match-expander expander
                          (syntax-rules ()
                            [(_ fields (... ...))
                             (vector '<tuple> (? (curry equal? prefix) flavor) fields (... ...))]))
                        )))]))

#|
(require rackunit rackunit/text-ui unstable/match)
(define-tuple-type (message foo) zuh buh [muh => #f] [guh => null])
(check-equal?  MESSAGE/FOO '(message foo))
(check-equal? (message/foo/new #t #"Hello")
              '#(<tuple> (message foo) #t #"Hello" #f ()) )
(check-equal? (message/foo/new 5 3 '() add1)
              `#(<tuple> (message foo) 5 3 () ,add1))
(check-true (message/foo? (message/foo/new #t #f)))
(check-equal? (:message/foo/zuh (message/foo/new 12 24))
              12)
(check-equal? (:message/foo/zuh (!:message/foo/zuh (message/foo/new 12 24) 6))
              6)
(check-true (match? (message/foo/new #t #"Hello")
                    (message/foo zuh* buh* #f (? null? guh))))
|#