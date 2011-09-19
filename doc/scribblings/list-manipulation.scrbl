#lang scribble/doc
@(require scribble/manual 
          scribble/racket
          (for-syntax racket/base))



@title[#:tag "list-manipulation"]{List manipulation}

@defproc[(length [lst list?]) exact-nonnegative-integer?]{Returns the number of elements in @racket[lst].}

@defproc*[([(append [lst list?] ...) list?]
           [(append [lst list?] ... [v any/c]) any/c])]{When given all list arguments, the result is a list that contains all of the elements of the given lists in order. 
                                                        The last argument is used directly in the tail of the result. 
                                                        
                                                        The last argument need not be a list, in which case the result is an ``improper list.''}
                                                       
       

@defproc[(reverse [lst list?]) list?]{Returns a list that has the same elements as @racket[lst], but in reverse order.}


@defproc[(cons [a any/c] [d any/c]) pair?]{Returns a newly allocated pair whose first element is @racket[a] and second element is @racket[d].}

@defproc[(list [v any/c] ...) list?]{Returns a newly allocated list containing the @racket[v]s as its elements.}

@defproc[(list* [v any/c] ... [tail any/c]) any/c]{Like @racket[list], but the last argument is used as the tail of the result, 
                                                        instead of the final element. The result is a list only if the last argument is a list.}

@defproc[(car [p pair?]) any/c]{Returns the first element of the pair @racket[p].}


@defproc[(cdr [p pair?]) any/c]{Returns the second element of the pair @racket[p].}


@(define-syntax (defc_r stx)
   (syntax-case stx ()
     [(_ x ...)
      (let ([xs (map syntax-e (syntax->list #'(x ...)))])
        (let ([name (string->symbol
                     (string-append
                      "c"
                      (apply string-append (map symbol->string xs))
                      "r"))]
              [contract (let loop ([l (reverse xs)])
                          (cond
                            [(null? (cdr l)) 'pair?]
                            [(eq? (car l) 'a) `(cons/c ,(loop (cdr l)) any/c)]
                            [(eq? (car l) 'd) `(cons/c any/c ,(loop (cdr l)))]))]
              [equiv (let loop ([l xs])
                       (cond 
                         [(null? l) 'p]
                         [(eq? (car l) 'a) `(car ,(loop (cdr l)))]
                         [(eq? (car l) 'd) `(cdr ,(loop (cdr l)))]))])
          (with-syntax ([name name]
                        [contract (let loop ([c contract] [pos 0])
                                    (if (pair? c)
                                        (let* ([a (loop (car c) (add1 pos))]
                                               [b (loop (cdr c) (+ 1 pos (syntax-span a)))]
                                               [span (+ 1 (syntax-span a) (syntax-span b))])
                                          (datum->syntax #'here
                                                         (cons a b)
                                                         (list (syntax-source stx)
                                                               1
                                                               pos
                                                               (add1 pos)
                                                               span)))
                                        (datum->syntax #'here c 
                                                       (list (syntax-source stx) 1 pos (add1 pos) 1))))]
                        
                        [equiv equiv])
            #'(defproc (name [v contract]) any/c
                "Returns " (to-element 'equiv)))))]))



@defc_r[a a]
@defc_r[a d]
@defc_r[d a]
@defc_r[d d]
@defc_r[a a a]
@defc_r[a a d]
@defc_r[a d a]
@defc_r[a d d]
@defc_r[d a a]
@defc_r[d a d]
@defc_r[d d a]
@defc_r[d d d]
@defc_r[a a a a]
@defc_r[a a a d]
@defc_r[a a d a]
@defc_r[a a d d]
@defc_r[a d a a]
@defc_r[a d a d]
@defc_r[a d d a]
@defc_r[a d d d]
@defc_r[d a a a]
@defc_r[d a a d]
@defc_r[d a d a]
@defc_r[d a d d]
@defc_r[d d a a]
@defc_r[d d a d]
@defc_r[d d d a]
@defc_r[d d d d]


@defproc[(list-ref [lst any/c] [pos exact-nonnegative-integer?]) any/c]{
                                                                        Returns the element of @racket[lst] at position @racket[pos], 
                                                                        where the list's first element is position @racket[0]. If the list has
                                                                        @racket[pos] or fewer elements, then the an exception is raised.
                                                                        
                                                                        The @racket[lst] argument need not actually be a list; @racket[lst]
                                                                        must merely start with a chain of at least @racket[pos] pairs.}
               
               
@defproc[(member [v any/c] [lst list?]) (or/c list? #f)]{
                          Locates the first element of @racket[lst] that is @racket[equal?] to @racket[v]. If such an element exists, 
                          the tail of @racket[lst] starting with that element is returned. Otherwise, the result is @racket[#f].}
                         
                         
@defproc[(memq [v any/c] [lst list?]) (or/c list? #f)]{Like @racket[member], but finds an element using @racket[eq?].}
                                                  
                                                  
@defproc[(memv [v any/c] [lst list?]) (or/c list? #f)]{Like @racket[member], but finds an element using @racket[eqv?].}
                                                  
@defproc[(assoc [v any/c] [lst (listof pair?)] [is-equal? (any/c any/c -> any/c) equal?]) (or/c pair? #f)]{
                                                                                                           Locates the first element of @racket[lst] 
                                                                                                           whose @racket[car] is equal to @racket[v] 
                                                                                                           according to @racket[is-equal?]. If such 
                                                                                                           an element exists, the pair (i.e., an 
                                                                                                           element of @racket[lst]) is returned. 
                                                                                                           Otherwise, the result is @racket[#f].}


@defproc[(assq [v any/c] [lst (listof pair?)]) (or/c pair? #f)]{Like @racket[assoc], but finds an element using @racket[eq?].}
                                                                           
@defproc[(assv [v any/c] [lst (listof pair?)]) (or/c pair? #f)]{Like @racket[assoc], but finds an element using @racket[eqv?].}
                                                                                                    
                                                                                                    