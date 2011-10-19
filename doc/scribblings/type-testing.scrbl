#lang scribble/doc
@(require scribble/manual)
 
@title[#:tag "type-testing"]{Type testing}


@defproc[(boolean? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is @racket[#t] or @racket[#f], @racket[#f] otherwise.}

@defproc[(list? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a list: either the empty list, or a pair whose second element is a list. Otherwise, it returns @racket[#f].}

@defproc[(null? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is the empty list, @racket[#f] otherwise.}

@defproc[(pair? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a pair, @racket[#f] otherwise.}

@defproc[(symbol? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a symbol, @racket[#f] otherwise.}

@defproc[(char? [v any/c]) boolean?]{Return @racket[#t] if @racket[v] is a character, @racket[#f] otherwise.}

@defproc[(string? [v any/c]) boolean?]{ Returns @racket[#t] if @racket[v] is a string, @racket[#f] otherwise.}

@defproc[(byte? [v any/c]) boolean?]{ Returns @racket[#t] if @racket[v] is
 a byte (i.e., an exact integer between @racket[0] and @racket[255]
 inclusive), @racket[#f] otherwise.}

@defproc[(bytes? [v any/c]) boolean?]{ Returns @racket[#t] if @racket[v] is a byte string, @racket[#f] otherwise.}

@defproc[(number? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a number, @racket[#f] otherwise.}

@defproc[(real? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a real number, @racket[#f] otherwise.}

@defproc[(integer? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is an integer number, @racket[#f] otherwise.}

@defproc[(zero? [z number?]) boolean?]{ Returns @racket[(= 0 z)].}

@defproc[(positive? [x real?]) boolean?]{ Returns @racket[(> x 0)].}

@defproc[(negative? [x real?]) boolean?]{ Returns @racket[(< x 0)].}
                                            
@defproc[(even? [n integer?]) boolean?]{ Returns @racket[(zero? (modulo n 2))].}

@defproc[(odd? [n integer?]) boolean?]{ Returns @racket[(not (even? n))].}

@defproc[(exact? [z any/c]) number?]{Returns @racket[#t] if @racket[z] is an exact number, @racket[#f] otherwise.}

@defproc[(inexact? [z any/c]) number?]{Returns @racket[#t] if @racket[z] is an inexact number, @racket[#f] otherwise.}

@defproc[(integer/exact? [v any/c]) boolean?]{Returns @racket[(and (integer? v) (exact? v))].}
                                                      
@defproc[(natural/exact? [v any/c]) boolean?]{Returns @racket[(and (integer/exact? v) (not (negative? v)))].}
                                                      
@defproc[(integer/positive/exact? [v any/c]) boolean?]{Returns @racket[(and (integer/exact? v) (positive? v))].}
                                                               
@defproc[(real/inexact? [v any/c]) boolean?]{Returns @racket[(and (real? v) (inexact? v))].}

@defproc[(procedure? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a procedure, @racket[#f] otherwise.}


