#lang scribble/doc
@(require scribble/manual)
 
@title[#:tag "equivalence-predicates"]{Equivalence predicates}


@defproc[(eqv? [v1 any/c] [v2 any/c]) boolean?]{

Two values are @racket[eqv?] if and only if they are @racket[eq?], unless otherwise specified for a particular datatype.

The @tech{number} and @tech{character} datatypes are the only ones for which @racket[eqv?] differs from @racket[eq?].}


@defproc[(eq? [v1 any/c] [v2 any/c]) boolean?]{

Return @racket[#t] if @racket[v1] and @racket[v2] refer to the same
object, @racket[#f] otherwise.}


@defproc[(equal? [v1 any/c] [v2 any/c]) boolean?]{

Two values are @racket[equal?] if and only if they are @racket[eqv?],
unless otherwise specified for a particular datatype.}


