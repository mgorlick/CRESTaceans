#lang scribble/doc
@(require scribble/manual)



@title[#:tag "logical-negation"]{Logical negation}

@defproc[(not [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is @racket[#f], @racket[#f] otherwise.}
