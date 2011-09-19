#lang scribble/doc
@(require scribble/manual)


@title[#:tag "numeric-comparisons"]{Numeric comparisons}

@defproc[(= [z number?] [w number?] ...+) boolean?]{ Returns
 @racket[#t] if all of the arguments are numerically equal,
 @racket[#f] otherwise.  An inexact number is numerically equal to an
 exact number when the exact coercion of the inexact number is the
 exact number. Also, @racket[0.0] and @racket[-0.0] are numerically
 equal, but @racket[+nan.0] is not numerically equal to itself.}
 
 @defproc[(< [x real?] [y real?] ...+) boolean?]{ Returns @racket[#t] if the arguments in the given order are strictly increasing, @racket[#f] otherwise.}

@defproc[(<= [x real?] [y real?] ...+) boolean?]{ Returns @racket[#t]
 if the arguments in the given order are non-decreasing, @racket[#f] otherwise.}


@defproc[(> [x real?] [y real?] ...+) boolean?]{ Returns @racket[#t] if
 the arguments in the given order are strictly decreasing, @racket[#f] otherwise.}


@defproc[(>= [x real?] [y real?] ...+) boolean?]{ Returns @racket[#t] if the arguments in the given order are non-increasing, @racket[#f] otherwise.}


                                                
