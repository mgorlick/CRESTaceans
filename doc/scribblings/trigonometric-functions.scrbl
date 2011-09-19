#lang scribble/doc
@(require scribble/manual)


@title[#:tag "trigonometric-functions"]{Trigonometric functions}


@defproc[(sin [z number?]) number?]{

Returns the sine of @racket[z], where @racket[z] is in radians. The result is normally inexact, but it is exact @racket[0] if @racket[z] is exact @racket[0].}


@defproc[(cos [z number?]) number?]{

Returns the cosine of @racket[z], where @racket[z] is in radians.}



@defproc[(tan [z number?]) number?]{

Returns the tangent of @racket[z], where @racket[z] is in radians. The
 result is normally inexact, but it is exact @racket[0] if @racket[z]
 is exact @racket[0].}


@defproc[(asin [z number?]) number?]{

Returns the arcsine in radians of @racket[z]. The result is normally
 inexact, but it is exact @racket[0] if @racket[z] is exact @racket[0].}


@defproc[(acos [z number?]) number?]{Returns the arccosine in radians of @racket[z].}


@defproc*[([(atan [z number?]) number?]
           [(atan [y real?] [x real?]) number?])]{

In the one-argument case, returns the arctangent of the inexact
 approximation of @racket[z], except that the result is an exact
 @racket[0] for an exact @racket[0] argument.

In the two-argument case, the result is roughly the same as @racket[
 (atan (/ (exact->inexact y)) (exact->inexact x))], but the signs of @racket[y]
 and @racket[x] determine the quadrant of the result. Moreover, a
 suitable angle is returned when @racket[y] divided by @racket[x]
 produces @racket[+nan.0] in the case that neither @racket[y] nor
 @racket[x] is @racket[+nan.0]. Finally, if @racket[y] is exact
 @racket[0] and @racket[x] is an exact positive number, the result is
 exact @racket[0]. If both @racket[x] and @racket[y] are exact
 @racket[0], an exception will be raised.}

 