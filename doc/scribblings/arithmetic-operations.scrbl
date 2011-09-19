#lang scribble/doc
@(require scribble/manual)


@title[#:tag "arithmetic-operations"]{Arithmetic operations}

@defproc[(+ [z number?] ...) number?]{
                                      Returns the sum of the @racket[z]s, adding pairwise from left to right. 
                                      If no arguments are provided, the result is @racket[0].}

@defproc*[([(- [z number?]) number?]
           [(- [z number?] [w number?] ...+) number?])]{

When no @racket[w]s are supplied, returns @racket[(- 0 z)].
 Otherwise, returns the subtraction of the @racket[w]s from @racket[z]
 working pairwise from left to right.}


@defproc[(* [z number?] ...) number?]{

Returns the product of the @racket[z]s, multiplying pairwise from left
 to right. If no arguments are provided, the result is
 @racket[1]. Multiplying any number by exact @racket[0] produces exact
 @racket[0].}
               

@defproc*[([(/ [z number?]) number?]
           [(/ [z number?] [w number?] ...+) number?])]{

When no @racket[w]s are supplied, returns @racket[(/ 1 z)].
 Otherwise, returns the division of @racket[z] by the @racket[w]s working
 pairwise from left to right.

If @racket[z] is exact @racket[0] and no @racket[w] is exact
 @racket[0], then the result is exact @racket[0]. If any @racket[w] is
 exact @racket[0], an exception will be raised.}


@defproc[(quotient [n integer?] [m integer?]) integer?]{

Returns @racket[(truncate (/ n m))].}

@defproc[(remainder [n integer?] [m integer?]) integer?]{

Returns @racket[_q] with the same sign as @racket[n] such that

@itemize[

 @item{@racket[(abs _q)] is between @racket[0] (inclusive) and @racket[(abs m)] (exclusive), and}

 @item{@racket[(+ _q (* m (quotient n m)))] equals @racket[n].}

]

If @racket[m] is exact @racket[0], an exception will be raised.}


@defproc[(modulo [n integer?] [m integer?]) integer?]{ 

Returns @racket[_q] with the same sign as @racket[m] where

@itemize[

 @item{@racket[(abs _q)] is between @racket[0] (inclusive) and @racket[(abs m)] (exclusive), and}

 @item{the difference between @racket[_q] and @racket[(- n (* m (quotient n m)))] is a multiple of @racket[m].}

]

If @racket[m] is exact @racket[0], an exception will be raised.}


@defproc[(add1 [z number?]) number?]{ Returns @racket[(+ z 1)].}

@defproc[(sub1 [z number?]) number?]{ Returns @racket[(- z 1)].}

@defproc[(1- [z number?]) number?]{ A handy alias for @racket[add1].}

@defproc[(1+ [z number?]) number?]{ A handy alias for @racket[sub1].}


@defproc[(exp [z number?]) number?]{

Returns Euler's number raised to the power of @racket[z]. The result
 is normally inexact, but it is exact @racket[1] when @racket[z] is an
 exact @racket[0].}

@defproc[(log [z number?]) number?]{

Returns the natural logarithm of @racket[z].  The result is normally
 inexact, but it is exact @racket[0] when @racket[z] is an exact
 @racket[1]. When @racket[z] is exact @racket[0], an exception will be raised.}


@defproc[(sqrt [z number?]) number?]{

Returns the principal @as-index{square root} of @racket[z].  The
 result is exact if @racket[z] is exact and @racket[z]'s square root
 is rational. See also @racket[integer-sqrt].}

@defproc[(expt [z number?] [w number?]) number?]{

Returns @racket[z] raised to the power of @racket[w]. If @racket[w] is
 exact @racket[0], the result is exact @racket[1]. If @racket[z] is
 exact @racket[0] and @racket[w] is negative, an exception will be raised.}


@defproc[(max [x real?] ...+) real?]{

Returns the largest of the @racket[x]s, or @racket[+nan.0] if any
 @racket[x] is @racket[+nan.0].  If any @racket[x] is inexact, the
 result is coerced to inexact.}


@defproc[(min [x real?] ...+) real?]{

Returns the smallest of the @racket[x]s, or @racket[+nan.0] if any
 @racket[x] is @racket[+nan.0].  If any @racket[x] is inexact, the
 result is coerced to inexact.}


@defproc[(gcd [n integer?] ...) integer?]{

Returns the @as-index{greatest common divisor} (a non-negative
 number) of the @racket[n]s. If no arguments are provided, the result
 is @racket[0]. If all arguments are zero, the result is zero.}


@defproc[(lcm [n integer?] ...) integer?]{

Returns the @as-index{least common multiple} (a non-negative number)
 of the @racket[n]s. If no arguments are provided, the result is
 @racket[1]. If any argument is zero, the result is zero; furthermore,
 if any argument is exact @racket[0], the result is exact @racket[0].}
 

@defproc[(round [x real?]) integer?]{

Returns the integer closest to @racket[x], resolving ties in favor of an even number.}


@defproc[(floor [x real?]) integer?]{

Returns the largest integer that is no more than @racket[x].}


@defproc[(ceiling [x real?]) integer?]{

Returns the smallest integer that is at least as large as @racket[x].}


@defproc[(truncate [x real?]) integer?]{

Returns the integer farthest from @racket[0] that is not farther from
 @racket[0] than @racket[x].}

 
@defproc[(numerator [q rational?]) integer?]{

Coerces @racket[q] to an exact number, finds the numerator of the
 number expressed in its simplest fractional form, and returns this
 number coerced to the exactness of @racket[q].}


@defproc[(denominator [q rational?]) integer?]{

Coerces @racket[q] to an exact number, finds the numerator of the
 number expressed in its simplest fractional form, and returns this
 number coerced to the exactness of @racket[q].}

@defproc[(rationalize [x real?] [tolerance real?]) real?]{

Among the real numbers within @racket[(abs tolerance)] of @racket[x],
 returns the one corresponding to an exact number whose
 @racket[denominator] is the smallest.  If multiple integers are within
 @racket[tolerance] of @racket[x], the one closest to @racket[0] is
 used.}




  