#lang scribble/doc
@(require scribble/manual)


@title[#:tag "strings"]{Strings}


@defproc[(make-string [k exact-nonnegative-integer?] [char char?
#\nul]) string?]{ Returns a new mutable string of length @racket[k] where
each position in the string is initialized with the character
@racket[char].}

@defproc[(string [char char?] ...) string?]{ Returns a new
mutable string whose length is the number of provided @racket[char]s, and
whose positions are initialized with the given @racket[char]s.}

@defproc[(string-length [str string?]) exact-nonnegative-integer?]{
 Returns the length of @racket[str].}

@defproc[(string-ref [str string?] [k exact-nonnegative-integer?])
 char?]{  Returns the character at position @racket[k] in @racket[str].
 The first position in the string corresponds to @racket[0], so the
 position @racket[k] must be less than the length of the string,
 otherwise an exception will be raised.}
       
@defproc[(substring [str string?] [start exact-nonnegative-integer?]
 [end exact-nonnegative-integer? (string-length str)]) string?]{
 Returns a new mutable string that is @racket[(- end start)]
 characters long, and that contains the same characters as
 @racket[str] from @racket[start] inclusive to @racket[end] exclusive.
 The @racket[start] and @racket[end] arguments must be less than or
 equal to the length of @racket[str], and @racket[end] must be greater
 than or equal to @racket[start], otherwise an exception will be raised.}
                                                               
@defproc[(string-append [str string?] ...) string?]{
                                                    Returns a new mutable string that is as long as the sum of the given @racket[str]s' lengths, 
                                                    and that contains the concatenated characters of the given @racket[str]s. If no
                                                    @racket[str]s are provided, the result is a zero-length string.} 
       
    
@defproc[(string=? [str1 string?] [str2 string?] ...+) boolean?]{ Returns
 @racket[#t] if all of the arguments are @racket[equal?].}

@defproc[(string<? [str1 string?] [str2 string?] ...+) boolean?]{
 Returns @racket[#t] if the arguments are lexicographically sorted
 increasing, where individual characters are ordered by
 @racket[char<?], @racket[#f] otherwise.}

@defproc[(string<=? [str1 string?] [str2 string?] ...+) boolean?]{Like @racket[string<?], but checks whether the arguments are nondecreasing.}

@defproc[(string>? [str1 string?] [str2 string?] ...+) boolean?]{Like @racket[string<?], but checks whether the arguments are decreasing.}

@defproc[(string>=? [str1 string?] [str2 string?] ...+) boolean?]{Like @racket[string<?], but checks whether the arguments are nonincreasing.}

@defproc[(string-ci=? [str1 string?] [str2 string?] ...+) boolean?]{
 Returns @racket[#t] if all of the arguments are @racket[eqv?] after
 locale-insensitive case-folding via @racket[string-foldcase].}

@defproc[(string-ci<? [str1 string?] [str2 string?] ...+) boolean?]{
 Like @racket[string<?], but checks whether the arguments would be in
 increasing order if each was first case-folded using
 @racket[string-foldcase] (which is locale-insensitive).}

@defproc[(string-ci<=? [str1 string?] [str2 string?] ...+) boolean?]{Like @racket[string-ci<?], but checks whether the arguments would be nondecreasing after case-folding.}

@defproc[(string-ci>? [str1 string?] [str2 string?] ...+) boolean?]{Like @racket[string-ci<?], but checks whether the arguments would be decreasing after case-folding.}

@defproc[(string-ci>=? [str1 string?] [str2 string?] ...+) boolean?]{Like @racket[string-ci<?], but checks whether the arguments would be nonincreasing after case-folding.}

@defproc[(format [form string?] [v any/c] ...+) string?]{Formats to a string. For example: @racket[(format "~a as a string is ~s.\n" '(3 4) "(3 4)")]}