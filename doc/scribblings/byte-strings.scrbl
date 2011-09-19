#lang scribble/doc
@(require scribble/manual)


@title[#:tag "byte-strings"]{Byte strings}


@defproc[(bytes [b byte?] ...) bytes?]{ Returns a new mutable byte
string whose length is the number of provided @racket[b]s, and whose
positions are initialized with the given @racket[b]s.}

@defproc[(bytes-length [bstr bytes?]) exact-nonnegative-integer?]{
 Returns the length of @racket[bstr].}
                       
@defproc[(bytes-ref [bstr bytes?] [k exact-nonnegative-integer?])
 byte?]{  Returns the character at position @racket[k] in @racket[bstr].
 The first position in the bytes cooresponds to @racket[0], so the
 position @racket[k] must be less than the length of the bytes,
 otherwise an exception is raised.}
       
@defproc[(subbytes [bstr bytes?] [start exact-nonnegative-integer?]
 [end exact-nonnegative-integer? (bytes-length str)]) bytes?]{ Returns
 a new mutable byte string that is @racket[(- end start)] bytes long,
 and that contains the same bytes as @racket[bstr] from @racket[start]
 inclusive to @racket[end] exclusive.  The @racket[start] and
 @racket[end] arguments must be less than or equal to the length of
 @racket[bstr], and @racket[end] must be greater than or equal to
 @racket[start], otherwise an exception is raised.}
       
@defproc[(bytes=? [bstr1 bytes?] [bstr2 bytes?] ...+) boolean?]{ Returns @racket[#t] if all of the arguments are @racket[eqv?].} 


@defproc[(bytes<? [bstr1 bytes?] [bstr2 bytes?] ...+) boolean?]{
 Returns @racket[#t] if the arguments are lexicographically sorted
 increasing, where individual bytes are ordered by @racket[<],
 @racket[#f] otherwise.}
 
@defproc[(bytes>? [bstr1 bytes?] [bstr2 bytes?] ...+) boolean?]{Like @racket[bytes<?], but checks whether the arguments are decreasing}
   
@defproc[(bytes/append [bstr bytes?] ...) bytes?]{ Returns a new mutable byte string
that is as long as the sum of the given @racket[bstr]s' lengths, and
that contains the concatenated bytes of the given @racket[bstr]s. If
no @racket[bstr]s are provided, the result is a zero-length byte
string.}
