#lang scribble/doc
@(require scribble/manual)
 
@title[#:tag "type-transformations"]{Type transformations}


@defproc[(exact->inexact [v number?]) inexact?]{Coerces @racket[v] to an inexact number. If @racket[v] is already inexact, it is returned.}

@defproc[(inexact->exact [v number?]) exact?]{Coerces @racket[v] to an inexact number. If @racket[v] is already exact, it is returned. If @racket[v] is @racket[+inf.0], @racket[-inf.0], 
                                                      or @racket[+nan.0], then an exception is raised.}

@defproc[(number->string [v number?] [radix (or/c 2 8 10 16) 10]) string?]{Returns a string that is the printed form of @racket[v] in the base specified by @racket[radix]. 
                                               If @racket[v] is inexact, @racket[radix] must be @racket[10], otherwise an exception is raised.}

@defproc[(string->number [s string?] [radix (integer-in 2 16) 10]) (or/c number? #f)]{Reads and returns a number datum from @racket[s], returning @racket[#f] if @racket[s] does not
parse exactly as a number datum (with no whitespace). The optional @racket[radix] argument specifies the default base for the number,
which can be overridden by @litchar{#b}, @litchar{#o}, @litchar{#d}, or
@litchar{#x} in the string.}

@defproc[(bytes/list [bstr bytes?]) (listof byte?)]{ Returns a new
 list of bytes corresponding to the content of @racket[bstr]. That is,
 the length of the list is @racket[(bytes-length bstr)], and the
 sequence of bytes in @racket[bstr] is the same sequence in the
 result list.}

@defproc[(list/bytes [lst (listof byte?)]) bytes?]{ Returns a new
 mutable byte string whose content is the list of bytes in @racket[lst].
 That is, the length of the byte string is @racket[(length lst)], and
 the sequence of bytes in @racket[lst] is the same sequence in
 the result byte string.}

@defproc[(symbol/string [sym symbol?]) string?]{Returns a freshly allocated mutable string whose characters are the same as in @racket[sym].}

@defproc[(string/symbol [str string?]) symbol?]{Returns an interned symbol whose characters are the same as in @racket[str].}


@defproc[(vector/list [vec vector/persist?]) list?]{Returns a list with the same length and elements as @racket[vec].}







