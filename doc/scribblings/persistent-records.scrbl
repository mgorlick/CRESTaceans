#lang scribble/doc
@(require scribble/manual)


@title[#:tag "persistent-records"]{Persistent functional records}

@defproc[(record? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a record, @racket[#f] otherwise. A record is a five-element vector where the 1st element is 'record and the 3rd element is a persistent hash table, i.e. (vector 'record <kind> <hash> <signature>).}

@defproc[(record/contains? [rec record?] [key any/c]) boolean?]{Returns @racket[#t] if record @racket[rec] contains a given @racket[key], @racket[#f] otherwise.}

@defproc[(record/signed? [rec record?]) boolean?]{Returns @racket[#t] if record @racket[rec] is digitally signed (for the purpose of verifying that the record has not been changed), @racket[#f] otherwise. Signature is in the form of a byte string.}

@defproc[(record/kind [rec record?]) symbol?]{Returns the kind of record @racket[rec] as a symbol.}

@defproc[(record/keys [rec record?]) list?]{Return the field names of record @racket[rec] as a list of symbols.}    

@defproc[(record/unsign [rec record?]) record?]{Produces an unsigned copy of record @racket[rec].}  

   

