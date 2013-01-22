#lang scribble/doc
@(require scribble/manual)
 

@title[#:tag "islands"]{Islands}

                                          
@defproc[(island/address/new [public_key bytes?][dns bytes?] [ip_port (and/c integer? positive?)]) island?]{Creates a new self-certifying island given its public key, DNS name, and IP port. A usage example is @racket[(island/address/new #"abcd" #"127.0.0.1" 5000)].}                   
                                                                               @defproc[(island/address? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is an island, @racket[#f] otherwise.}    

@defproc[(island/address/ok? [i island/address?]) boolean?]{Returns @racket[#t] if the fields of island/address @racket[i] are type correct, @racket[#f] otherwise.}

@defproc[(island/address/equal? [i1 island/address?] [i2 island/address?]) boolean?]{Returns @racket[#t] island/address @racket[i1] is equal to island/address @racket[i2], @racket[#f] otherwise.}

@defproc[(island/address/public [i island/address?]) bytes?]{Returns island @racket[i]'s public key.}

@defproc[(island/address/dns [i island/address?]) bytes?]{Returns island @racket[i]'s DNS name.}

@defproc[(island/address/port [i island/address?]) (and/c integer positive?)]{Returns island @racket[i]'s port number.}

@defproc[(island/address/get-public [i island/address?]) bytes?]{Like @racket[island/address/public] it returns island @racket[i]'s public key.}

@defproc[(island/address/get-dns [i island/address?]) bytes?]{Like @racket[island/address/dns] it returns island @racket[i]'s DNS name.}

@defproc[(island/address/get-port [i island/address?]) (and/c integer positive?)]{Like @racket[island/address/port] it returns island @racket[i]'s port number.}

@defproc[(this/island) any/c]{Returns a value that by default is @racket[#f]. If invoked with argument @racket[v] it changes the return value of invoking @racket[(this/island)] to @racket[v].}
 
 
 