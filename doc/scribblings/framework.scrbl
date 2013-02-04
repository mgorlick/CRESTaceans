#lang scribble/doc
@(require scribble/manual)

@;{Bindings from peer/src/api/framework.rkt}
 
 
@title[#:tag "framework"]{ISLAND - Peering infrastructure}

@defproc[(listen-on/make-root [i island/address?]) (values actor? locative? locative? curl? thread?)]{Sets the island address to @racket[i], creates a root actor, creates and registers a public locative in island @racket[i]'s locative serialization table, retrieves a public locative and public CURL for use in speaking to the root actor, and starts up the networking layer listening on that island address. Returns a set of values which includes the root actor, its locative, a public locative, and a public curl.} 

@defproc[(eval-actor [a actor?] [f procedure?] [env environ?]) void?]{Given an actor @racket[a], a motile zero-argument procedure @racket[f], and a binding environment @racket[env], instantiate @racket[a] as the procedure @racket[f] evaluated in the context of @racket[env].}

@defproc[(curl/known-public? [host (or/c string? bytes?)] [port exact-nonnegative-integer?]) boolean?]{Test to see whether there is a public curl for some @racket[host/port] combination.}

@defproc[(curl/get-public [host (or/c string? bytes?)] [port exact-nonnegative-integer?]) curl?]{Returns a public CURL @racket[c] naming some computation on some other island if we have one available.}

@defproc[(argsassoc [key any/c] [#:call call procedure? values] [#:no-val no-val any/c #f] [#:default default any/c #t]) any/c]{Separates the provided command line arguments of the form @racket[key1=val1 key2=val2 key3=val3]. If the provided @racket[key] name is present and has a value, returns @racket[call] applied to that value. If the provided @racket[key] name is present but has no value, returns `@racket[default].If the provided @racket[key] name is not present, returns @racket[no-val].}