#lang scribble/doc
@(require scribble/manual)

@;{Bindings from Motile/generate/baseline.rkt}
 
 
@title[#:tag "generate-baseline"]{BASELINE}

@defproc[(global-defines [f procedure?] ...) (listof pair?)]{Returns a list of global definitions by wrapping each host Racket function @racket[f] so that they can be called from Motile programs. The resulting list can be used to augment an existing binding environment.}

@defproc[(motile/call [f procedure?] [env environ?] [args list? null?]) any/c]{Allows for a Racket host function to a call a Motile function @racket[f] in the context of global binding environ @racket[env]. The value returned is the value computed by @racket[f].}
