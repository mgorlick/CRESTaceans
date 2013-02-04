#lang scribble/doc
@(require scribble/manual)
 

@title[#:tag "promise"]{Promise}

                                          
@defproc[(promise/new [lifespan (and/c number? positive?)]) pair?]{Returns the pair @racket[(p . c)] where @racket[p] is the promise and @racket[c] is the CURL to which the resolution of @racket[p] should be sent. @racket[lifespan] is the maximun length of the promise expressed in seconds.} 

@defproc[(promise? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a promise, @racket[#f] otherwise.} 

@defproc[(promise/kept? [p promise?]) boolean?]{Returns @racket[#t] if promise @racket[p] has been resolved, @racket[#f] otherwise.} 

@defproc[(promise/ruined? [p promise?]) boolean?]{Returns @racket[#t] if promise @racket[p] has expired before its resolution, @racket[#f] otherwise.}  

@defproc[(promise/wait [p promise?] [patience (and/c number? positive?)] [failure any/c]) any/c]{Waits on promise @racket[p]. Patience @racket[p] is the time (a postive number) giving the span of time (expressed in real seconds) that the calling thread (actor) is willing to wait for the resolution of promise @racket[p]. @racket[failure] is the value returned if promise @racket[p] is not kept by the caller's deadline (in @racket[patience] seconds), or if promise @racket[p] has been ruined. Returns the value of promise @racket[p] if the promise was kept and returns @racket[failure] if the promise has been ruined or if the caller's patience is exhausted.}
 
 @defproc[(promise/call [p promise?] [f procedure?] [patience (and/c number? positive?)] [failure any/c]) any/c]{Returns the value of applying function @racket[f] to the outcome of promise @racket[p]. Patience @racket[p] is the time for resolution of promise @racket[p] expressed in real seconds. @racket[failure] is the value to be substituted if promise @racket[p] is not kept in @racket[patience] seconds.} 
 
@defproc[(promise/result [lst list?]) any/c]{Equivalent to @racket[car].} 

@defproc[(promise/to-fulfill [lst list?]) any/c]{Equivalent to @racket[cdr].}