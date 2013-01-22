#lang scribble/doc
@(require scribble/manual)
 

@title[#:tag "curls"]{Capability URL (CURL) - NOTE}

                                          
@defproc[(curl/new/any [l locative?] [path (listof symbol?)] [meta (or/c #f (and/c hash/persist? hash/eq?))]) (or/c #f curl?)]{Creates a new unsigend CURL derived from locative @racket[l] on the following conditions: Locative @racket[l] must not be expired nor revoked; and @racket[locative/sends/positive?] must not be @racket[#f], meaning that there are still sends allowed to this locative. Otherwise it returns @racket[#f].
                                                                       The null @racket[path] is equivalent the path name @racket[/]. The path @racket[(s_1 s_2 ... s_n)] is equivalent to the path name @racket[/s_1/s_2/.../s_n].}


@defproc[(curl/new/any [l locative?] [path (listof symbol?)] [meta (or/c #f (and/c hash/persist? hash/eq?))] [lifespan list?]) (or/c #f curl?)]{Creates a new unsigend CURL derived from locative @racket[l] on the following conditions: Locative @racket[l] must not be expired nor revoked; the @racket[lifespan] of the new CURL must be equal to or smaller that the expiration of locative @racket[l]; and @racket[locative/sends/positive?] must not be @racket[#f], meaning that there are still sends allowed to this locative. Otherwise it returns @racket[#f].
                                                                       
The null @racket[path] is equivalent the path name @racket[/]. The path @racket[(s_1 s_2 ... s_n)] is equivalent to the path name @racket[/s_1/s_2/.../s_n].} 
                                                                                                         
@defproc[(curl/new [l locative?] [path (listof symbol?)] [meta (or/c #f (and/c hash/persist? hash/eq?))] [lifespan (or/c null? list?)]) (or/c #f curl?)]{The same as @racket[curl/new/any], but with the additional restriction that the current actor must be a member of the @racket[locative/curl/authority] of locative @racket[l] (authorized to create a CURL for this locative @racket[l]).}

@defproc[(curl? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a CURL, @racket[#f] otherwise.}   

/* don't know what type is strict? */
@defproc[(curl/ok? [c curl?] [strict? ?]) boolean?]{Returns @racket[#t] if CURL @racket[c] is well-formed, @racket[#f] otherwise. It is intended to be used by the Motile deserializer as it checks incoming CURLs.}

@defproc[(curl/intra? [c curl?]) boolean?]{Returns @racket[#t] if CURL @racket[c] is an intra-island CURL, @racket[#f] otherwise.}

@defproc[(curl/island [c curl?]) island?]{Returns the island address for CURL @racket[c].} 

@defproc[(curl/get-island-address [c curl?]) island?]{Equivalent to @racket[curl/island].} 

@defproc[(curl/id [c curl?]) (or/c locative? pair? symbol?)]{Returns either the id of the locative CURL @racket[c] represents or the locative itself. It this is an inter-island CURL it will return the locative's id represented by a pair or symbol. If instead the CURL is intra-island, it will return the locative.}
 
@defproc[(curl/id! [c curl?] [id (or/c locative? pair? symbol?)]) void?]{Changes the value of CURL @racket[c]'s id.}

@defproc[(curl/target=? [c1 curl?] [c2 curl?]) boolean?]{Returns @racket[#t] if CURLs @racket[c1] and @racket[c1] share both the same island address and the locative they represent. This means that the results of @racket[(curl/id c)] and @racket[(curl/island c)] must be the same for both.}

@defproc[(curl/path [c curl?]) (listof symbol?)]{Returns the path of CURL @racket[c] as a list (possibly empty) of symbols.}

@defproc[(curl/get-path [c curl?]) (listof symbol?)]{Equivalent to @racket[curl/path].}
 
@defproc[(curl/sends [c curl?]) (or/c 0 (and/c postive? integer?))]{Returns the use count of CURL @racket[c].}

@defproc[(curl/signing [c curl?]) ?]{Returns the island signing for CURL @racket[c].}
 
@defproc[(curl/signing! [c curl?]) ?]{Modifies the island signing for CURL @racket[c].} 

@defproc[(curl/get-meta [c curl?]) ?]{Returns the arbitrary metadata embedded in CURL @racket[c].}
 
@defproc[(curl/pretty [c curl?]) (listof pair?)]{Returns the (partial) contents of a CURL @racket[c] as an association list.}
