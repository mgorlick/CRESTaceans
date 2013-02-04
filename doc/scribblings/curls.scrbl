#lang scribble/doc
@(require scribble/manual)
 
@;{ Bindings from Motile/actor/curl.rkt and
   Motile/actor/send.rkt}


@title[#:tag "curls"]{Capability URL (CURL)}
         

@defproc[(curl/new/any [l locative?] [path (listof symbol?)] [meta (or/c #f (and/c hash/persist? hash/eq?))] [lifespan list? null]) (or/c #f curl?)]{Creates a new unsigend CURL derived from locative @racket[l] on the following conditions: locative @racket[l] must not be expired nor revoked; the @racket[lifespan] of the new CURL (expressed in seconds and included as the first element of list @racket[lifespan]) must be equal to or smaller that the expiration of locative @racket[l]; and @racket[locative/sends/positive?] must not be @racket[#f], meaning that there are still sends allowed to this locative. Otherwise it returns @racket[#f].
                                                                       
The null @racket[path] is equivalent the path name @racket[/]. The path @racket[(s_1 s_2 ... s_n)] is equivalent to the path name @racket[/s_1/s_2/.../s_n].} 
                                                              


@defproc[(curl/new [l locative?] [path (listof symbol?)] [meta (or/c #f (and/c hash/persist? hash/eq?))] [lifespan list? null]) (or/c #f curl?)]{Equivalent to @racket[curl/new/any], but imposes the additional restriction of checking whether the actor creating the curl is in the @racket[curl/new] whitelist of locative @racket[l], namely if the result of @racket[locative/curl/authority?] is @racket[#t].}
              

@defproc[(curl? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a CURL, @racket[#f] otherwise.}   

@defproc[(curl/intra? [c curl?]) boolean?]{Returns @racket[#t] if CURL @racket[c] is an intra-island CURL, @racket[#f] otherwise.}

@;{ not sure if to incude this one, sice used by itself it returns #f given that the island address in embedded at some other time}
@defproc[(curl/island [c curl?]) island?]{Returns the island address for CURL @racket[c].} 

@;{ not sure if to incude this one, sice used by itself it returns #f given that the island address in embedded at some other time}
@defproc[(curl/get-island-address [c curl?]) island?]{Equivalent to @racket[curl/island].} 

@defproc[(curl/id [c curl?]) (or/c locative? pair? symbol?)]{Returns either the id of the locative CURL @racket[c] represents or the locative itself. It this is an inter-island CURL it will return the locative's id represented by a pair or symbol. If instead the CURL is intra-island, it will return the locative.}
 
@;{ not sure if to provide this to the outside world }
@defproc[(curl/id! [c curl?] [id (or/c locative? pair? symbol?)]) void?]{Changes the value of CURL @racket[c]'s id.}

@defproc[(curl/target=? [c1 curl?] [c2 curl?]) boolean?]{Returns @racket[#t] if CURLs @racket[c1] and @racket[c1] share both the same island address and the locative they represent. This means that the results of @racket[(curl/id c)] and @racket[(curl/island c)] must be the same for both.}

@defproc[(curl/path [c curl?]) (listof symbol?)]{Returns the path of CURL @racket[c] as a list (possibly empty) of symbols.}

@defproc[(curl/get-path [c curl?]) (listof symbol?)]{Equivalent to @racket[curl/path].}
 
@defproc[(curl/sends [c curl?]) (or/c 0 (and/c postive? integer?))]{Returns the use count of CURL @racket[c].}

@;{ not sure if ths is meant for the outside world}
@defproc[(curl/signing [c curl?]) ?]{Returns the island signing for CURL @racket[c].}

@;{ not sure if ths is meant for the outside world}
@defproc[(curl/signing! [c curl?]) ?]{Modifies the island signing for CURL @racket[c].} 

@defproc[(curl/get-meta [c curl?]) ?]{Returns the arbitrary metadata embedded in CURL @racket[c].}
 
@defproc[(curl/pretty [c curl?]) (listof pair?)]{Returns the (partial) contents of a CURL @racket[c] as an association list.}

@defproc[(curl/send [c curl?] [msg any/c]) boolean?]{Using CURL @racket[c] as a destination address, sends message @racket[msg] to the actor denoted by CURL @racket[c]. Returns @racket[#t] on successful delivery and @racket[#f] otherwise. If the sender desires a reply, then the CURL metadata should contain an item reply/x where reply is the symbol reply and x is the CURL to which the reply should be directed.} 
    
@defproc[(curl/send/multiple [cl (listof curl?)] [msg vector?]) boolean?]{Using the CURL within the list @racket[cl] as a destination addresses, sends message @racket[msg] to all the actors denoted by those CURLs. Returns @racket[#t] on successful delivery and @racket[#f] otherwise. If the sender desires a reply, then the CURL metadata should contain an item reply/x where reply is the symbol reply and x is the CURL to which the reply should be directed.} 

@defproc[(curl/send/promise [c curl?] [msg vector?] [lifespan number?]) boolean?]{Using CURL @racket[c] as a destination address, sends message @racket[msg] to the actor denoted by CURL @racket[c]....} 

@defproc[(curl/forward [d delivery?]) boolean?]{Only to be used by chieftains to deliver messages embedded within delivery @racket[d] to actors. Do not expose this function to actors.}
