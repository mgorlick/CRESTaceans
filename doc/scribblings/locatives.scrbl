#lang scribble/doc
@(require scribble/manual)  

@title[#:tag "locatives"]{Locatives}
          

@defproc[(locative/unrestricted/new [a actor?]) locative?]{Returns a new unrestricted locative @racket[l] assigned to actor @racket[a], which by default never expires, has infinite amount of sends (messages that can be sent to this actor), has no restrictions on inter- and intra-island sends, it is not revoked nor derived.}

 @defproc[(locative? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a locative, @racket[#f] otherwise.}
 
 @defproc[(locative/id [l locative?]) symbol?]{Returns the UUID as island-unique locative @racket[l] identifier.}
 
 @defproc[(locative/actor [l locative?]) actor?]{Returns the actor locative @racket[l] guards.}
 
 @defproc[(locative/expires [l locative?]) real?]{Returns the expiration time of locative @racket[l].}
 
@defproc[(locative/expired? [l locative?]) boolean?]{Returns @racket[#t] if locative @racket[l] has expired, @racket[#f] otherwise.}

@defproc[(locative/unexpired? [l locative?]) boolean?]{Returns @racket[#t] if locative @racket[l] has not expired, @racket[#f] otherwise.}

@defproc[(locative/revoked [l locative?]) real?]{Returns @racket[#t] if locative @racket[l] is revoked, @racket[#f] otherwise.}

@defproc[(locative/revoked? [l locative?]) boolean?]{Returns @racket[#t] if locative @racket[l] has been revoked or if any superior locative of @racket[l] has been revoked, @racket[#f] otherwise.}
 
 @defproc[(locative/cons/authority? [l locative?] [a actor?]) boolean?]{Returns @racket[#t] if actor @racket[a] is a member of the locative/cons whitelist of locative @racket[l], @racket[#f] otherwise.}

 @defproc[(locative/cons/authority! [l locative?] [al (listof actor?)]) void?]{Sets the whitelist of actors@racket[al]  permited to execute a locative/cons against locative @racket[l].}
 
 @defproc[(locative/curl/authority? [l locative?] [a actor?]) boolean?]{Returns @racket[#t] if actor @racket[a] is a member of the curl/new whitelist of locative @racket[l], @racket[#f] otherwise.}
 
@defproc[(locative/curl/authority! [l locative?] [al (listof actor?)]) void?]{Sets the whitelist of actors @racket[al] permited to execute a curl/new against locative @racket[l].} 
 
 @defproc[(locative/send [l locative?] [msg any/c]) boolean?]{Returns @racket[#t] if the message @racket[msg] was delivered to the actor given by locative @racket[l]. Returns @racket[#f] if locative @racket[l] is invalid or revoked and no message is delivered in these cases.}
 
 @defproc[(locative/sends/positive? [l locative?]) (or/c boolean? integer?)]{Returns @racket[n = sends count > 0] for locative @racket[l], @racket[#f] if sends count is exhausted.}
 

  
  
  @defproc[(locative/cons/any [l locative?] [expires (or/c #f (and/c positive? real?))] [sends (or/c #f (and/c integer? positive?) +inf.0)] [senders/intra (or/c #t procedure?)] [senders/inter (or/c #t procedure?)]) (or/c #f locative?)]{Permits any actor to create a derivative of any locative @racket[l]. Given locative @racket[l] derives and returns a more restricted locative @racket[l\']. Derived locatives form a tree in which a descendant locative grants no more (and often less) capability than its parent locative. The capability of a locative @racket[l] comprises the expiration date of @racket[l], the total number of sends permitted for @racket[l], the criteria used to determine which set of intra- and inter- island actors may use @racket[l] to send a message to the actor guarded by @racket[l] implemented either by @racket[#t] (allows any actor to send messages) or a one-argument predicate. The one-argument predicate assigned to @racket[senders/intra] operates over actors and the one-argument predicate assigned to @racket[senders/inter] operates over island addresses.
                                                                                                                                                                                                                                                                                                     The path of @racket[l] is always prefix of the path of @racket[l\'].  The expiration of @racket[l\'] is always <= the expiration of @racket[l].The total number of sends allocated to @racket[l\'] is always <= the total number of sends outstanding for @racket[l] at the creation time of @racket[l\']. The set of actors that may use @racket[l\'] to send a message to the actor denoted by @racket[l] is always a subset of the set of actors that may use @racket[l] to send a message to the actor guarded by @racket[l].}
  
  @defproc[(locative/cons [l locative?] [expires (or/c #f (and/c positive? real?))] [sends (or/c #f (and/c integer? positive?) +inf.0)] [senders/intra (or/c #t procedure?)] [senders/inter (or/c #t procedure?)]) (or/c #f locative?)]{Operates in the same way as locative/cons/any but additionally @racket[locative/cons/authority?] must return @racket[#t].}
  
@defproc[(locative/pretty [l locative?]) (listof list?)]{Returns a list ((id_1 actor_nickname_1 expires_1 sends_1 revoked_1) ... (id_m actor_nickname_m expires_m sends_m revoked_m)) tracing the parentage of locative @racket[l] from itself (id_1 actor_nickname_1 expires_1 sends_1 revoked_1) all the way to the root chieftain (id_m actor_nickname_m expires_m sends_m revoked_m) for debugging purposes.}
 
 
 
 