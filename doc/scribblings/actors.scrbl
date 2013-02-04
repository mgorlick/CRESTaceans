#lang scribble/doc
@(require scribble/manual          
          "../../Motile/actor/locative.rkt")

@;{ Bindings from Motile/actor/actor.rkt
   Motile/actor/jumpstart.rkt
   Motile/actor/root.rkt}
  
  
  @title[#:tag "actors"]{Actors}
  
  @;{This definition is wrong -- don't know how to express a return value that is multiple values.}                                             
    @defproc[(actor/new [chieftain actor?][nickname list?]) (values actor? locative?)]{Creates a new actor whose clan chieftain is given returning the primal locative of the actor. There is only one primal locative per actor. List @racket[nickname] provides a name to identify the actor. An actor is an independent thread of computation that may transmit asynchronous messages to other actors, receive asynchronous messages from other actors, conduct private computation, and spawn new actors. It is implemented as a vector where the 1st element is @racket['actor].}
    
    
    
    @defproc[(actor/chieftain/new [chieftain actor?][nickname list?]) (values actor? locative?)]{Creates a new chieftain by returning the primal locative of the chieftain. Chieftains are the only actors that are members of two clans: the parent clan that gave them birth and the clan that they themselves represent. In addition clan chieftains are the only actors for which their actor id and their clan id are identical. This function returns two values, a chieftain actor and its locative.}
    
    @defproc[(actor? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is an actor, @racket[#f] otherwise.}       
    
    @defproc[(actor/chieftain? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is the chieftain of some clan, @racket[#f] otherwise.}
    
    @defproc[(actor/root? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is the chieftain of the root clan, @racket[#f] otherwise.}   
    
    @defproc[(actor/dead? [a actor?]) boolean?]{Tests for the execution state of actor @racket[a].}
    
    @defproc[(actor/alive? [a actor?]) boolean?]{Tests for the execution state of actor @racket[a].}
    
    @defproc[(actor/suspended? [a actor?]) boolean?]{Tests for the execution state of actor @racket[a].}
    
    
    @defproc[(actor/clan/id [a actor?]) symbol?]{Returns the clan id of which actor @racket[a] is a member.}
    
    @defproc[(actor/chieftain [a actor?]) (or/c boolean? (and/c actor? actor/chieftain?)) ]{Returns the chieftain of actor @racket[a].}
    
    @defproc[(actor/id [a actor?]) symbol?]{Returns actor @racket[a]'s id.}
    
    @defproc[(actor/thread [a actor?]) (or/c boolean? thread?)]{Returns the thread that is animating actor @racket[a]. If there is not a thread assigned to this actor it returns @racket[#f].}
    
    @defproc[(actor/birthdate [a actor?]) real?]{Returns the creation timestamp of actor @racket[a].}
    
    @defproc[(actor/nickname [a actor?]) symbol?]{Returns actor @racket[a]'s nickname meant for debugging and logging.}
    
    @defproc[(actor/jumpstart [a actor?] [thunk any/c]) void?]{Jumpstarts an actor @racket[a] by sending a @racket[thunk] to the thread the actor is running on. After this procedure is invoked, @racket[(actor/dead? a)] will return @racket[#t].}
    
    @defproc[(actor/pretty [a actor?]) (listof pair?)]{Returns a list ((id_1 . nickname_1) ... (id_m . nickname_m)) tracing the parentage of actor @racket[a] from itself (id_1 . nickname_1) all the way to the root chieftain (id_m . nickname_m) for debugging purposes.}