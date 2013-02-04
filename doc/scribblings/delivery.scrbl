#lang scribble/doc
@(require scribble/manual)
 

@title[#:tag "delivery"]{Delivery}

                                          
@defproc[(delivery? [msg vector?]) boolean?]{Returns @racket[#t] if msg is a vector whose fist element is a CURL, as well as the third element (if this exists), otherwise it returns @racket[#f].}

@defproc[(delivery/curl-used [msg vector?]) curl?]{Returns the CURL used for the delivery of message @racket[msg].}

@defproc[(delivery/contents-sent [msg vector?]) any/c]{Returns the content of message @racket[msg].}

/*not sure about this description*?
@defproc[(delivery/promise-fulfillment [msg vector?]) (or/c #f curl?)]{Returns the CURL to which the recipient of message @racket[msg] should respond the request.}

        