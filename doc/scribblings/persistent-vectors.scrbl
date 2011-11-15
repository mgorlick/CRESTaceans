#lang scribble/doc
@(require scribble/manual)
 
@title[#:tag "persistent-vectors"]{Persistent functional vectors}

@defidform[vector/null]{Produces a new persistent vector for which @racket[(vector/length _vec)] is equal to @racket[0], therefore and empty vector.}

@defproc[(vector/null? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is equal to @racket[vector/null], @racket[#f] otherwise.}


@defproc[(vector/persist? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a persistent vector (an immutable data structure), 
                                               @racket[#f] otherwise.}


@defproc[(vector/build [n natural/exact?] 
                       [proc (natural/exact? . -> . any/c)])
         vector/persist?]{Creates a persistent vector of @racket[n] elements by applying @racket[proc] to the integers from @racket[0] to 
                          @racket[(sub1 n)] in order. If @racket[_vec] is the resulting immutable vector, then @racket[(vector/ref _vec _i)] is the value
                          produced by @racket[(proc _i)].}
                         
                         
@defproc[(vector/length [vec vector/persist?]) natural/exact?]{Returns the length of @racket[vec] (i.e., the number of slots in the vector).}
   

@defproc[(vector/ref [vec vector/persist?] [pos natural/exact?]) any/c]{Returns the element in slot @racket[pos] of @racket[vec]. The first
                                                                            slot is position @racket[0], and the last slot is one less than
                                                                            @racket[(vector/length vec)].}


@defproc[(list/vector [vec vector/persist?] [lst list?]) vector/persist?]{Returns a new immutable or persistent vector that contains the values of
                                                                          @racket[vec] and the values of @racket[lst].} The values of @racket[lst] 
                                                                          are appended, in the newly created vector, to the tail of @racket[vec]. 
                                                                          @racket[vec] remains unchanged. If @racket[vec] @racket[vector/length] is 0
                                                                          the new persistent vector's lenght is the same as @racket[lst] lenght,
                                                                          otherwise the new persistent vector's lenght is equal to the lenght of
                                                                          @racket[vec] plus the length of the list.}
                                                                          

                                                                          
@defproc[(vector/fold/left [vec vector/persist?] [proc procedure?] [init any/c] ) any/c]{

@racket[vector/fold/left] applies a procedure to the elements of a persistent vector, combining the return values in an arbitrary way determined 
by @racket[proc]. @racket[proc] must be a function that takes at least 2 arguments. On the first iteration, @racket[proc] takes as arguments @racket[init] 
and the first element on the vector. The input vector is traversed from left to right, and the result of the whole @racket[vector/fold/left] 
is the result of the last application of @racket[proc]. If the @racket[vec] is empty, the result is @racket[init].}                                                                        
                                                                          
                                                                          
@defproc[(vector/fold/right [vec vector/persist?] [proc procedure?] [init any/c] ) any/c]{
Like @racket[vector/fold/left], but the vector @racket[vec] is traversed from right to left.}
     
     
@defproc[(vector/map [vec vector/persist?] [proc procedure?]) vector/persist?]{
Applies @racket[proc] to the elements of @racket[vec] from the first elements to the last. The @racket[proc] function is an unary one, 
thus takes a single argument. The result is a fresh vector containing each result of @racket[proc] in order.}
                   

@defproc[(vector/filter [vec vector/persist?] [pred procedure?]) vector/persist?]{
Returns a fresh persisntent vector with the elements of @racket[vec] for which @racket[pred] produces a true value. The @racket[pred] procedure is
 applied to each element from first to last.}
 
@defproc[(vector/cons [vec vector/persist?] [v any/c]) vector/persist?]{Appends @racket[v] at the end of vector @racket[vec].}
                                                                                
@defproc[(vector/subvector [vec vector/persist?] 
                           [from natural/exact?]) vector/persist?]{Returns a new persistent vector with the subset of @racket[vec]'s 
                                                                               elements starting from index @racket[from] up to the last index of
                                                                               @racket[vec].}
 

@defproc[(vector/subvector [vec vector/persist?] 
                           [from natural/exact?]
                           [end natural/exact?]) vector/persist?]{Returns a new persistent vector with the subset of @racket[vec]'s elements
                                                                              starting from index @racket[from] up to index of @racket[end] of vector 
                                                                              @racket[vec].} 
                                                                                            
@defproc[(vector/cdr [vec vector/persist?]) vector/persist?]{Removes the tail (last) element of persistent vector @racket[vec].} 

@defproc[(vector/update [vec vector/persist?] 
                        [i natural/exact?] 
                        [v any/c]) vector/persist?]{Sets the value at index @racket[i] of vector @racket[vec] to @racket[v].}
          