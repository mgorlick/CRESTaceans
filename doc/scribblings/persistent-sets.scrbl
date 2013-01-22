#lang scribble/doc
@(require scribble/manual)


@title[#:tag "persistent-sets"]{Persistent unordered sets}

@defidform[set/eq/null]{Produces a new persistent set for which @racket[(set/empty? set)] is equal to @racket[#t], therefore an empty persistent set where elements are compared with @racket[eq?].}

@defidform[set/eqv/null]{Produces a new persistent set for which @racket[(set/empty? set)] is equal to @racket[#t], therefore an empty persistent set where elements are compared with @racket[eqv?].}

@defidform[set/equal/null]{Produces a new persistent set for which @racket[(set/empty? set)] is equal to @racket[#t], therefore an empty persistent set where elements are compared with @racket[equal?].}

@defproc[(set/persist? [e any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a persistent set, @racket[#f] otherwise.}

@defproc[(set/eq? [s set/persist?]) boolean?]{Returns @racket[#t] if the equality test for elements is @racket[eq?], @racket[#f] otherwise.}

@defproc[(set/eqv? [s set/persist?]) boolean?]{Returns @racket[#t] if the equality test for elements is @racket[eqv?], @racket[#f] otherwise.}

@defproc[(set/equal? [s set/persist?]) boolean?]{Returns @racket[#t] if the equality test for elements is @racket[equal?], @racket[#f] otherwise.}

@defproc[(list/set [s set/persist?] [lst list?]) set/persist?]{Nondestructively produces a new persistent set @racket[hash\'] from the union of set @racket[s] and a list of elements (e_0 ... e_{n-1})}.

@defproc[(set/new [s set/persist?] [e any/c] ...) set/persist?]{Equivalent to @racket[(list/set s lst)], but takes multiple arguments instead of a list of elements.}

@defproc[(vector/set [s set/persist?] [vec vector?]) set/persist?]{Non-destructively creates a set @racket[s\'] that is the union of the contents of set @racket[s] and the contents of vector @racket[vec = #(e_0 e_1 ... e_{n-1})].}

@defproc[(set/cons [s set/persist?] [e any/c]) set/persist?]{Produces a new persistent set @racket[s\'] by adding the given value to the set.}

@defproc[(set/remove [s set/persist?] [e any/c]) set/persist?]{Produces a new persistent set @racket[s\'] whose contents are identlical to @racket[s] but without the element indexed by @racket[e].}

@defproc[(set/union [s1 set/persist?] [s2 set/persist?])set/persist?]{Produces a new persistent set @racket[s1\'] that is the union of set @racket[s1] and @racket[s2]. The resulting set is defined with the equality? and hash functions of set @racket[s1].}

@defproc[(set/intersection [s1 set/persist?] [s2 set/persist?])set/persist?]{Produces a new persistent set @racket[s1\'] that is the intersection of set @racket[s1] and @racket[s2]. The resulting set is defined with the equality? and hash functions of set @racket[s1].}
@defproc[(set/difference [s1 set/persist?] [s2 set/persist?])set/persist?]{Returns the subset @racket[s1\'] of  @racket[s1] such that no member of @racket[s1\'] is also a member of @racket[s2].}
@defproc[(set/list [s set/persist?]) list?]{Returns a list of the elements of @racket[s] in an unspecified order.}


@defproc[(set/vector [s set/persist?]) vector?]{Returns a vector #(e_0 ... e_{n-1}) of elements within @racket[s].}


@defproc[(set/car [s set/persist?]) any/c]{Returns the "first" element of set @racket[s].}              

@defproc[(set/cdr [s set/persist?]) list?]{Produces a new persistent set @racket[s\'] in which the element returned by @racket[(set/car hash)] is absent.} 


@defproc[(set/length [s set/persist?]) natural/exact?]{Returns the length of @racket[s] (i.e., the number of elements in the persistent set).}                                                                                   

@defproc[(set/empty? [s set/persist?]) boolean?]{Returns @racket[#t] if @racket[(set/length s)] is equal to @racket[0], otherwise returns @racket[#f].}


@defproc[(set/contains? [s set/persist?] [e any/c]) boolean?]{Returns @racket[#t] if @racket[s] contains element @racket[e] and @racket[#f] otherwise.}


@defproc[(set/fold [s set/persist?] [proc procedure?] [seed any/c]) any/c]{Fold function @racket[proc] with @racket[seed] over all pairs in persistent set @racket[s]. Function @racket[proc] takes two arguments, an element of set s and the current seed, and returns the new seed value. The value of the fold is the final seed value. }


@defproc[(set/map [s set/persist?] [proc procedure?]) set/persist?]{Applies @racket[proc] to each element in @racket[s]. The @racket[proc] function takes a single argument. The result is a persistent set containing the results.}


@defproc[(set/subset? [s1 set/persist?] [s2 set/persist?]) booleant?]Returns @racket[#t] if set @racket[s2] is a subset of set @racket[s1] and @racket[#f] otherwise.}


@defproc[(set/filter [s set/persist?] [proc procedure?]) set/persist?]{Returns a persistent set containing only those elements in @racket[s] for which @racket[(proc e)] is @racket[#t].}


@defproc[(set/partition [s set/persist?] [proc procedure?]) pair?]{Partitions set @racket[s] into two disjoint subsets @racket[s1] and @racket[s2] of @racket[s] where @racket[(proc e)] is @racket[#t] for all members of @racket[s1] and returns @racket[#f] for all members of @racket[s2]. Returns the partition as a pair @racket[(s1 . s2)].}


