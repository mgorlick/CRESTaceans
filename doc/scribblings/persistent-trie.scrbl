#lang scribble/doc
@(require scribble/manual)

@title[#:tag "persistent-trie"]{Persistent trie}
 

@defidform[trie/empty]{Produces an empty trie node with a map of zero and no allocated slots.}

@defproc[(trie? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a persistent trie, @racket[#f] otherwise.}

@defproc[(trie/empty? [t trie?]) boolean?]{Returns @racket[#t] if trie @racket[t] is empty, @racket[#f] otherwise.}

@defproc[(trie/car [t trie?]) pair?]{Assuming that trie node @racket[t] is non-empty, return the first key/value pair found in a left-to-right depth-first search of the trie rooted at @racket[t].}

@defproc[(trie/cdr [t trie?]) trie?]{Assuming that trie node @racket[t] is non-empty, non-destructively return a replacement trie node @racket[t\'] such that @racket[t\'] is the root of a trie constituting the "rest" of @racket[t].}

@defproc[(trie/get [equality procedure?] [t trie?] [key any/c] [hash any/c] [shift byte?]) pair?]{Locate key/value pair in trie node @racket[t] of hash table @racket[h]. If found return the pair @racket[(key . value)], otherwise return @racket[#f].} @racket[equality] is the equality predicate for keys; @racket[hash] is the hash code of key, that is, return value of @racket[(hasher key)]; and @racket[shift] the rightward shift to compute trie map mask.}

@defproc[(trie/with [equality procedure?] [hasher procedure?] [t trie?] [key any/c] [value any/c] [h any/c] [shift byte?]) trie?]{Non-destructively add the key/value pair to trie node t returning the successor trie. @racket[equality] is the equality predicate for keys; @racket[hasher] is the hash function for keys; @racket[hash] is the hash code of key, that is, return value of @racket[(hasher key)]; and @racket[shift] the rightward shift to compute trie map mask.}

@defproc[(trie/key/with [equality procedure?] [hasher procedure?] [t trie?] [key any/c] [value any/c] [hash any/c] [shift byte?]) trie?]{A specialized version of trie/with for the case in which only the key is significant and the accompanying value is immaterial, for example, when using hash tables to implement unordered sets. This allows us to optimize the special case in which the key already appears in trie t or its subnodes by eliminating the expense of generating a new trie or overflow node needlessly. @racket[equality] is the equality predicate for keys; @racket[hasher] is the hash function for keys; @racket[hash] is the hash code of key, that is, return value of @racket[(hasher key)]; and @racket[shift] the rightward shift to compute trie map mask.}

@defproc[(trie/without [equality procedure?] [t trie?] [key any/c] [hash any/c] [shift byte?]) trie?]{Remove (non-destructively) the given key from the trie whose root is @racket[t]. @racket[equality] is the equality predicate for keys; @racket[hash] is the hash code of key, that is, return value of @racket[(hasher key)]; and @racket[shift] the rightward shift to compute trie map mask.}

@defproc[(pairs/count [t trie?]) integer?]{Return the total number of key/value pairs in the trie rooted at @racket[t].}

@defproc[(pairs/fold [f procedure?] [seed any/c] [t trie?]) integer?]{Fold function @racket[f] with @racket[seed] over all pairs appearing in the trie rooted at @racket[t].}



                                     