#lang scribble/doc
@(require scribble/manual)

@title[#:tag "persistent-trie"]{Persistent ordered trie}
 

@defidform[trie/empty]{Produces an empty trie node with a map of zero and no allocated slots.}

@defproc[(trie/car [t trie/pair?]) pair?]{Assuming that trie node @racket[t] is non-empty, return the first key/value pair found in a left-to-right depth-first search of the trie rooted at @racket[t].}

@defproc[(trie/cdr [t trie/pair?]) trie/pair?]{Assuming that trie node @racket[t] is non-empty, non-destructively return a replacement trie node @racket[t\'] such that @racket[t\'] is the root of a trie constituting the "rest" of @racket[t].}

@defproc[(trie/get [equality procedure?] [t trie/pair?] [key any/c] [hash any/c] [shift byte?]) pair?]{Locate key/value pair in trie node @racket[t] of hash table @racket[h]. If found return the pair @racket[(key . value)], otherwise return @racket[#f].} @racket[equality] is the equality predicate for keys; @racket[hash] is the hash code of key, that is, return value of @racket[(hasher key)]; and @racket[shift] the rightward shift to compute trie map mask.}

@defproc[(trie/with [equality procedure?] [hasher procedure?] [t trie/pair?] [key any/c] [value any/c] [h any/c] [shift byte?]) trie/pair?]{Non-destructively add the key/value pair to trie node t returning the successor trie. @racket[equality] is the equality predicate for keys; @racket[hasher] is the hash function for keys; @racket[hash] is the hash code of key, that is, return value of @racket[(hasher key)]; and @racket[shift] the rightward shift to compute trie map mask.}

@defproc[(trie/key/with [equality procedure?] [hasher procedure?] [t trie/pair?] [key any/c] [value any/c] [hash any/c] [shift byte?]) trie?]{A specialized version of trie/with for the case in which only the key is significant and the accompanying value is immaterial, for example, when using hash tables to implement unordered sets. This allows us to optimize the special case in which the key already appears in trie t or its subnodes by eliminating the expense of generating a new trie or overflow node needlessly. @racket[equality] is the equality predicate for keys; @racket[hasher] is the hash function for keys; @racket[hash] is the hash code of key, that is, return value of @racket[(hasher key)]; and @racket[shift] the rightward shift to compute trie map mask.}

@defproc[(trie/without [equality procedure?] [t trie/pair?] [key any/c] [hash any/c] [shift byte?]) trie?]{Remove (non-destructively) the given key from the trie whose root is @racket[t]. @racket[equality] is the equality predicate for keys; @racket[hash] is the hash code of key, that is, return value of @racket[(hasher key)]; and @racket[shift] the rightward shift to compute trie map mask.}

@defproc[(trie/pairs/total [t trie/pair?]) integer?]{Return the total number of trie pairs in the trie rooted at @racket[t].}

@defproc[(trie/fold [proc procedure?] [seed any/c] [x trie/pair?]) integer?]{Fold function @racket[proc] with @racket[seed] over all trie/pairs appearing in the trie rooted at @racket[t]. @racket[proc] accepts two arguments: a @racket[trie/pair] and the most recent folded value.
;; The first invocation of f is given the seed as the initial folded value.}

@defproc[(trie/flat/fold [proc procedure?] [seed any/c] [t trie/pair?]) integer?]{Similar to @racket[trie/pairs/fold] above except that @racket[proc] is given three arguments: key, value, and the most recent folded value. This function helps to insulate higher-level structures such as hash maps and binding environments from the internal representation that the trie uses for key/value pairs.}

@defproc[(trie/for-each [t trie/pair?] [proc procedure?]) trie/pair?]{Apply function @racket[proc], as @racket[(proc key value)], to each key/value in the trie @racket[t].}

@defproc[(trie/sort/insertion [t trie/pair?]) trie/pair?]{Return the contents of the hash table rooted at @racket[t] as a list of trie/pair where the list is sorted in increasing insertion order of the pairs.}

@defproc[(trie/sort/insertion=>vector [t trie/pair?]) vector?]{Like @racket[trie/sort/insertion] but returns the contents in insertion order as a flat vector @racket[#(k_1 v_1 ... k_n v_n)] where pair @racket[k_i/v_i] was added to the trie before pair @racket[k_{i+1}/v_{i+1}].}

@defproc[(trie/pair? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a trie/pair, and @racket[#f] otherwise. A trie/pair is a structure that has a key and a value and an associated order.}

@defproc[(trie/pair-order [t trie/pair?]) integer?]{Returns the order assigned to a trie/pair.}

@defproc[(trie/pair-key [t trie/pair?]) any/c]{Returns the key of a trie/pair structure.}

@defproc[(trie/pair-value [t trie/pair?]) any/c]{Returns the value of a trie/pair structure.}