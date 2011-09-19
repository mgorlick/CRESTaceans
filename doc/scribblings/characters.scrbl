#lang scribble/doc
@(require scribble/manual)


@title[#:tag "characters"]{Characters}

@defproc[(char=? [char1 char?] [char2 char?] ...+) boolean?]{Returns @racket[#t] if all of the arguments are @racket[eqv?].}

@defproc[(char<? [char1 char?] [char2 char?] ...+) boolean?]{

Returns @racket[#t] if the arguments are sorted increasing, where two characters are ordered by their scalar values, @racket[#f] otherwise.}

@defproc[(char<=? [char1 char?] [char2 char?] ...+) boolean?]{Like @racket[char<?], but checks whether the arguments are nondecreasing.}

@defproc[(char>? [char1 char?] [char2 char?] ...+) boolean?]{Like @racket[char<?], but checks whether the arguments are decreasing}

@defproc[(char>=? [char1 char?] [char2 char?] ...+) boolean?]{Like char<?, but checks whether the arguments are nonincreasing.}

@defproc[(char-ci=? [char1 char?] [char2 char?] ...+) boolean?]{Returns @racket[#t] if all of the arguments are @racket[eqv?] after 
                                                                locale-insensitive case-folding via @racket[char-foldcase].}


@defproc[(char-ci<? [char1 char?] [char2 char?] ...+) boolean?]{Like @racket[char<?], but checks whether the arguments would be in
                                                                increasing order if each was first case-folded using @racket[char-foldcase] 
                                                                (which is locale-insensitive).}


@defproc[(char-ci<=? [char1 char?] [char2 char?] ...+) boolean?]{Like @racket[char-ci<?], but checks whether the arguments would be nondecreasing after case-folding.}

@defproc[(char-ci>? [char1 char?] [char2 char?] ...+) boolean?]{Like @racket[char-ci<?], but checks whether the arguments would be decreasing after case-folding.}
    
@defproc[(char-ci>=? [char1 char?] [char2 char?] ...+) boolean?]{Like @racket[char-ci<?], but checks whether the arguments would be nonincreasing after case-folding.}

@defproc[(char-alphabetic? [char char?]) boolean?]{Returns @racket[#t] if @racket[char] has the Unicode ``Alphabetic'' property.}
   
@defproc[(char-numeric? [char char?]) boolean?]{Returns @racket[#t] if @racket[char] has the Unicode ``Numeric'' property.}  

@defproc[(char-whitespace? [char char?]) boolean?]{Returns @racket[#t] if @racket[char] has the Unicode ``White_Space'' property.}
    
@defproc[(char-lower-case? [char char?]) boolean?]{Returns @racket[#t] if @racket[char] has the Unicode ``Lowercase'' property.}    
    
@defproc[(char->integer [char char?]) exact-integer?]{Returns a character's code-point number.} 

@defproc[(integer->char [k (and/c exact-integer? (or/c (integer-in 0 #xD7FF) (integer-in #xE000 #x10FFFF)))]) char?]{

Return the character whose code-point number is @racket[k]. For @racket[k] less than @racket[256], the result is the same object for
the same @racket[k].}
    
@defproc[(char-upcase [char char?]) char?]{Produces a character consistent with the 1-to-1 code point mapping defined by Unicode. 
                                           If @racket[char] has no upcase mapping, @racket[char-upcase] produces @racket[char].}

@defproc[(char-downcase [char char?]) char?]{Like @racket[char-upcase], but for the Unicode downcase mapping.}
                                                
