#lang scribble/doc
@(require scribble/manual)


@title[#:tag "persistent-binding-environments"]{Persistent binding environments}

@defidform[environ/null]{Produces a canonical empty binding environment, namely a two element vector @racket[vec], where the first element is the symbol 'environ/persist and the second element is a persistent hash table whose keys are the names of lexical variables (given as symbols) and arbirary Motile values.}

@defidform[BASELINE]{The binding environment containing set of primitive procedures available to all Motile mobile code.}

@defproc[(environ? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is a binding environment, @racket[#f] otherwise.}

@defproc[(environ/cons [env environ?] [key symbol?] [v symbol?]) environ?]{Produces a new persistent binding environment by merging the given key/value pair with the given environment @racket[env].}

@defproc[(environ/cons [env environ?] [lst list?]) environ?]{Produces a new persistent binding environment from the union of the contents of binding environment @racket[env] and a list of key/value pairs given as input in the form of (k_0 v_0 ... ... k_i v_i).}

@defproc[(vector/environ [env environ?] [vec vector?]) environ?]{Given a binding environment @racket[env] and a vector @racket[vec = #(k_0 v_0 ... k_{n-1} v_{n-1})] where @racket[k_i, v_i] is a symbol/value pair, this function returns a successor binding environment containing bindings k_0 v_0 ... k_{n-1} v_{n-1}. Environ @racket[env] remains unchanged.}

@defproc[(vectors/environ [env environ?] [keys vector?] [values vector?]) environ?]{Given a binding environment @racket[env] and vectors @racket[keys = #(k_0 ... k_{n-1})] corresponding to keys and @racket[values = #(v_0 ... v_{n-1})] corresponding to their respective values, this function returns a successor binding environment containing bindings k_0 v_0 ... k_{n-1} v_{n-1}.Environ @racket[env] remains unchanged.}

@defproc[(environ/merge [env1 environ?] [env2 environ?]) environ?]{Returns the merge of binding environment @racket[env2] into binding environment @racket[env1] returning a successor binding environment where the bindings of @racket[env2] are joined with those of @racket[env1]. The bindings of @racket[env2] take precedence over those of alpha.}

@defproc[(environ/vector/remove [env environ?] [keys vector?]) environ?]{Let @racket[keys] be a vector (possibly empty) of key names (k_1 ... k_m). This procedure removes the bindings k_1/v_1, ..., k_m/v_m from binding environment @racket[env] returning a successor environment that does not contain any of k_1/v_1, ..., k_m/v_m. @racket[env] remains unchanged.}

@defproc[(environ/ref [env environ?] [key any/c] [failure any/c]) any/c]{Returns the value for @racket[key] in binding environment @racket[env]. @racket[key] may be a symbol or a vector; otherwise an error will be returned. If no value is found for @racket[key], then @racket[failure] is returned as the result. @racket[failure] can as well be a procedure that is called in case @racket[key] is not in @racket[env].}

@defproc[(environ/ref/symbol [env environ?] [key symbol?] [failure any/c]) any/c]{Returns the value for @racket[key] in binding environment @racket[env]. If no value is found for @racket[key], then @racket[failure] is returned as the result. @racket[failure] can as well be a procedure that is called in case @racket[key] is not in @racket[env].}

@defproc[(environ/ref/path [env environ?] [keys vector?] [failure any/c]) any/c]{Returns the value for @racket[key] in binding environment @racket[env] and within a path of its nested binding environments. For example, @racket[(environ/ref/path env (vector 'env_sub1 'env_sub2 'hello_world) "failure")] is looking for the value of the binding named by symbol @racket['hello_world] within environment @racket[env_sub2], which is within environment @racket[env_sub1], which exists at the same time within environment @racket[env]. If no value is found for @racket[key], then @racket[failure] is returned as the result. @racket[failure] can as well be a procedure that is called in case @racket[key] is not in @racket[env].}

@defproc[(list/environ [env environ?] [lst list?]) environ?]{Given a list @racket[lst] of bindings (k_1 v_1 ... k_m v_m), join bindings k_1/v_1, ..., k_m/v_m to binding environment @racket[env] returning the successor environment containing k_1/v_1, ..., k_m/v_m. @racket[env] remains unchanged.}

@defproc[(pairs/environ [env environ?] [lst (listof pair?)]) environ?]{Given a list of pairs of bindings @racket[lst] ((k_0 . v_0) ... (k_i . v_i)), join bindings k_0/v_0 ... k_i/v_i to binding environment @racket[env] returning the successor environment containing k_0/v_0 ... k_i/v_i. @racket[env] remains unchanged.}



