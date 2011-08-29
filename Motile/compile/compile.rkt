#lang racket/base

;; Copyright 2010 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(require racket/pretty)

(require
 "dissect.ss"
 "free.ss"
 "set.ss"
 (only-in "utility.rkt" vector/all?)
 (only-in "match.rkt" match/translate)
 
 (only-in "../generate/baseline.rkt"    motile/call)
 (only-in "../generate/closure.rkt"     closure/generate closure/rest/generate)
 (only-in "../generate/combination.rkt" combination/generate)
 (only-in "../generate/constant.rkt"    constant/generate)

 (only-in
  "../generate/control.rkt"
  and/generate
  if/generate
  or/generate
  sequence/generate
  when/generate
  unless/generate)

 (only-in
  "../generate/environ.rkt"
  environ/cons/generate
  environ/reflect/generate
  environ/remove/generate
  environ/path/value/generate
  environ/symbol/value/generate)

 (only-in "../generate/frame.rkt"      global/get/generate variable/get/generate)
 (only-in "../generate/lambda.rkt"     lambda/generate lambda/rest/generate)
 (only-in "../generate/letrec.rkt"     letrec/set/generate)
 (only-in "../generate/quasiquote.rkt" quasiquote/append/generate quasiquote/cons/generate quasiquote/tuple/generate)
 (only-in "../persistent/environ.rkt"  environ/null)
 (only-in "../persistent/hash.rkt"     hash/eq/null hash/ref vector/hash))

(provide
 motile/compile)

;; A closure is a function that captures the bindings of free variables in its lexical context.

(define (motile/compile x) (scheme/compile x #f))

;; Generate a closure (code) for the given expression.
;; e: the expression to compile
;; lexical: the lexical scope stack
;; frees: the set of free variables
(define (scheme/compile e lexical)
  (let ((e (motile/macro/expand e MACROS/INTERNAL)))
    (cond
      ((symbol? e)
       (cond
         ((lexical/variable/parameter? lexical e)
          ; local variable (Lambda argument) in lambda body.
          (variable/get/generate (reference/local/offset lexical e)))
         ((lexical/variable/closed? lexical e)
          ; Closed variable in lambda body.
          (variable/get/generate (reference/closed/offset lexical e)))
         ((eq? e 'null)
          (constant/generate null))         ; The constant, null.
         (else
          (global/get/generate e))))  ; Global variable.
      
      ((procedure? e) e) ; Inline code.
      
      ((not (pair? e))
       (constant/generate e))
      
      ((macro? e lexical)
       (scheme/compile (macro/expand e lexical) lexical))
      
      ((quote? e)
       (constant/generate (quotation e)))
      
      ((if? e)
       (if/compile e lexical))
      
;      ((cond? e)
;       (let ((rewrite (cond/clauses/translate (cdr e))))
;         (scheme/compile rewrite lexical)))
      
      ((when? e)
       (when/compile e lexical))
      
      ((unless? e)
       (unless/compile e lexical))
      
      ((begin? e)
       (begin/compile e lexical))
      
      ; (lambda <formals> <body>).
      ((lambda? e)
       (lambda/compile e lexical))
      
      ((and? e)
       (and/compile e lexical))
      
      ((or? e)
       (or/compile e lexical))
      
      ; Special forms for binding environments.
      
      ; (environ/cons <environ> <symbol_1> ... <symbol_n>)
      ((environ/cons? e)
       (environ/cons/compile e lexical))
      
      ; (environ/value <environ> <symbol> <substitute>)
      ((environ/value? e)
       (environ/value/compile e lexical))
      
      ; (environ/remove <environ> <symbol_1> ... <symbol_n>)
      ((environ/remove? e)
       (environ/remove/compile e lexical))
      
      ; (environ/reflect E e_1 ... e_n)
      ((environ/reflect? e)
       (environ/reflect/compile e lexical))
      
      ; The following three cases deal with quasiquotation.
      ((quasiquote? e)
       (quasiquote/compile e lexical))
      
      ((unquote? e)
       (unquote/compile e lexical))
      
      ((unquote-splicing? e)
       (unquote-splicing/compile e lexical))
      
      (else ; combination (f a_1 a_2 ... a_N)
       (combination/compile e lexical)))))

(define (macro/expand e lexical)
  (apply (lexical/macro/fetch lexical (car e)) (lambda (x) x) (cdr e)))

;; (define-macro (<name> <formals>) <body>).
;; Compile macro definition e and make it available in the current lexical scope.
;; As a convenience returns the lexical stack.
(define (macro/compile e lexical)
  (shape e 3)
  (let ((name    (definition/macro/name e))
        (formals (definition/macro/formals e))
        (body    (definition/macro/body e)))
    ; If a (define-macro (X <formals>) <body>) appears within the <body> B of e (itself a define-macro)
    ; then any subexpression of B matching (X ...) will be macro expanded at macro definition time
    ; (probably not what the macro writer had in mind).
    ; In other words, including local macro definitions within a (define-macro ...) is not the
    ; analogue of including local function definitions within the body of a lambda expression.
    ; On the other hand, local function definitions within the body of a define-macro have exactly the intended
    ; effect, the functions are local to the define-macro and are invoked at macro expansion time.
    (let ((procedure (scheme/compile `(lambda ,formals ,@body) #f)))
      (motile/call procedure environ/null))))  ; Evaluate the procedure at its point of definition.
      
(define (let/compile e lexical)
  (shape e 3)
  (let ((x (let/bindings e)))
    (cond
      ((symbol? x)
       ; Named let, (let <variable> <bindings> <body>), for example, (let loop ((i 0)) <body>).
       (shape e 4)
       ; Rewrite it as a letrec defining a single recursive procedure named by <variable>.
       (let* ((bindings    (let/named/bindings e))
              (variables   (let/bindings/variables bindings))
              (expressions (let/bindings/expressions bindings))
              (body        (let/named/body e))
              ; The procedure parameters are the <variables> in the <bindings> of the named let.
              ; The procedure <body> is the named let <body>.
              (procedure   `(lambda ,(let/bindings/variables bindings) ,@body)))
         (scheme/compile
          `((letrec ((,x ,procedure)) ,x) ,@expressions)
          lexical)))

      ((pair? x)
       ; (let <bindings> <body>).
       (let* ((variables   (let/bindings/variables x))
              (expressions (let/bindings/expressions x))
              (body        (let/body e)))
         (scheme/compile
          `((lambda ,variables ,@body) ,@expressions)
          lexical)))

      (else
       ; (let () <body>).
       (scheme/compile `((lambda () ,@(let/body e))) lexical)))))


(define (let*/compile e lexical)
  (shape e 3)
  (let ((bindings (let/bindings e))
        (body     (let/body e)))
    (if (pair? bindings)
        ; (let* <bindings> <body>).
        (scheme/compile
         `(let (,(car bindings)) (let* ,(cdr bindings) ,@body))
         lexical)

        ; (let* () <body>).
        (lambda/body/compile body lexical))))

(define (lambda/compile e lexical)
  (shape e 3)
  (let* ((parameters (lambda/parameters e))
         (flat       (parameters/extract parameters)) ; Even with no parameters (lambda () ...) still denotes a change in lexical scope.
         ; Set of closed variables in lambda body.
         (closed (variables/closed (lambda/body e) (lexical/push/parameters lexical flat)))
         ; Lexical address (frame . offset) of each closed variable at time of call.
         (addresses (map (lambda (s) (lexical/address lexical s)) closed))
         ; All code references to local and closed variables within the body are an offset within frame 0.
         (code (lambda/body/compile (lambda/body e) (lexical/push lexical flat closed))))
    ; Generate construction of the run time frame from the argument values and the closed variables.
    (if (null? addresses)
        (if (parameters/rest? parameters)
            (lambda/rest/generate (length flat) code)
            (lambda/generate      (length flat) code))
        (let ((a (list->vector addresses)))
          (if (parameters/rest? parameters)
              (closure/rest/generate (length flat) (vector-length a) a code)
              (closure/generate      (length flat) (vector-length a) a code))))))


;; Return #t if the list of formal parameters ends with a rest parameter,
;; say (lambda x ...) or (lambda (x y . z) ...), and #f if no rest parameter is present.
(define (parameters/rest? parameters)
  (cond
    ((pair? parameters) (parameters/rest? (cdr parameters)))
    ((null? parameters) #f)
    (else               #t)))

;; Given a list of formal parameters extract the identifiers as a list and verify
;; there are no duplicate identifiers in the list.
;; Returns the list of identifiers iff all parameters are identifiers and all parameters are unique,
;; otherwise raises an exception.
(define (parameters/extract parameters)
  ; Ensures that each of the formal parameters of a lambda expression is a valid symbol
  ; and if so returns them as a flat list otherwise it raises an error exception.
  ; The rest parameter, if any, appears as the last element of the list.
  ; Examples:
  ;   ()        => ()
  ;   a         => a
  ;   (a b c)   => (a b c)
  ;   (a b . c) => (a b c)
  (define (legal? parameters)
    (cond
      ((null? parameters) null)
      ((pair? parameters)
       (let ((x (car parameters)))
         (variable? x) ; Guarantee that each parameter is a symbol.
         (cons x (legal? (cdr parameters)))))
      (else
       (variable? parameters)
       (list parameters))))
  
  ; Given a sorted list of symbols determine if the list contains any duplicates.
  ; Return #f if no duplicates exist or a sample duplicate symbol.
  (define (duplicates? bag)
    (cond
      ((null? bag) #f)
      ((null? (cdr bag)) #f)
      ((eq? (car bag) (cadr bag)) (car bag)) ; Found a duplicate. Return a sample.
      (else (duplicates? (cdr bag)))))
  
  ; Given a flat list of identifiers guarantee that no two identifiers are duplicates.
  ; Return #t iff all identifiers are unique otherwise raise an error exception.
  (define (unique? bag parameters)
    (cond
      ((null? bag) #t)        ; Empty parameters list.
      ((null? (cdr bag)) #t)  ; Singleton list.
      ((duplicates? (set/sort bag))
       => (lambda (duplicate) (((error (format "Identifier duplicated: ~a in formals: ~a" duplicate parameters))))))
      (else #t)))
  
  (let ((bag (legal? parameters)))
    (unique? bag parameters)
    bag))
         
;; In an erudite thread on various letrec implementations and their pitfalls (noting, in particular,
;; that the macro definition of letrec in R5RS is incorrect as it does not permit one or more (define ...)
;; forms to appear as a prefix of the body of the letrec)
;; [http://groups.google.com/group/comp.lang.scheme/browse_thread/thread/141d47814c79776b/306ccb44cd2ab7e5?#306ccb44cd2ab7e5]
;; oleg@pobox.com, May 21, 2001 offers:
;; 
;;     Here's the definition of letrec that behaves correctly with respect to the above
;;     test, permits defines in the letrec body, is much simpler than the
;;     R5RS definition, permits an arbitrary order of evaluating the init expressions,
;;     and avoids creation of many closures and O(n) temporary names:
;;
;;     (define-syntax letrec
;;       (syntax-rules ()
;;         ((_ ((var init) ...) . body)
;;          (let ((var 'undefined) ...)
;;            (let ((temp (list init ...)))
;;              (begin (set! var (car temp)) (set! temp (cdr temp))) ...
;;              (let () . body))))))

;; In Mischief Letrec is implemented by the pseudo-code:
;;
;;    ((lambda (v_1 ... v_N)
;;       ((lambda (t_1 ... t_N)
;;          (setter N)
;;          (let () . <body>))
;;          e_1 ... e_N))
;;     #f ... #f)
;;
;; where:
;; v_i is the i'th letrec variable
;; e_i is the defining expresssion for variable v_i
;; (setter N) is a fragment of internal code that sets each variable v_i, i = 1, ..., N to the value of expression e_i
;; <body> is the body of the letrec.
;; #f is the initial junk value for each of the letrec variables v_i.

(define (lambda/body/compile body lexical)
  ;(display "lambda/body/compile\n")
  ;(pretty-display body)   (newline)
  (define (letrec/defines variables values body lexical)
    (if (pair? body)
        (let ((e (car body)))
          (cond
            ((not (pair? e))
             ; End of body.
             (letrec/defines* variables values body lexical))

            ((macro? e lexical)
             (letrec/defines variables values (cons (macro/expand e lexical) (cdr body)) lexical))

            ((begin? e)
             ; ((begin u_1 u_2 ... u_M) e_1 ... e_N) => (u1 u2 ... u_M e_1 ... e_N).
             (letrec/defines variables values (append (cdr e) (cdr body)) lexical))
            
            ((definition? e)
             ; (define <variable> <expression>) or (define (<variable> <formals>) <body>).
             (let ((x (definition/name e)))
               (letrec/defines (cons x variables) (cons (definition/value e) values) (cdr body) lexical)))

            ((definition/macro? e)
             ; (define-macro (name <formals>) <body>).
             (let ((x (definition/macro/name e)))
               (variable? x)
               (letrec/defines
                variables values (cdr body)
                (lexical/macro/push lexical x (macro/compile e lexical)))))

            (else
             ; No more definitions or begin expressions at this point in the body.
             (letrec/defines* variables values body lexical))))

        (error "Body must contain at least one expression")))

  ; If the lambda body contained one or more definitions at its head then translate the lambda body
  ; and compile it as a letrec otherwise simply compile the lambda body as a sequence of expressions.
  (define (letrec/defines* variables expressions body lexical)
    (if (null? variables)
        (sequence/compile body lexical) ; The body is free of definitions.

        ; As the body is prefixed by (define ...) forms rewrite it as a (letrec ...)
        ; and shove it through the compiler again.
        (scheme/compile
         (defines/letrec/rewrite variables expressions body)
         lexical)))
  
  (letrec/defines null null body lexical)) ; Start the sweep for (define ...) forms.

;; Rewrite the body of a lambda containing one or more (define ...) special forms
;; as a (letrec ...) and shove it back through the compiler.
(define (defines/letrec/rewrite variables expressions body)
  (let ((bindings (map list variables expressions))) ; Poor man's zip.
    `(letrec ,bindings ,@body)))

;; Internal macro expansion for all special forms.
(define (motile/macro/expand e macros)
  ; Return #t if x is a literal value and #f otherwise..
  (define (literal? x) (not (or (symbol? x) (pair? x))))
  ; Internal helper function for mapping the macro expander across expression sequences.
  (define (map/expand e) (motile/macro/expand e macros))

  (cond
    ((symbol? e) e)

    ((literal? e) e)

    ((quote? e) e)
    
    ((hash/ref macros (car e) #f)
     =>
     ; Macro expand expression e using an e-specifc macro and then recursively macro expand
     ; that result all over again.
     (lambda (m) (motile/macro/expand (m e) macros)))

    ((lambda? e)
     ; Run the macro expander over every expression in the lambda body.
     `(lambda ,(lambda/parameters e) ,@(map map/expand (lambda/body e))))

    ((if? e)
     `(if
       ,(motile/macro/expand (if/test e) macros)
       ,(motile/macro/expand (if/then e) macros)
       ,(motile/macro/expand (if/else e) macros)))

;    ((cond? e)
;     (if (null? (cdr e))
;         ; e is nothing more than (cond).
;         e
;         ; Macro expand each clause of the cond.
;         `(cond
;            ,@(map
;               (lambda (c)
;                 (cond
;                   ((cond/clause/else? c) ; (else <expressions>)
;                    `(else ,@(map map/expand (cdr c))))
;                   
;                   ((not (pair? (cdr c))) ; Clause c is just (<test>)
;                    `(,(motile/macro/expand (cond/clause/test c) macros))) ; Macro-expand <test>
;                   
;                   ((cond/clause/=>? c) ; (<test> => <procedure>)
;                    `(,(motile/macro/expand (cond/clause/test c) macros)
;                      ,(motile/macro/expand (cond/clause/procedure c) macros)))
;                   
;                   (else ; (<test> <expressions>)
;                    `(,(motile/macro/expand (cond/clause/test c) macros)
;                      ,@(map map/expand (cdr c))))))
;
;               (cdr e)))))

    ((when? e)
     `(when
        ,(motile/macro/expand (when/test e) macros)
        ,(map map/expand (when/thens e))))
         
    ((unless? e)
     `(unless
        ,(motile/macro/expand (unless/test e) macros)
        ,(map map/expand (unless/elses e))))

    ((begin? e)
     `(begin ,@(map map/expand (cdr e))))

    ((and? e)
     `(and ,@(map map/expand (cdr e))))

    ((or? e)
     `(or ,@(map map/expand (cdr e))))
    
    ; (environ/cons <environ> <symbol_1> ... <symbol_n>)
    ((environ/cons? e)
     `(environ/cons ,(motile/macro/expand (environ/cons/environ e) macros) ,@(environ/cons/identifiers e)))

    ; (environ/value <environ> <symbol> <substitute>)
    ((environ/value? e)
     `(environ/value
       ,(motile/macro/expand (environ/value/environ e) macros)
       ,(environ/value/accessor e)
       ,(motile/macro/expand (environ/value/substitute e) macros)))

    ; (environ/remove <environ> <symbol_1> ... <symbol_n>)
    ((environ/remove? e)
     `(environ/remove ,(motile/macro/expand (environ/remove/environ e) macros) ,@(environ/remove/symbols e)))

    ; (environ/reflect E e_1 ... e_n)
    ((environ/reflect? e)
     `(environ/reflect
       ,(motile/macro/expand (environ/reflect/environ e) macros)
       ,@(map map/expand (environ/reflect/expressions e))))

    (else (map map/expand e)))) ; Macro expand every element of s-expression e.
  
;; Macro translate a (letrec ...) into canonical nested lambdas plus some inline Motile code
;; for setting the letrec bindings properly.
(define (letrec/translate e)
  ;(pretty-display e) (newline)
  (let* ((bindings    (let/bindings e))
         (variables   (let/bindings/variables   bindings))
         (expressions (let/bindings/expressions bindings))
         (body        (let/body e))
         (rewrite
         `((lambda ,variables
             ((lambda ,(aliases/generate variables)
                ,(letrec/set/generate (length variables)) ; Inline Motile code.
                (let () ,@body))
              ,@expressions))
           ,@(build-list (length variables) (lambda (i) #f))))) ; Generate a list (#f ... #f) that is (length variables) wide.
    ;(display "letrec/translate: rewrite\n")
    ;(pretty-display rewrite) (newline)
    rewrite))

;; Macro translate a (let ...) into a (lambda ...) application.
(define (let/translate e)
  (shape e 3)
  (let ((x (let/bindings e)))
    (cond
      ((symbol? x)
       ; Named let, (let <variable> <bindings> <body>), for example, (let loop ((i 0)) <body>).
       (shape e 4)
       ; Rewrite it as a letrec defining a single recursive procedure named by <variable>.
       (let* ((bindings    (let/named/bindings e))
              (variables   (let/bindings/variables bindings))
              (expressions (let/bindings/expressions bindings))
              (body        (let/named/body e))
              ; The procedure parameters are the <variables> in the <bindings> of the named let.
              ; The procedure <body> is the named let <body>.
              (procedure   `(lambda ,(let/bindings/variables bindings) ,@body))
              (rewrite     `(letrec ((,x ,procedure)) (,x ,@expressions))))  ;`((letrec ((,x ,procedure)) ,x) ,@expressions)))
         rewrite))

      ((pair? x)
       ; (let <bindings> <body>).
       (let* ((variables   (let/bindings/variables x))
              (expressions (let/bindings/expressions x))
              (body        (let/body e))
              (rewrite     `((lambda ,variables ,@body) ,@expressions)))
         rewrite))

      (else
       ; (let () <body>).
       `((lambda () ,@(let/body e)))))))

;; Macro translate a (let* ...) into nested lambda applications.
(define (let*/translate e)
  (shape e 3)
  (let ((bindings (let/bindings e))
        (body     (let/body e)))
    (if (pair? bindings)
        ; (let* <bindings> <body>).
        `(let (,(car bindings)) (let* ,(cdr bindings) ,@body))

        ; (let* () <body>).
        `((lambda () ,@body)))))

;; Generate n unique variable names.
(define (aliases/generate variables)
  (let loop ((variables variables) (aliases null))
    (cond
      ((null? variables) (reverse aliases))
      (else
       (let ((base (string-append (symbol->string (car variables)) "."))) ; For example symbol alpha => "alpha."
         (loop (cdr variables) (cons (gensym base) aliases)))))))


(define (cond/translate x)
  (cond/clauses/translate (cdr x)))

(define (cond/clauses/translate clauses)
  ;(pretty-display clauses) (newline)
  (if (pair? clauses)
      (let ((clause (car clauses)))
        (shape clause 1)
        (cond
          ((cond/clause/else? clause) ; (else <expression> ...)
           (shape clause 2)
           (if (null? (cdr clauses))
               ; The (else ...) clause is the last cond clause.
               `(begin ,@(cdr clause)) ; Return (<expression> ...).
               (error (format "<else> clause ~a is not last clause in cond" clause))))

          ((not (pair? (cdr clause))) ; (<test>).
           `(or ,(cond/clause/test clause) ,(cond/clauses/translate (cdr clauses))))

          ((cond/clause/=>? clause) ; (<test> => <procedure>)
           (shape clause 3)
           (let ((test      (cond/clause/test clause))
                 (procedure (cond/clause/procedure clause))
                 (else      (cdr clauses))
                 (outcome   (gensym "outcome.")))
             `(let ((,outcome ,test))
                (if ,outcome (,procedure ,outcome) ,(cond/clauses/translate else)))))

          (else ; (<test> <expression> ...)
           (let ((test (cond/clause/test clause))
                 (else  (cdr clauses)))
           `(if ,test (begin ,@(cdr clause)) ,(cond/clauses/translate else))))))
        
      #f)) ; (cond). In R5RS the return value for this degenerate case is unspecified. We chose #f.

;; *** END compile (cond <clauses>). ***

;(define (cond/=>/generate test procedure else)
;  (lambda (rtk rte)
;    (test
;     (lambda (x)
;       (if x
;           ; The procedure target of the => is either a literal lambda expression or a variable.
;           ; In either case it must be evaluated in the context of the run time environment of the
;           ; cond clause. In the case of the literal lambda expression that evaluation yields the
;           ; closure (lambda (rtk x) ...) that is the generated code of the lambda expression.
;           ; In this case, the evaluation amounts to the evaluation of the literal lambda expression at
;           ; its point of defintiion.
;           ; In the second case, where a variable is the target of the => then the initial evaluation is
;           ; a dereference of the variable, again yielding a closure (lambda (rtk x) ...), for some lambda expression.
;           ; In all cases we apply the closure to two arguments, the continuation in effect at the cond clause and
;           ; the outcome, x, of the evaluation of the test of the cond clause.
;           ((procedure (lambda (p) p) rte) rtk x)
;           (else rtk rte)))
;     rte)))

;; Rewrite
;;(case <key>
;;  (<datums>_1 <expressions>_1)
;;  ...
;;  (<datums>_N <expressions>_N)
;;  (else <expressions>))
;;
;; AS
;;
;;(let ((k <key>))
;;  (cond
;;    ((memv k '<datums>_1) <expressions>_1)
;;    ...
;;    ((memv k '<datums>_N) <expressions>_N)
;;    (else <expressions)))

(define (case/clauses/translate key clauses)
  (if (pair? clauses)
      (let ((clause (car clauses)))
        (shape clause 2)
        (cond
          ((case/clause/datums? clause)
           (cons `((memv ,key (quote ,(case/clause/datums clause))) ,@(case/clause/expressions clause))
                 (case/clauses/translate key (cdr clauses))))
          ((case/clause/else? clause)
           (unless (null? (cdr clauses))
             (error (format "(else ...) clause appears in middle of case expression: ~a" clauses)))
           clauses)
          (else
           (error (format "unknown clause in case expression: ~a" clauses)))))
      null))
            
(define (case/translate e)
  (let ((key (gensym 'key.)))
    `(let ((,key ,(case/key e)))
       (cond ,@(case/clauses/translate key (case/clauses e))))))

;; A do binding is (<variable> <init> <step>) or (<variable> <init>).
(define (do/bindings/unzip bindings)
  (let loop ((bindings bindings) (variables null) (initials null) (steps null))
    (if (null? bindings)
        (list (reverse variables) (reverse initials) (reverse steps))

        (let* ((binding (car bindings))
               (n (length binding)))
          (cond
            ((= n 2) ; (<variable> <initial>)
             (variable? (do/binding/variable binding))
             (loop
              (cdr bindings) 
              (cons (do/binding/variable binding) variables)
              (cons (do/binding/initial binding) initials)
              (cons (do/binding/variable binding) steps)))
            ((= n 3) ; (<variable> <initial> <step>)
             (variable? (do/binding/variable binding))
             (loop
              (cdr bindings)
              (cons (do/binding/variable binding) variables)
              (cons (do/binding/initial binding) initials)
              (cons (do/binding/step binding) steps)))
            (else
             (error (format "Ill-formed binding ~a in bindings ~a of (do ...)" binding bindings))))))))

(define (do/translate e)
  (let ((bindings (do/bindings e))
        (test (do/test e))
        (commands (do/commands e))
        (loop (gensym 'loop/)))
    (let* ((unzip (do/bindings/unzip bindings))
           (variables (car unzip))
           (initials  (cadr unzip))
           (steps     (caddr unzip))
           (test/expressions (do/test/expressions test)))
      `(let ,loop ,(map list variables initials) ; Poor man's zip.
         (if ,(car test)
             ,(if (null? test/expressions) (void) `(begin ,@test/expressions))
             (begin
               ,@commands
               (,loop ,@steps)))))))

;; A persistent hash table of macros internal to the Motile compiler for
;; the translation of special forms.
(define MACROS/INTERNAL
  (vector/hash
   hash/eq/null
   (vector
    'case   case/translate
    'cond   cond/translate
    'do     do/translate
    'letrec letrec/translate
    'let    let/translate
    'let*   let*/translate
    'match  match/translate
     ))
   )

(define (and/compile e lexical)
  (let ((rest (cdr e)))
    (if (pair? rest)
        (and/rest/compile rest lexical)
        (constant/generate #t))))

(define (and/rest/compile rest lexical)
  (let ((code (scheme/compile (car rest) lexical))
        (tail (cdr rest)))
    (if (pair? tail)
        (and/generate code (and/rest/compile tail lexical))
        code)))

(define (or/compile e lexical)
  (let ((rest (cdr e)))
    (if (pair? rest)
        (or/rest/compile rest lexical)
        (constant/generate #f))))

(define (or/rest/compile rest lexical)
  (let ((code (scheme/compile (car rest) lexical))
        (tail (cdr rest)))
    (if (pair? tail)
        (or/generate code (or/rest/compile tail lexical))
        code)))


;; Parsing and code generation for quasiquotation.

(define (quasiquote/compile x lexical)         ; x is expression (quasiquote <object>)
  (quasiquotation/compile (cadr x) 1 lexical)) ; (cadr x) is <object>.

;; form: taken from (quasiquote <form>)
;; level: 0, outside the quasiquotation
;;        1, within the outmost quasiquotation
;;        > 1, within an inner (nested) quasiquotation
;; lexical: the compile-time lexical scope.
(define (quasiquotation/compile form level lexical)
  (cond
    ((= level 0)
     ; We are not inside a quasiquotation.
     (scheme/compile form lexical))
    
    ((pair? form)
     (cond
       ((eq? (car form) 'quasiquote)
        ;; We've descended into another level of quasiquotation.
        (quasiquotation/list/compile form (add1 level) lexical))
       
       ((eq? (car form) 'unquote) ; form is (unquote <object>) or more commonly ,<object>.
        (if (= level 1)
            ; Compile <object> as we are situated at the outermost quasiquotation.
            (scheme/compile (cadr form) lexical)
            (quasiquotation/list/compile form (sub1 level) lexical))) ; 
       
       ((eq? (car form) 'unquote-splicing) ; form is (unquote-splicing <object>) or more commonly ,@<object>.
        (when (= level 1)
          (error (format "Ill-placed 'unquote-splicing' ~a" form)))
        (quasiquotation/list/compile form (sub1 level) lexical))
       
       (else
        (quasiquotation/list/compile form level lexical))))
    
    ((vector? form)
     (quasiquote/tuple/generate
      (quasiquotation/list/compile (vector->list form) level lexical)))
    
    (else
     (constant/generate form))))

(define (quasiquotation/list/compile l level lexical)
  (if (pair? l)
    (let ((first (car l)))
      (if (= level 1)
        (if (and (pair? first) (unquote-splicing? first))
          (begin
            (shape first 2)
            (quasiquote/append/generate
             (scheme/compile (cadr first) lexical)
             (quasiquotation/compile (cdr l) 1 lexical)))

          (quasiquote/cons/generate
           (quasiquotation/compile first level lexical)
           (quasiquotation/compile (cdr l) level lexical)))

        ; level > 1.
        (quasiquote/cons/generate
         (quasiquotation/compile first level lexical)
         (quasiquotation/compile (cdr l) level lexical))))

    (quasiquotation/compile l level lexical)))


;; Compilation of environ/... special forms.

;; MUST implement restriction to lexical scope identifiers only !!!

; (alphabet <environ> <identifier> ... <identifier>).
(define (environ/cons/compile e lexical)
  (shape e 3)
  (let ((x (environ/cons/environ e)) ; <environ> expression
        (identifiers (environ/cons/identifiers e))) ; <identifier> ... <identifier>
    (if (andmap symbol? identifiers)
        
        ; In addition every <identifier> must be declared within lexical scope.
        (if (andmap (lambda (s) (lexical/variable? lexical s)) identifiers)
            (let ((environ/code ; The closure to evaluate the <environ> expression.
                   (scheme/compile x lexical))
                  (values/code  ; The closures for each identifier binding in lexical scope.
                   (map (lambda (identifier) (scheme/compile identifier lexical)) identifiers)))
              (environ/cons/generate environ/code (list->vector identifiers) (list->vector values/code)))
            
            (error (format "All symbols ~a in ~a must be in lexical scope" identifiers e)))

        ; Some <identifier> is not a literal symbol.
        (error (format "Non-symbol(s) appear in arguments ~a to ~a" identifiers e)))))

;; A "reader path" is an immutable vector of symbols that was read by the Racket reader,
;; for example, #(foo bar nix) is a reader path while (vector-immutable 'foo 'bar 'nix) is not
;; as the reader will not immediately read it as a constant vector.
(define (reader/path? x)
  (and
   (vector? x)
   (immutable? x)
   (positive? (vector-length x))
   (vector/all? x symbol?)))

;; (environ/value <environ> <symbol> <substitute>).
(define (environ/value/compile e lexical)
  (shape e 4)
  (let ((environ    (environ/value/environ e))
        ;(symbol     (environ/value/identifier e))
        (accessor   (environ/value/accessor e))
        (substitute (environ/value/substitute e)))
    (let ((generator
           (cond
             ((symbol? accessor)      environ/symbol/value/generate)
             ((reader/path? accessor) environ/path/value/generate)
             (else (error (format "Expected symbol or reader/path for accessor in ~a but got ~a instead" e accessor))))))
      (generator (scheme/compile environ lexical) accessor (scheme/compile substitute lexical)))))

;; (environ/remove <environ> <symbol_1> ... <symbol_n>)
(define (environ/remove/compile e lexical)
  (shape e 3)
  (let ((x (environ/remove/e e)) ; <environ>
        (symbols (environ/remove/symbols e)))
    (if (andmap symbol? symbols)
        (let ((environ/code (scheme/compile x lexical)))
          (environ/remove/generate environ/code (list->vector symbols)))
        (error "Illegal identifier in ~s" e))))

(define (environ/reflect/compile e lexical)
  (shape e 2)
  (let ((global/code (scheme/compile (environ/reflect/environ e) lexical))
        (body        (sequence/compile (environ/reflect/expressions e) lexical)))
    (environ/reflect/generate global/code body)))

;; Compile (if <test> <then> <else>).
(define (if/compile e lexical)
  (shape e 4)
  (let ((test (scheme/compile (if/test e) lexical))
        (then (scheme/compile (if/then e) lexical))
        (else (scheme/compile (if/else e) lexical)))
    (if/generate test then else)))
    
(define (when/compile e lexical)
  (shape e 3)
  (when/generate
   (scheme/compile (when/test e)  lexical)
   (scheme/compile (when/thens e) lexical)))

(define (unless/compile e lexical)
  (shape e 3)
  (unless/generate
   (scheme/compile (unless/test e) lexical)
   (scheme/compile (unless/elses e) lexical)))
          
(define (begin/compile e lexical)
  (shape e 2)
  (sequence/compile (cdr e) lexical))

;; Map f over a list of elements in list order return the map as a vector.
(define (map/vector f elements)
  (let ((v (make-vector (length elements))))
    (let loop ((i 0) (elements elements))
      (cond
        ((null? elements) v)
        (else
         (vector-set! v i (f (car elements)))
         (loop (add1 i) (cdr elements)))))))

(define (sequence/compile expressions lexical)
  (let ((elements (map/vector (lambda (e) (scheme/compile e lexical)) expressions))) ; Compile each expression e_i in the sequence.
    (sequence/generate (vector-length elements) elements))) ; Combine the elements into a single code (lambda (rtk rte) ...).

;; Return #t if symbol is a lambda parameter in the topmost lexical frame and #f otherwise.
(define (lexical/variable/parameter? lexical symbol)
  (and lexical (set/member? symbol (lexical/frame/parameters lexical))))

;; Return #t if symbol is a closed variable in the topmost lexical frame and #f otherwise.
;; lexical: lexical environment stack
;; symbol: variable appearing in an expression
(define (lexical/variable/closed? lexical symbol)
  (and lexical (set/member? symbol (lexical/frame/closed lexical))))

;; From the contents of the lexical stack calculate the frame offset in the topmost frame
;; of the run-time stack) of the argument named by symbol.
;; lexical - lexical stack
;; symbol - name of argument
(define (reference/local/offset lexical symbol)
  (let* ((arguments (lexical/frame/parameters lexical))
         (tail      (memq symbol arguments)))
    (add1 (- (length arguments) (length tail)))))

;; From the contents of the lexical stack calculate the frame offset (in the topmost frame of
;; the run-time stack) of the closed variable named by symbol.
;; lexical - lexical stack
;; symbol - name of closed variable
(define (reference/closed/offset lexical symbol)
  (let* ((arguments (lexical/frame/parameters lexical)) ; (a_1 a_2 ... a_m)
         (closed    (lexical/frame/closed lexical))     ; (c_1 c_2 ... c_n)
         (tail      (memq symbol closed)))              ; (c_i ... c_n)
    (+ 1 (length arguments) (- (length closed) (length tail))))) ; Offset from start of run-time frame.

;; Was thinking of converting the list of combination arguments to a vector but now I'm not so sure.
;; I'm just going to leave it as is for the time being - 2011.08.02.

(define VECTOR/EMPTY #())

;; A version of map that returns a vector rather than a list.
(define (list/vector/map f items)
  (if (null? items)
      VECTOR/EMPTY
      (let loop ((v (make-vector (length items))) (i 0) (items items))
        (cond
          ((null? items) v)
          (else
            (vector-set! v i (f (car items)))
            (loop v (add1 i) (cdr items)))))))

(define (combination/compile e lexical)
  (let ((operator (car e))
        (arguments (map (lambda (a) (scheme/compile a lexical)) (cdr e))))
    (cond
      ((symbol? operator)
       (cond
         ((lexical/variable/parameter? lexical operator)
          (combination/generate
           (variable/get/generate (reference/local/offset lexical operator))  ; Reference to an argument.
           arguments))
         
         ((lexical/variable/closed? lexical operator)
          (combination/generate
           (variable/get/generate (reference/closed/offset lexical operator)) ; Reference to a closed variable.
           arguments))

         (else
          (combination/generate (global/get/generate operator) arguments))))  ; Reference to a global variable.

      ((pair? operator) ; Operator is a complex expression though not necessarily appropriate for this position.
       (combination/generate (scheme/compile operator lexical) arguments))

      (else ; Must be a constant.
       (error (format "Inappropriate operator ~a in combination" e))))))


(define (unquote/compile e lexical)
  (error (format "Ill-placed unquote ~a" e)))

(define (unquote-splicing/compile e lexical)
  (error (format "Ill-placed unquote-splicing: ~a" e)))

;; The trick employed here for closure generation, namely appending the values of the closed variables
;; to the environment frame of the argument values for the lambda expression, was described in
;; Marc Feeley and Guy Lapalme, "Closure generation based on viewing LAMBDA as EPSILON plus COMPILE,"
;; Journal of Computer Languages, 17(4), pp 251-267, 1992.
;; This allows us to represent a lexical closure as a Scheme procedure object rather than as a vector
;; or record structure and requires only one uniform reference mechanism, namely indexed access to the most
;; recent frame, for both the argument bindings and the values of closed variables.

;; Letrec allows a closure to refer to itself, the essence of a recursive function.
;; However, at the point of definition of a recursive function F, where the (lambda (rtk rte) ...) code of the closure of F
;; is first evaluated, the binding for F is undefined. Thus F, if it obtains its closure bindings at the time of definition,
;; will discover, at its point of application, that its recursive self reference in the closure bindings is undefined.
;; The solution is to delay the construction of closure bindings until after the letrec bindings have been defined.


;; Suppose we have:
;; (let ((f (lambda (a_1 ... a_m) <lambda-body>))
;;       ...)
;;   <let-body>)
;; where the lexical variable f is being bound to the definition of a lambda expression (lambda (a_1 ... a_m) ...).
;; The compiler generates a definition-closure, f_D, that accepts two arguments, the run-time continuation at the point of definition (rtk)
;; and the run-time lexical scope binding environment at the point of definition (rte).

;; Here the compiler must bind f to a closure A that, when executed at the point of definition of f, returns a second closure B
;; constituting the executable for (lambda (a_1 ... a_m) <lambda-body>).
;; The closure A = (lambda (rtk rte) ...) where rtk and rte are arguments for the run-time continuation and
;; environment respectively at the point of definition.

;; Irrespective of the number of arguments, m >= 0, of the lambda expression or the number of closed variables, n >= 0,
;; appearing in the lambda-body, the compiler ALWAYS generates a specialized TWO-argument closure for the lambda-body
;; where the first argument is the continuation k at the point of application of the lambda-body and the second argument
;; is the topmost frame of the run-time stack. This frame is an 1 + m + n length vector v where:
;;   v[0] is the run-time environment at the point of DEFINITION
;;
;;   v[1] ... v[m] contain the m argument values for arguments a_1 ... a_m respectively at the point of call, and
;;
;;   v[m+1] ... v[m+n] contain the values for the n closed variables v_1 ... v_n appearing in the lambda-body. These
;;   values were captured at the point of definition of the lambda expression.

;; Let body = (lambda (k frame) ...) represent the compiler-generated two-argument closure for some lambda-body.
;; By convention k may either be continuation (itself a function of one argument) or #f.
;; If k is a procedure then the following steps are taken:
;;   * An empty frame is allocated containing 1 + m + n slots
;;   * Slot 0 of the frame is set to the run-time environment captured at the point of DEFINITION
;;   * If the n >= 0 closure bindings have not yet been captured then a vector of length n is allocated and the bindings
;;     are fetched from the run-time environment at the point of definition
;;   * The n >= 0 closure bindings are copied into slots m+1, ..., m+n of the frame
;;   * The body closure applied to the the procedure (continuation) k and the frame constructed above.
;;
;; If k is #f then we are either generating the descriptor 


;; We first describe the case where f is defined as (lambda () <lambda-body>), that is, where f is a zero-argument function whose
;; lambda body contains N > 0 closed variables. The closure A when called with the run-time continuation and environment at the
;; point of definition must return a closure B whose closed variables captured the values available at the point of definition.
;; Closure B is a variable argument closure accepting either one or two arguments. We discuss each of these cases:
;;   (k)
;;   Here the argument k is the continuation at the point of application of f.


;(define (test/closure)
;  (define (test/closure/1)
;    (let ((code
;           (motile/compile
;            '(let ((f (lambda (n) (lambda () n))))
;               (let ((f100 (f 100))
;                     (f200 (f 200))
;                     (f300 (f 300)))
;                 (list (f100) (f200) (f300)))))))
;      (pretty-display (motile/start code BASELINE))))
;
;  (define (test/closure/2)
;    (let* ((code
;           (motile/compile
;            '(let ((f (lambda (n) (lambda () n))))
;               (let ((f100 (f 100))
;                     (f200 (f 200))
;                     (f300 (f 300)))
;                 (vector  f100 f200 f300)))))
;           (closures (motile/start code ENVIRON/TEST)))
;      (pretty-display (motile/decompile (vector-ref closures 0)))
;      (newline)
;      (pretty-display (motile/decompile (vector-ref closures 1)))
;      (newline)
;      (pretty-display (motile/decompile (vector-ref closures 1)))))
;  
;  (test/closure/1)
;  (test/closure/2))
;
;(define (test/map/1)
;  (let ((code
;         (motile/compile
;          '(let ((f (lambda (n) (+ n 3))))
;             (map f '(5 10 15 20 25))))))
;    (motile/start code BASELINE)))
;
;(define (test/map/2)
;  (let ((code
;         (motile/compile
;          '(let ((f (lambda (n) (+ n 3))))
;             (map f '(5 10 15 20 25))))))
;    (pretty-display (motile/decompile code))))
;
;(define (test/for-each)
;  (let ((code
;         (motile/compile
;          '(let ((f (lambda (n)
;                      (display (+ n 3)) (newline))))
;             (for-each f '(5 10 15 20 25))))))
;    (motile/start code ENVIRON/TEST)))
;
; (define (test/apply)
;  (let ((code
;         (motile/compile
;          '(apply + 5 10 15 20 25))))
;    (motile/start code BASELINE)))
; 
; (define (test/sort)
;   (let ((code
;          (motile/compile
;           '(sort '(25 20 5 15 10) <))))
;     (motile/start code BASELINE)))
