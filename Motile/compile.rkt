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

(require
 srfi/1
 (only-in racket/vector vector-copy vector-map))
(require
 "baseline.rkt"
 "dissect.ss"
 "free.ss"
 (only-in
  "persistent/hash.rkt"
  hash/eq/null
  hash/ref
  vector/hash)
 "set.ss")

(require racket/pretty) ; For testing only.
(require "persistent/environ.rkt") ; Testing only at this point 2011.06.16

(provide
 mischief/compile
 mischief/decompile
 mischief/start
 motile/compile
 motile/decompile
 motile/start
 motile/start*
  
 ; Code generation (for the recompiler)
 and/generate
 closure/inner/generate
 closure/generate
 closure/rest/inner/generate
 closure/rest/generate
 combination/generate
 constant/generate
 environ/cons/generate
 environ/reflect/generate
 environ/remove/generate
 environ/value/generate
 if/generate
 lambda/inner/generate
 lambda/generate
 lambda/rest/inner/generate
 lambda/rest/generate
 or/generate
 reference/global/variable/generate
 sequence/generate
 setter/generate
 unless/generate
 when/generate
 )

;; A closure is a function that captures the bindings of free variables in its lexical context.

;; The "rte" (run-time environment) contains all lexical scope bindings in view at run-time.
;; It is represented as an n > 0 length vector v where v[1] ... v[n-1] represent the values for the n-1 variables in the
;; topmost (most recent) lexical scope and v[0] is ALWAYS the prior rte.
;; A value of #f for v[0] represents the bottom (empty frame) of the run-time environment.
(define (rte/push rte arguments) (list->vector (cons rte arguments)))
(define (rte/pop rte) (vector-ref rte 0))
(define-syntax-rule (e/pop e) (vector-ref e 0))

(define (evaluate e)
  (let ((closure (compile `(lambda () ,e))))
    ((closure rtk/RETURN RTE) rtk/RETURN RTE)))

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
          (reference/variable/parameter/generate lexical e)) ; local variable (Lambda parameter) in lambda body.
         ((lexical/variable/closed? lexical e)
          (reference/variable/closed/generate lexical e))    ; Closed variable in lambda body.
         ((eq? e 'null)
          (constant/null/generate))                          ; The constant, null.
         (else
          (reference/global/variable/generate e))))          ; Global variable.
      
      ((procedure? e) e) ; Inline code.
      
      ((not (pair? e))
       (constant/generate e))
      
      ((macro? e lexical)
       (scheme/compile (macro/expand e lexical) lexical))
      
      ; The three forms of lex expressions.
      ; Our implementation is essentially macro expansion into lambda application so
      ; all expansion must preceed lambda compilation.
      ;    ((let? e)
      ;     (let/compile e lexical))
      ;    
      ;    ((let*? e)
      ;     (let*/compile e lexical))
      ;    
      ;    ((letrec? e)
      ;     (motile/letrec/compile e lexical))
      
      ((quote? e)
       (constant/generate (quotation e)))
      
      ((if? e)
       (if/compile e lexical))
      
      ((cond? e)
       (cond/compile e lexical))
      
      ;    ((case? e)
      ;     (case/compile e lexical))
      ;    
      ;    ((do? e)
      ;     (do/compile e lexical))
      
      ((when? e)
       (when/compile e lexical))
      
      ((unless? e)
       (unless/compile e lexical))
      
      ((begin? e)
       (begin/compile e lexical))
      
      ; (lambda <formals> <body>).
      ((lambda? e)
       (lambda/compile e lexical))
      
      ;    ; (let ((s_1 v_1) ... (s_N v_N)) <body>).
      ;    ((let? e)
      ;     (let/compile e lexical))
      ;    
      ;    ((let*? e)
      ;     (let*/compile e lexical))
      ;    
      ;    ((letrec? e)
      ;     (motile/letrec/compile e lexical))
      
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
      (start procedure))))  ; Evaluate the procedure at its point of definition. !!! Fix this to reflect ENVIRONMENT passing !!!
      
(define (letrec? e) (eq? 'letrec (car e)))

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
       ;(lambda/body/compile (let/body e) lexical)))))

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

(define (rte/depth rte)
  (let loop ((rte rte) (i 1))
    (if (vector-ref rte 0)
        (loop (rte/pop rte) (add1 i))
        i)))

;; (map f l) in continuation-passing style where k is the continuation of the map.
(define (map/k f l k)
  (if (null? l)
      (k null)

      (f
       (car l)
       (lambda (v1) (map/k f (cdr l) (lambda (v2) (k (cons v1 v2))))))))

;; Generate the custom apply code required for the letrec setter.
;; The reason for this structure is that during evaluation of the letrec bindings a binding expression may invoke call/cc
;; hence, we must be capable of creating a proper continuation at any point in setting the letrec bindings.
;(define (letrec/setter/apply/generate setter arguments)
;  (lambda (rtk rte) ; rtk is the continuation for the application.
;    (map/k
;     (lambda (a k) (a k rte)) ; Evaluate argument a_i within the lexical scope of the letrec.
;     arguments
;     (lambda (values) (setter rtk rte values)))))

;; (let* ((b_1 v_1) ... (b_N v_N)) <body>)
(define (let*? e) (eq? 'let* (car e)))

;; Returns #t if e has form (define ...) and #f otherwise.
(define (definition? e) (and (pair? e) (eq? 'define (car e))))
;; Returns 3t if e has form (define <name> ...) and #f otherwise.
(define (definition/variable? e) (symbol? (cadr e))) 
(define (definition/lambda? e) (pair? (cadr e)))

(define (definition/variable/ok? e) ; (define <variable> <expression>).
  (shape e 3)
  (definition/variable? e))

(define (definition/variable/name e) (cadr e))
(define (definition/variable/expression e) (caddr e))

; Pick apart (define (<name> <formals>) <body>).
(define (definition/lambda/name e) (caadr e)) 
(define (definition/lambda/formals e) (cdadr e))
(define (definition/lambda/body e) (cddr e))

;; Pick apart (define-macro (<name> <formals>) <body>).
(define (definition/macro/name e) (caadr e)) 
(define (definition/macro/formals e) (cdadr e))
(define (definition/macro/body e) (cddr e))

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
        (if (parameters/rest? parameters)
            (closure/rest/generate (length flat) (length addresses) addresses code)
            (closure/generate      (length flat) (length addresses) addresses code)))))


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
      ((eq? (first bag) (second bag)) (first bag)) ; Found a duplicate. Return a sample.
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

;; Return #t if the given symbol is defined within lexical scope and #f otherwise.
;(define (lexical/scope? lexical symbol)
;  (let loop ((lexical lexical))
;    (cond
;      ((null? lexical) #f)
;      ((memq symbol (lexical/frame lexical)) #t)
;      (else (loop (lexical/pop lexical))))))

;; Code generation for the body of a lambda expression.
;; Returns a closure #(code addresses void ... void) for the lambda body.
(define (lambda/body/compile body lexical)
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
               (variable? x)
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
        ; The body is free of definitions.
        (sequence/compile body lexical)
        ; The body is prefixed by one or more definitions.
        ;(scheme/compile (letrec/exploded/translate variables expressions body) lexical)))
        (letrec/generate variables expressions body lexical)))
  
  (letrec/defines null null body lexical))
         
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
;;
;; N = 1, 2, 3 are compiled as special cases for the sake of speed and to eliminate consing where possible.
;; N > 3 is compiled as a general case.

(define (letrec/compile e lexical)
  (let* ((bindings (let/bindings e))
         (variables (let/bindings/variables bindings))
         (expressions (let/bindings/expressions bindings))
         (body (let/body e)))     
    (letrec/generate variables expressions body lexical)))


(define (lambda/body/translate body lexical)
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
               (variable? x)
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
        ; The body is free of definitions.
        (sequence/compile body lexical)
        ; The body is prefixed by one or more definitions.
        ;(scheme/compile (letrec/exploded/translate variables expressions body) lexical)))
        (letrec/generate variables expressions body lexical)))
  
  (letrec/defines null null body lexical))


(define (motile/macro/expand e macros)
  (define (literal? x) (not (or (symbol? x) (pair? x))))
  (define (map/expand e) (motile/macro/expand e macros))

  (cond
    ((symbol? e) e)

    ((literal? e) e)

    ((hash/ref macros (car e) #f)
     =>
     ; Macro expand expression e using an e-specifc macro and then recursively macro expand
     ; that result all over again.
     (lambda (m) (motile/macro/expand (m e) macros)))

    ((quote? e) e)

    ((lambda? e)
     ; Run the macro expander over every expression in the lambda body.
     `(lambda ,(lambda/parameters e) ,@(map map/expand (lambda/body e))))

    ((if? e)
     `(if
       ,(motile/macro/expand (if/test e) macros)
       ,(motile/macro/expand (if/then e) macros)
       ,(motile/macro/expand (if/else e) macros)))

    ((cond? e)
     (if (null? (cdr e))
         ; e is nothing more than (cond).
         e
         ; Macro expand each clause of the cond.
         `(cond
            ,@(map
               (lambda (c)
                 (cond
                   ((cond/clause/else? c) ; (else <expressions>)
                    `(else ,@(map map/expand (cdr c))))
                   
                   ((not (pair? (cdr c))) ; Clause c is just (<test>)
                    `(,(motile/macro/expand (cond/clause/test c) macros))) ; Macro-expand <test>
                   
                   ((cond/clause/=>? c) ; (<test> => <procedure>)
                    `(,(motile/macro/expand (cond/clause/test c) macros)
                      ,(motile/macro/expand (cond/clause/procedure c) macros)))
                   
                   (else ; (<test> <expressions>)
                    `(,(motile/macro/expand (cond/clause/test c) macros)
                      ,@(map map/expand (cdr c))))))

               (cdr e)))))

    ((when? e)
     `(when
        (motile/macro/expand (when/test e) macros)
        ,@(map map/expand (when/thens e))))
         
    ((unless? e)
     `(unless
        (motile/macro/expand (unless/test e) macros)
        ,@(map map/expand (unless/elses e))))

    ((begin? e)
     `(begin ,@(map map/expand (cdr e))))

    ((and? e)
     `(and ,@(map map/expand (cdr e))))

    ((or? e)
     `(and ,@(map map/expand (cdr e))))
    
    ; (environ/cons <environ> <symbol_1> ... <symbol_n>)
    ((environ/cons? e)
     `(environ/cons ,(motile/macro/expand (environ/cons/environ e) macros) ,@(environ/cons/identifiers e)))

    ; (environ/value <environ> <symbol> <substitute>)
    ((environ/value? e)
     `(environ/value
       ,(motile/macro/expand (environ/value/environ e) macros)
       ,(environ/value/identifier e)
       ,(motile/macro/expand (environ/value/substitute e) macros)))

    ; (environ/remove <environ> <symbol_1> ... <symbol_n>)
    ((environ/remove? e)
     `(environ/remove ,(motile/macro/expand (environ/remove/environ e)) ,@(environ/remove/symbols e)))

    ; (environ/reflect E e_1 ... e_n)
    ((environ/reflect? e)
     `(environ/reflect
       ,(motile/macro/expand (environ/reflect/environ e) macros)
       ,@(map map/expand (environ/reflect/expressions e))))

    (else (map map/expand e)))) ; Macro expand every element of s-expression e.

(define-syntax-rule (define? e) (eq? 'define (car e)))
(define-syntax-rule (define/name e) (cadr e))
(define-syntax-rule (define/body e) (cddr e))

;; Extract all of the (define ...) expressions from the head of sequence body
;; returning (head . tail) where:
;; head is the maximal list of (define ...) expressions such that body = (append head tail)
;; and tail is nonempty.
    ;; If the tail turns out empty then an error is thrown.
    ;; (error 'scheme/compile "body ~a contains NO expression" body))
(define (defines/extract body)
  (let loop ((body body) (defines null))
    (cond
      ((null? body) #f) ; body is ill-constructed as it is NOT terminated by at least one expression.
      ((and (pair? (car body)) (define? (car body)))
       (loop (cdr body) (cons (car body) defines)))
      (else (cons defines body)))))

;; Convert (define x ...)       => (x ...) where x is a symbol.
;; Convert (define (x ...) ...) => (x (lambda (...) ...) where x is a symbol.
;(define (define/binding d)
  


(define (letrec/translate e)
  (let* ((bindings (let/bindings e))
         (variables (let/bindings/variables bindings))
         (expressions (let/bindings/expressions bindings))
         (body (let/body e))
         (rewrite
         `((lambda ,variables
             ((lambda ,(aliases/generate variables)
                ,(motile/setter/generate (length variables))
                (let () ,@body))
              ,@expressions))
           ,@(build-list (length variables) (lambda (_) #f)))))
    rewrite))

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

(define (let*/translate e)
  (shape e 3)
  (let ((bindings (let/bindings e))
        (body     (let/body e)))
    (if (pair? bindings)
        ; (let* <bindings> <body>).
        `(let (,(car bindings)) (let* ,(cdr bindings) ,@body))

        ; (let* () <body>).
        `((lambda () ,@body)))))

       

(define (motile/letrec/compile e lexical)
  (let* ((bindings (let/bindings e))
         (variables (let/bindings/variables bindings))
         (expressions (let/bindings/expressions bindings))
         (body (let/body e)))     
    (motile/letrec/generate variables expressions body lexical)))

;; Generate n unique variable names.
(define (aliases/generate variables)
  (let loop ((variables variables) (aliases null))
    (cond
      ((null? variables) (reverse aliases))
      (else
       (let ((base (string-append (symbol->string (car variables)) "."))) ; For example alpha => "alpha."
         (loop (cdr variables) (cons (gensym base) aliases)))))))

(define (motile/letrec/generate variables expressions body lexical)
  (let ((rewrite
         `((lambda ,variables
             ((lambda ,(aliases/generate variables)
                ,(motile/setter/generate (length variables))
                (let () ,body))
              ,@expressions))
           ,@(build-list (length variables) (lambda (_) #f)))))
    (scheme/compile rewrite lexical)))

    

(define (letrec/generate variables expressions body lexical)
  (let ((n (length variables)))
    (cond
      ((= n 1) (letrec/1/generate (car variables) (car expressions) body lexical))
      ((= n 2) (letrec/2/generate variables (car expressions) (cadr expressions) body lexical))
      ((= n 3) (letrec/3/generate variables (car expressions) (cadr expressions) (caddr expressions) body lexical))
      (else    (letrec/N/generate n variables expressions body lexical)))))

;; A cheatsheet for the routines below.
;;    ((lambda (v_1 ... v_N)        <--- letrec/outer with letrec variables v_1, ..., v_N
;;       ((lambda (t_1 ... t_N)       <--- letrec/inner
;;          (setter N)                  <--- setter
;;          (let () <body>))            <--- letrec/body
;;          e_1 ... e_N))             <--- letrec/e_1 letrec/e_2 ...
;;     #f ... #f)                   <--- junk values for v_1 ... v_N of letrec/outer

(define (letrec/1/generate variable e_1 body lexical)
  (let* ((lexical/expression ; Handcrafted lexical environment for compiling the letrec binding expression.
          (lexical/push/parameters lexical (list variable)))

         (letrec/e_1 ; Code for letrec expression.
          (scheme/compile e_1 lexical/expression)) ; Compile the letrec expression in its handcrafted lexical scope.

         (lexical/body ; Handcrafted lexical environment for compiling the letrec body.
          (lexical/push/parameters lexical/expression (list #f))) ; Fake lambda parameter that is bound to value of letrec expression.
         
         (letrec/body ; Code for letrec body.
          (scheme/compile `(let ()  ,@body) lexical/body)) ; Compile the letrec body in its handcrafted lexical scope.

         (letrec/inner ; Lambda that binds letrec variable to its definition and then invokes the letrec body.
          (motile/lambda/1/generate
           (sequence/generate
            2 
            (list 
             (motile/setter/1/generate) ; Set the letrec variable to its defined value.
             letrec/body))))       ; Execute the letrec body post binding.

         (letrec/outer ; Lambda that defines the letrec variable and calls the inner lambda with the proper letrec binding value
          (motile/lambda/1/generate
           (motile/combination/1/generate letrec/inner letrec/e_1)))
         
         (final
          (motile/combination/1/generate letrec/outer (constant/false/generate))))
    final)) 

(define (letrec/2/generate variables e_1 e_2 body lexical)
  (let* ((lexical/expression ; Handcrafted lexical environment for compiling the letrec binding expression.
          (lexical/push/parameters lexical variables))

         ; Code for letrec expressions.
         ; Compile each letrec expression in our handcrafted lexical scope.
         (letrec/e_1 (scheme/compile e_1 lexical/expression))
         (letrec/e_2 (scheme/compile e_2 lexical/expression))

         (lexical/body ; Handcrafted lexical environment for compiling the letrec body.
          (lexical/push/parameters lexical/expression (list #f #f))) ; Fake lambda parameters bound to letrec expressions.
         
         (letrec/body ; Code for letrec body.
          (scheme/compile `(let () ,@body) lexical/body)) ; Compile the letrec body in its handcrafted lexical scope.

         (letrec/inner ; Lambda that binds the letrec variables and invokes the letrec body.
          (motile/lambda/2/generate
           (sequence/generate
            2 
            (list 
             (motile/setter/2/generate) ; Set the letrec variables in letrec/outer to their defined values.
             letrec/body))))       ; Execute the letrec body post binding.

         (letrec/outer ; Lambda that defines the letrec variables and calls the inner lambda with the proper letrec binding value
          (motile/lambda/2/generate
           (motile/combination/2/generate letrec/inner letrec/e_1 letrec/e_2)))
         
         (final
          (motile/combination/2/generate letrec/outer (constant/false/generate) (constant/false/generate))))
    final))

(define (letrec/3/generate variables e_1 e_2 e_3 body lexical)
  (let* ((lexical/expression ; Handcrafted lexical environment for compiling the letrec binding expression.
          (lexical/push/parameters lexical variables))

         ; Code for letrec expressions.
         ; Compile each letrec expression in our handcrafted lexical scope.
         (letrec/e_1 (scheme/compile e_1 lexical/expression))
         (letrec/e_2 (scheme/compile e_2 lexical/expression))
         (letrec/e_3 (scheme/compile e_3 lexical/expression))

         (lexical/body ; Handcrafted lexical environment for compiling the letrec body.
          (lexical/push/parameters lexical/expression (list #f #f #f))) ; Fake lambda parameters bound to letrec expressions.
         
         (letrec/body ; Code for letrec body.
          (scheme/compile `(let () ,@body) lexical/body)) ; Compile the letrec body in its handcrafted lexical scope.

         (letrec/inner ; Lambda that binds the letrec variables and invokes the letrec body.
          (motile/lambda/3/generate
           (sequence/generate
            2 
            (list 
             (motile/setter/3/generate) ; Set the letrec variables in letrec/outer to their defined values.
             letrec/body))))       ; Execute the letrec body post binding.

         (letrec/outer ; Defines the letrec variables and calls the inner lambda with the binding values for the letrec variables.
          (motile/lambda/3/generate
           (motile/combination/3/generate letrec/inner letrec/e_1 letrec/e_2 letrec/e_3)))
         
         (final
          (motile/combination/3/generate letrec/outer (constant/false/generate) (constant/false/generate) (constant/false/generate))))
    final))

(define (letrec/N/generate n variables expressions body lexical)
  (let* ((lexical/expression ; Handcrafted lexical environment for compiling the letrec binding expression.
          (lexical/push/parameters lexical variables))

         ; Code for letrec expressions.
         ; Compile each letrec expression in our handcrafted lexical scope.
         (letrec/expressions (map (lambda (e) (scheme/compile e lexical/expression)) expressions))

         (lexical/body ; Handcrafted lexical environment for compiling the letrec body.
          (lexical/push/parameters lexical/expression (make-list n #f))) ; Fake lambda parameters bound to letrec expressions.
         
         (letrec/body ; Code for letrec body.
          (scheme/compile `(let () ,@body) lexical/body)) ; Compile the letrec body in its handcrafted lexical scope.

         (letrec/inner ; Lambda that binds the letrec variables defined by letrec/outer and invokes the letrec body.
          (lambda/rest/1/generate
           (sequence/generate
            2 
            (list 
             (motile/setter/N/generate n) ; Set the letrec variables in letrec/outer to their defined values.
             letrec/body))))                          ; Execute the letrec body post binding.

         (letrec/outer ; Defines the letrec variables and calls the inner lambda with the binding values for the letrec variables.
          (motile/lambda/N/generate
           n
           (motile/combination/N/generate letrec/inner letrec/expressions))) ; Body of letrec/outer.
         
         (final
          (motile/combination/N/generate letrec/outer (make-list n (constant/false/generate))))) ; Apply letrec/outer to junk values.
    final))


(define (test/letrec/1a)
  (let ((code (letrec/2/generate
               '(a b)                 ; Variables.
               11                     ; Expression_1.
               '(lambda () (+ a 13))  ; Expression_2.
               '(b)                   ; Body.
               #f)))                  ; Lexical stack (empty).
    (pretty-display (motile/decompile code))
    (let ((f (motile/start code ENVIRON/TEST)))
      (pretty-display (motile/start f ENVIRON/TEST)))))
    ;(pretty-display (motile/start code ENVIRON/TEST)

(define (test/letrec/1b)
  (let ((code (letrec/1/generate
               'factorial ; Variable.
               '(lambda (n) (if (= n 1) 1 (* n (factorial (sub1 n))))) ; Expression.
               '(factorial 5) ; Body.
               #f))) ; Empty lexical stack
    (pretty-display (motile/decompile code))
    (pretty-display (motile/start code ENVIRON/TEST))))

(define (test/letrec/2)
  (let ((code (letrec/2/generate
               '(even? odd?) ; letrec variables
               '(lambda (n) (if (= n 0) #t (odd? (sub1 n))))   ; even?
               '(lambda (n) (if (= n 0) #f (even? (sub1 n))))  ; odd?
               '(list (even? 12) (even? 3) (odd? 17) (odd? 8)) ; letrec body
               #f))) ; Empty lexical stack
    (pretty-display (code #f #f))
    (pretty-display (code rtk/RETURN RTE))))

(define (test/letrec/3)
  (let ((code (letrec/3/generate
               '(even? odd? zero) ; letrec variables
               '(lambda (n) (if (= n zero) #t (odd? (sub1 n))))  ; even?
               '(lambda (n) (if (= n zero) #f (even? (sub1 n)))) ; odd?
               0                                                 ; zero
               '(list (even? 12) (even? 3) (odd? 17) (odd? 8)) ; letrec body
               #f))) ; Empty lexical stack
    (pretty-display (code #f #f))
    (pretty-display (code rtk/RETURN RTE))))

(define (test/letrec/N)
  (let ((code (letrec/N/generate
               4                      ; Total number of letrec variables/definitions
               '(even? odd? zero one) ; letrec variables
               '((lambda (n) (if (= n zero) #t (odd? (- n one))))  ; even?
                 (lambda (n) (if (= n zero) #f (even? (- n one)))) ; odd?
                 0                                                 ; zero
                 1)                                                ; one
               '(list (even? 12) (even? 3) (odd? 17) (odd? 8)) ; letrec body
               #f))) ; Empty lexical stack
    (pretty-display (code #f #f))
    (pretty-display (code rtk/RETURN RTE))))

;; Only the recompiler uses this function. 
(define (setter/generate n)
  (cond
    ((= n 1) (motile/setter/1/generate))
    ((= n 2) (motile/setter/2/generate))
    ((= n 3) (motile/setter/3/generate))
    (else    (motile/setter/N/generate n))))

(define (motile/setter/generate n)
  (cond
    ((= n 1) (motile/setter/1/generate))
    ((= n 2) (motile/setter/2/generate))
    ((= n 3) (motile/setter/3/generate))
    (else    (motile/setter/N/generate n))))
(define (setter/1/generate)
  (lambda (rtk rte)
    (if rtk
        (rtk (vector-set! (rte/pop rte) 1 (vector-ref rte 1))) ; Set address (1 . 1) to value of (0 . 1).
        (vector 'setter 1))))

(define (motile/setter/1/generate)
  (lambda (k e _)
    (if k
        (k (vector-set! (rte/pop e) 1 (vector-ref e 1))) ; Set address (1 . 1) to value of (0 . 1).
        (vector 'setter 1))))

(define (setter/2/generate)
  (lambda (rtk rte)
    (if rtk
        (rtk
         (let ((prior (rte/pop rte)))
           (vector-set! prior 1 (vector-ref rte 1))   ; Set address (1 . 1) to value of (0 . 1).
           (vector-set! prior 2 (vector-ref rte 2)))) ; Set address (1 . 2) to value of (0 . 2).
        (vector 'setter 2))))

(define (motile/setter/2/generate)
  (lambda (k e _)
    (if k
        (k
         (let ((prior (e/pop e)))
           (vector-set! prior 1 (vector-ref e 1))   ; Set address (1 . 1) to value of (0 . 1).
           (vector-set! prior 2 (vector-ref e 2)))) ; Set address (1 . 2) to value of (0 . 2).
        (vector 'setter 2))))

(define (setter/3/generate)
  (lambda (rtk rte)
    (if rtk
        (rtk
         (let ((prior (rte/pop rte)))
           (vector-set! prior 1 (vector-ref rte 1))   ; Set address (1 . 1) to value of (0 . 1).
           (vector-set! prior 2 (vector-ref rte 2))   ; Set address (1 . 2) to value of (0 . 2).
           (vector-set! prior 3 (vector-ref rte 3)))) ; Set address (1 . 3) to value of (0 . 3).
        (vector 'setter 3))))

(define (motile/setter/3/generate)
  (lambda (k e _)
    (if k
        (k
         (let ((prior (e/pop e)))
           (vector-set! prior 1 (vector-ref e 1))   ; Set address (1 . 1) to value of (0 . 1).
           (vector-set! prior 2 (vector-ref e 2))   ; Set address (1 . 2) to value of (0 . 2).
           (vector-set! prior 3 (vector-ref e 3)))) ; Set address (1 . 3) to value of (0 . 3).
        (vector 'setter 3))))

(define (setter/N/generate n)
  (lambda (rtk rte)
    (if rtk
        (rtk
         (let loop ((prior (rte/pop rte))
                    (values (vector-ref rte 1)) ; The rest argument of the enclosing lambda.
                    (i 1))
           (cond 
             ((pair? values)
              (vector-set! prior i (car values))  ; Set address (1 . i) to the i'th value.
              (loop prior (cdr values) (add1 i)))
             (else
              (void)))))
        (vector 'setter n))))

(define (motile/setter/N/generate n)
  (lambda (k e _)
    (if k
        (k
         (let loop ((prior (e/pop e))
                    (values (vector-ref e 1)) ; The rest argument of the enclosing lambda.
                    (i 1))
           (when (pair? values)
              (vector-set! prior i (car values))  ; Set address (1 . i) to the i'th value.
              (loop prior (cdr values) (add1 i)))))

        (vector 'setter n))))

;; Extract the <name> from a definition.
(define (definition/name e) ; (define (f ...) <body>) or (define x <body>)
  (shape e 3)
  (let* ((pattern (cadr e))
         (name (if (pair? pattern) (car pattern) pattern)))
    (if (symbol? name)
        name
        (error (format "Identifier expected: ~a" name)))))

;; e has one of two forms:
;;   (define <name> <expression>) or
;;   (define (<name> <parameters>) <body>) where <parameters> may be empty.
;; Extract the form constituting the <body> of the definition.
(define (definition/value e)
  (let ((pattern (cadr e))) 
    (if (pair? pattern)
        ; pattern = (<name> <parameters>)
        (cons 'lambda (cons (cdr pattern) (cddr e))) ; (lambda <parameters> <body>).
        ; pattern = <name>
        (caddr e)))) ; <expression>.

;; *** END compile a lambda expression. ***

;; *** Compile (cond <clauses>). ***
(define (cond/compile e lexical)
  (cond/clauses/compile (cdr e) lexical))

(define (cond/clauses/compile clauses lexical)
  (if (pair? clauses)
      (let ((clause (car clauses)))
        (shape clause 1)
        (cond
          ((cond/clause/else? clause) ; (else <expressions>).
           (shape clause 2)
           (if (null? (cdr clauses))
               (sequence/compile (cdr clause) lexical)
               (error (format "<else> clause ~a is not last clause in cond" clause))))

          ((not (pair? (cdr clause))) ; (<test>).
           (or/generate
            (scheme/compile (cond/clause/test clause) lexical)
            (cond/clauses/compile (cdr clauses) lexical)))

          ((cond/clause/=>? clause) ; (<test> => <procedure>).
           (shape clause 3)
           (let ((test (scheme/compile (cond/clause/test clause) lexical))
                 (procedure (scheme/compile (cond/clause/procedure clause) lexical))
                 (else (cond/clauses/compile (cdr clauses) lexical)))
             (cond/=>/generate test procedure else)))

          (else ; (<test> <expressions>).
           (if/generate
            (scheme/compile (car clause) lexical)
            (sequence/compile (cdr clause) lexical)
            (cond/clauses/compile (cdr clauses) lexical)))))

      (constant/void/generate))) ; (cond).


(define (cond/clause/else? c) (eq? 'else (car c))) ; (else e_1 ... e_N).
(define (cond/clause/=>? c) ; (test => procedure).
   (eq? '=> (cadr c))) 
(define (cond/clause/test c) (car c)) ; (test ...).
(define (cond/clause/procedure c) (caddr c)) ; (test => procedure).

;; *** END compile (cond <clauses>). ***

(define (cond/=>/generate test procedure else)
  (lambda (rtk rte)
    (test
     (lambda (x)
       (if x
           ; The procedure target of the => is either a literal lambda expression or a variable.
           ; In either case it must be evaluated in the context of the run time environment of the
           ; cond clause. In the case of the literal lambda expression that evaluation yields the
           ; closure (lambda (rtk x) ...) that is the generated code of the lambda expression.
           ; In this case, the evaluation amounts to the evaluation of the literal lambda expression at
           ; its point of defintiion.
           ; In the second case, where a variable is the target of the => then the initial evaluation is
           ; a dereference of the variable, again yielding a closure (lambda (rtk x) ...), for some lambda expression.
           ; In all cases we apply the closure to two arguments, the continuation in effect at the cond clause and
           ; the outcome, x, of the evaluation of the test of the cond clause.
           ((procedure (lambda (p) p) rte) rtk x)
           (else rtk rte)))
     rte)))

(define (case? e) (eq? 'case (car e)))
(define (case/key e) (cadr e))
(define (case/clauses e) (cddr e))
(define (case/clause/datums? c) (pair? (car c)))
(define (case/clause/else? c) (eq? 'else (car c)))
(define (case/clause/datums c) (car c))
(define (case/clause/expressions c) (cdr c))

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
  (let ((key (gensym 'key/)))
    `(let ((,key ,(case/key e)))
       (cond ,@(case/clauses/translate key (case/clauses e))))))

(define (case/compile e lexical)
  (scheme/compile (case/translate e) lexical))

(define (do? e) (eq? 'do (car e)))
(define (do/bindings e) (cadr e))
(define (do/test e) (caddr e))
(define (do/test/expressions t) (cdr t))
(define (do/commands e) (cdddr e))
(define (do/binding/variable b) (car b))
(define (do/binding/initial b) (cadr b))
(define (do/binding/step b) (caddr b))

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
    'case    case/translate
     'do     do/translate
     'letrec letrec/translate
     'let    let/translate
     'let*   let*/translate
     ))
   )

(define (do/compile e lexical)
  (scheme/compile (do/translate e) lexical))

(define (and/compile e lexical)
  (let ((rest (cdr e)))
    (if (pair? rest)
        (and/rest/compile rest lexical)
        (constant/true/generate))))

(define (and/rest/compile rest lexical)
  (let ((code (scheme/compile (car rest) lexical))
        (tail (cdr rest)))
    (if (pair? tail)
        (and/generate code (and/rest/compile tail lexical))
        code)))
        
(define (and/generate head tail)
  (lambda (k e g)
    (if k
        (head
         ; Continuation for evauation of head.
         (lambda (x)  (if x (tail k e g) (k #f)))
         e g)
        
        (vector 'and (motile/decompile head) (motile/decompile tail)))))
;  (lambda (rtk rte)
;    (if rtk
;        (head
;         ; Continuation for evauation of head.
;         (lambda (x) 
;           (if x (tail rtk rte) (rtk #f)))
;         rte)
;        (vector 'and (head #f #f) (tail #f #f)))))

(define (or/compile e lexical)
  (let ((rest (cdr e)))
    (if (pair? rest)
        (or/rest/compile rest lexical)
        (constant/false/generate))))

(define (or/rest/compile rest lexical)
  (let ((code (scheme/compile (car rest) lexical))
        (tail (cdr rest)))
    (if (pair? tail)
        (or/generate code (or/rest/compile tail lexical))
        code)))

(define (or/generate head tail)
  (lambda (k e g)
    (if k
        (head
         (lambda (x) (if x (k x) (tail k e g)))
         e g)
        (vector 'or (motile/decompile head) (motile/decompile tail)))))
;  (lambda (rtk rte)
;    (if rtk
;        (head
;         (lambda (x)
;           (if x (rtk x) (tail rtk rte)))
;         rte)
;        (vector 'or (head #f #f) (tail #f #f)))))

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

;; environ/code - a Motile closure that will return a Motile binding environment
;; identifiers - a vector of symbols s_1 ... s_n
;; values/code - a vector of Motile closures v_1 ... v_n
(define (environ/cons/generate environ/code identifiers values/code)
  (lambda (k e g)
    (if k
        (k
         (let
             ; This code can't fail at the Motile-level since it is just lexical lookup.
             ((values (vector-map (lambda (code) (code k/RETURN e g)) values/code)))
           (environ/code
            (lambda (environ) (vectors/environ environ identifiers values))
            e
            g)))
        
        (vector
         'environ/cons
         (vector-length identifiers)
         (motile/decompile environ/code)
         identifiers
         (vector-map (lambda (code) (motile/decompile code)) values/code)))))

;; (environ/value <environ> <symbol> <substitute>).
(define (environ/value/compile e lexical)
  (shape e 4)
  (let ((environ    (environ/value/environ e))
        (symbol     (environ/value/identifier e))
        (substitute (environ/value/substitute e)))
    (if (symbol? symbol)
        (let ((environ/code (scheme/compile environ lexical))
              (substitute/code (scheme/compile substitute lexical)))
          (environ/value/generate environ/code symbol substitute/code))
        (error (format "Expected symbol for identifier in ~a got ~a instead" e symbol)))))

(define (environ/value/generate environ/code symbol failure/code)
  (lambda (k e g)
    (if k
        (k
         (environ/code
          ; Continuation for environ/code.
          (lambda (environ)
            (failure/code 
             ; Continuation for failure/code
             (lambda (failure) (environ/value environ symbol failure))
             e g))
          e g))

        (vector 'environ/value (motile/decompile environ/code) symbol (motile/decompile failure/code)))))

;; (environ/remove <environ> <symbol_1> ... <symbol_n>)
(define (environ/remove/compile e lexical)
  (shape e 3)
  (let ((x (environ/remove/e e)) ; <environ>
        (symbols (environ/remove/symbols e)))
    (if (andmap symbol? symbols)
        (let ((environ/code (scheme/compile x lexical)))
          (environ/remove/generate environ/code (list->vector symbols)))
        (error "Illegal identifier in ~s" e))))

;; environ/code - a Motile closure that will return a Motile binding environment
;; symbols - a non-empty vector of symbols
(define (environ/remove/generate environ/code symbols)
  (lambda (k e g)
    (if k
        (k
         (environ/code
          (lambda (environ) (environ/vector/remove environ symbols)) ; Continuation for environ/code.
          e g))

        (vector 'environ/remove (vector-length symbols) (motile/decompile environ/code) symbols))))

(define (environ/reflect/compile e lexical)
  (shape e 2)
  (let ((global/code (scheme/compile (environ/reflect/environ e) lexical))
        (body        (sequence/compile (environ/reflect/expressions e) lexical)))
    (environ/reflect/generate global/code body)))

(define (environ/reflect/generate global/code body)
  (lambda (k e g)
    (if k
        (global/code
         (lambda (global) (body k e global)) ; Evaluate the body in the context of the reflected global binding environment.
         e g)

        (vector 'environ/reflect (motile/decompile global/code) (motile/decompile body)))))
     
;; The COMPILE-time (NOT the run-time) lexical scope stack is represented as (prior s_1 s_2 ... s_N)
;; where s_1, s_2, ..., s_N are the symbols available in the most recent lexical scope and
;; prior is the next recent enclosing lexical scope. The root (outermost) lexical scope is denoted as #f.

;;; Push a frame onto the compile-time lexical scope stack.
;(define (lexical/push lexical formals)
;  (cons lexical formals))
;;; Pop a frame off the compile-time lexical scope stack.
;(define (lexical/pop lexical) (car lexical))
;;; Obtain the topmost frame.
;(define (lexical/frame lexical) (cdr lexical))

;; If the symbol is in lexical scope return a pair (major . minor) where:
;; major = 0, 1, ... is the frame number relative to the top of the lexical frame stack
;; minor is the (ordinal) position 1, 2, ... of the symbol within the frame
;; If the symbol is NOT in lexical scope return #f.
;(define (lexical/address lexical symbol)
;  (let loop ((lexical lexical)
;             (major 0))
;    (if lexical
;        (let* ((frame (lexical/frame lexical))
;               (x     (memq symbol frame)))
;          (if x
;              (cons major (- (add1 (length frame)) (length x)))
;              (loop (lexical/pop lexical) (add1 major))))
;        
;        #f))) ; symbol must be a global reference.

(define (lexical/address? a)
  (and (pair? a) (integer? (car a)) (integer? (cdr a))))
(define-syntax-rule (lexical/address/major a) (car a))
(define-syntax-rule (lexical/address/minor a) (cdr a))

;; Set the value of the rte slot at address a to the given value.
(define (lexical/address/set a value)
  (let ((major (lexical/address/major a))
        (minor (lexical/address/minor a)))
    (lambda (rtk rte)
      (rtk (vector-set! (rte/frame rte major) minor value)))))


;; lexical: lexical stack
;; symbol: a variable
;; If symbol is a free variable return its offset the run time frame.
;; If symbol is not a free variable return #f.
(define (closed/offset lexical symbol)
  (let ((closed (lexical/frame/closed lexical)))
    (cond
      ((memq symbol closed)
       => (lambda (place) (+ 1 (length (lexical/frame/parameters lexical)) (- (length closed) (length place)))))
      (else #f))))

;; Code to fetch the value of a free variable v_i from a closure.
(define (reference/closed/generate index)
  (lambda (rtk rte)
    (if rtk
        (rtk (vector-ref rte index))
        (vector 'reference/closed index))))

(define (rte/dump rte)
  (if rte
      (let loop ((i 0) (rte rte))
        (when (vector-ref rte 0)
          (display (format "~a: ~a\n" i (cdr (vector->list rte))))
          (loop (add1 i) (rte/pop rte))))
      (display "rte/dump: rte: #f\n")))
        

;; Return the number of free variables contained in the closure. !!! UNUSED !!!
;(define (closure/span v) (- (vector-length v) 2))
;(define (closure/code v)      (vector-ref v 0))
;(define (closure/addresses v) (vector-ref v 1))

(define (pretty/e e)
  (display ">>>\n")
  (let loop ((i 0) (e e))
    (when (vector-ref e 0)
      (display (format "frame ~a: " i))
      (pretty-display (cdr (vector->list e)))
      (loop (add1 i) (e/pop e))))
  (display "<<<\n"))
  

;; Fetch a value from a frame of the run-time environ.
(define (reference/rte/free rte address)
  (let loop ((rte rte)
             (frame  (car address))
             (offset (cdr address)))
    (cond
      ((= frame 0) (vector-ref rte offset))
      ((= frame 1) (vector-ref (vector-ref rte 0) offset))
      ((= frame 2) (vector-ref (vector-ref (vector-ref rte 0) 0) offset))
      ((= frame 3) (vector-ref (vector-ref (vector-ref (vector-ref rte 0) 0) 0) offset))
      (else        (loop (vector-ref rte 0) (sub1 frame) offset)))))

(define (reference/local/generate address)
  (reference/rte/generate (lexical/address/major address) (lexical/address/minor address)))

(define (reference/rte/generate frame offset)
  (cond
    ((= frame 0) (reference/rte/frame/0/generate offset))
    ((= frame 1) (reference/rte/frame/1/generate offset))
    ((= frame 2) (reference/rte/frame/2/generate offset))
    (else        (reference/rte/frame/N/generate frame offset))))

;; Skip two frames up the run-time environ and apply the closure to the old continuation (rtk) and the shorter rte.
;; The combined effect of reference/rte/generate above and this routine is to generate a closure that walks
;; up the run-time environ in two-frame "hops" until it arrives at desired frame pair
;; and then select a value from the frame/slot address 0/minor or 1/minor.

(define (reference/rte/frame/N/generate frame offset)
  (lambda (rtk rte)
    (if rtk
        (rtk (vector-ref (rte/unwind rte frame) offset)) ;(rtk (vector-ref (rte/unwind rte frame offset)))
        (vector 'reference/local frame offset))))

;(define (reference/rte/frame/+2/generate closure)
;  (lambda (rtk rte)
;    (closure
;     rtk
;     (vector-ref (vector-ref rte 0) 0)))) ; Walk up the run-time environ by two frames.

(define-syntax-rule (macro/reference/rte/frame/0 offset)
  (lambda (rtk rte)
    (if rtk
        (rtk (vector-ref rte offset))
        (vector 'reference/local 0 offset))))

(define (reference/rte/frame/0/generate offset)
  (cond
    ((= offset 1) (macro/reference/rte/frame/0 1))
    ((= offset 2) (macro/reference/rte/frame/0 2))
    ((= offset 3) (macro/reference/rte/frame/0 3))
    ((= offset 4) (macro/reference/rte/frame/0 4))
    (else         (macro/reference/rte/frame/0 offset))))

;(define (reference/rte/frame/0/generate minor)
;  (cond
;    ((= minor 1) (lambda (rtk rte) (rtk (vector-ref rte 1))))
;    ((= minor 2) (lambda (rtk rte) (rtk (vector-ref rte 2))))
;    ((= minor 3) (lambda (rtk rte) (rtk (vector-ref rte 3))))
;    ((= minor 4) (lambda (rtk rte) (rtk (vector-ref rte 4))))
;    (else        (lambda (rtk rte) (rtk (vector-ref rte minor))))))

(define-syntax-rule (macro/reference/rte/frame/1 offset)   
  (lambda (rtk rte)
    (cond
      ((procedure? rtk) (rtk (vector-ref (vector-ref rte 0) offset)))
      ((= rtk 0) (vector 'reference/local 1 offset)))))

(define (reference/rte/frame/1/generate offset)
  (cond
    ((= offset 1) (macro/reference/rte/frame/1 1))
    ((= offset 2) (macro/reference/rte/frame/1 2))
    ((= offset 3) (macro/reference/rte/frame/1 3))
    ((= offset 4) (macro/reference/rte/frame/1 4))
    (else        (macro/reference/rte/frame/1 offset))))

;(define (reference/rte/frame/1/generate minor)
;  (cond
;    ((= minor 1) (lambda (rtk rte) (rtk (vector-ref (vector-ref rte 0) 1))))
;    ((= minor 2) (lambda (rtk rte) (rtk (vector-ref (vector-ref rte 0) 2))))    
;    ((= minor 3) (lambda (rtk rte) (rtk (vector-ref (vector-ref rte 0) 3))))
;    ((= minor 4) (lambda (rtk rte) (rtk (vector-ref (vector-ref rte 0) 4))))
;    (else        (lambda (rtk rte) (rtk (vector-ref (vector-ref rte 0) minor))))))

(define-syntax-rule (macro/reference/rte/frame/2 offset)
  (lambda (rtk rte)
    (cond
      ((procedure? rtk) (rtk (vector-ref (vector-ref (vector-ref rte 0) 0) offset)))
      ((= rtk 0) (vector 'reference/local 2 offset)))))

(define (reference/rte/frame/2/generate offset)
  (cond
    ((= offset 1) (macro/reference/rte/frame/2 1))
    ((= offset 2) (macro/reference/rte/frame/2 2))
    ((= offset 3) (macro/reference/rte/frame/2 3))
    ((= offset 4) (macro/reference/rte/frame/2 4))
    (else        (macro/reference/rte/frame/2 offset))))


;; Returns the bottom frame #(#f X ...) of the run-time environ.
(define (rte/bottom rte)
  (if (vector-ref rte 0)
      (rte/bottom (rte/pop rte))
      rte))

;; Wind your way down to the very bottom of the run-time environ to find the global namespace.
;; The bottommost (initial) form of the run-time environ is the vector #(#f GLOBAL).
(define (rte/bottom/global rte)
  (vector-ref (rte/bottom rte) 1))

;; Unwind the run-time environ n frames.
(define (rte/unwind rte n)
  (cond
    ((= n 0) rte)
    ((= n 1) (vector-ref rte 0))
    ((= n 2) (vector-ref (vector-ref rte 0) 0))
    ((= n 3) (vector-ref (vector-ref (vector-ref rte 0) 0) 0))
    (else (rte/unwind rte (- n 3)))))

(define (global/find global symbol)
  (or
   ;(hash-ref global symbol #f)
   (hash/ref global symbol #f) ; We now use persistent functional hash tables rather than Racket immutable hash tables.
   (error 'global/find "undefined: ~s" symbol)))

(define JUNK (gensym 'junk))

(define (motile/global/find global symbol)
  (let ((value (environ/value global symbol JUNK)))
    (if (eq? value JUNK)
        (error 'motile/global/find "undefined: ~s" symbol)
        value)))

; Any variable not in lexical scope must appear in the global environ.
; Generate a closure to dereference the given symbol in the global environ.
;(define (reference/global/variable/generate symbol)
;  (let* ((undefined (gensym))
;         (value undefined))
;    (lambda (rtk rte)
;      (if rtk
;          (begin
;            (when (eq? value undefined)
;              (set! value (global/find (rte/bottom/global rte) symbol))) ; Cache the value to eliminate another lookup.
;            (rtk value))
;          
;          (vector 'reference/global symbol)))))

;; !!!! Experiment A !!!!
;; This version DOESN'T cache global values.
;; If we try to cache the values of global bindings then those cache values persist.
;(define (reference/global/variable/generate symbol)
;    (lambda (rtk rte)
;      (if rtk
;          (rtk (global/find (rte/bottom/global rte) symbol))
;          (vector 'reference/global symbol))))

(define (reference/global/variable/generate symbol)
  (lambda (k _ g)
    (if k
        (k (motile/global/find g symbol))
        (vector 'reference/global symbol))))
            
        

;; Return the frame of the run-time environ corresponding to major, the given frame index.
(define (rte/frame rte major)
  (if (= major 0)
      rte
      (rte/frame (rte/pop rte) (sub1 major))))

(define-syntax-rule (macro/constant/generate c)
  (lambda (k/apply e/apply _)
    (if k/apply
        (k/apply c)
        (vector 'constant/generate c))))

;; Generate a closure for a constant value.
(define (constant/generate value)
  (cond
    ((eqv? value 0)      (constant/0/generate))
    ((eqv? value 1)      (constant/1/generate))
    ((eqv? value 2)      (constant/2/generate))
    ((eqv? value '())    (constant/null/generate))
    ((eqv? value #t)     (constant/true/generate))
    ((eqv? value #f)     (constant/false/generate))
    ((eqv? value (void)) (constant/void/generate))
    (else                (constant/any/generate value))))

;; Generate a closure for a specific constant value: 0, 1, 2, nil, #t, #f.
(define (constant/0/generate)
  (macro/constant/generate 0))

(define (constant/1/generate)
  (macro/constant/generate 1))
                     
(define (constant/2/generate)
  (macro/constant/generate 2))

(define (constant/null/generate)
  (macro/constant/generate '()))

(define (constant/true/generate)
  (macro/constant/generate #t))

(define (constant/false/generate)
  (macro/constant/generate #f))

(define (constant/void/generate)
  (macro/constant/generate (void)))

;; Generate a closure for an arbitrary constant value.
(define (constant/any/generate value)
  (macro/constant/generate value))

;; Compile (if <test> <then> <else>).
(define (if/compile e lexical)
  (shape e 4)
  (let ((test (scheme/compile (if/test e) lexical))
        (then (scheme/compile (if/then e) lexical))
        (else (scheme/compile (if/else e) lexical)))
    (if/generate test then else)))                    
                    
;; Generate a closure for (if <test> <then> <else>). 
;(define (if/generate test then else)
;  (lambda (rtk rte)
;    (if rtk
;        (test
;         (lambda (x) ; The continuation for the test.
;           (if x (then rtk rte) (else rtk rte)))
;         rte)
;        (vector 'if (test #f #f) (then #f #f) (else #f #f)))))

(define (if/generate test then else)
  (lambda (k e g)
    (if k
        (test
         (lambda (x) (if x (then k e g) (else k e g))) ; The continuation for the test.
         e g)
        
        (vector 'if (motile/decompile test) (motile/decompile then) (motile/decompile else)))))
    
(define (when/compile e lexical)
  (shape e 3)
  (when/generate
   (scheme/compile (when/test e)  lexical)
   (scheme/compile (when/thens e) lexical)))

;; Generate closure for (when <test> <thens>).
(define (when/generate test thens)
  (lambda (rtk rte)
    (if rtk
        (test
         (lambda (x)
           (if x (thens rtk rte) (rtk (void))))
         rte)
        (vector 'when (motile/decompile test) (motile/decompile thens)))))

(define (unless/compile e lexical)
  (shape e 3)
  (unless/generate
   (scheme/compile (unless/test e) lexical)
   (scheme/compile (unless/elses e) lexical)))

(define (unless/generate test elses)
  (lambda (rtk rte)
    (if rtk
        (test
         (lambda (x)
           (if (not x) (elses rtk rte) (rtk (void))))
         rte)
        (vector 'unless (motile/decompile test) (motile/decompile elses)))))
          
(define (begin/compile e lexical)
  (shape e 2)
  (sequence/compile (cdr e) lexical))

(define (sequence/compile expressions lexical)
  (let ((elements (map (lambda (e) (scheme/compile e lexical)) expressions))) ; Compile each expression e_i in the sequence.
    (sequence/generate (length elements) elements))) ; Combine the elements into a single code (lambda (rtk rte) ...).

;; Generate the closure for a nonempty sequence of closures (c_1 c_2 ... c_N).
;; It returns a closure (lambda (rtk rte) ...) that:
;; (1) Ensures left-to-right evaluation of closures c_i
;; (2) Returns the value of the evaluation of c_N.
;(define (sequence/generate n codes)
;  (if (= n 1)
;      (car codes)
;
;      (let ((c (car codes))
;            (tail (sequence/generate (sub1 n) (cdr codes))))
;        (lambda (rtk rte)
;          (if rtk
;            (c (lambda (_) (tail rtk rte)) rte)
;            (vector 'sequence n (map (lambda (c) (c #f #f)) codes)))))))

(define (sequence/generate n codes)
  (if (= n 1)
      (car codes)

      (let ((c (car codes))
            (tail (sequence/generate (sub1 n) (cdr codes))))
        (lambda (k/apply e/apply g/apply)
          (if k/apply
              ; Let codes = (c_i c_{i+1} ... c_{n-1}).
              ; Call c_i with three arguments:
              ;   * A continuation that will invoke the tail of codes c_{i+1} ... c_{n-1}
              ;   * e/apply, the stack at point of application
              ;   * g/apply, the global binding environment at point of application.
              (c (lambda (_) (tail k/apply e/apply g/apply)) e/apply g/apply)

              ; Return a descriptor for the sequence.
              (vector 'sequence n (map (lambda (c) (motile/decompile c)) codes)))))))

;; (lambda rest <body>).
;(define (lambda/rest/1/generate body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((descriptor #f)
;              (rte rte))
;          (rtk
;           (lambda (k . rest)
;             (if k
;                 (body k (vector rte rest))
;
;                 (let ((x (car rest)))
;                   (cond
;                     ((pair? x)
;                      ; Reset the global binding environment where a = (#(#f <globals>)).
;                      (set! rte (car x)))
;                     ((vector? x)
;                      ; Ignore any attempt to reset the nonexistent bindings for closed variables.
;                      (void))
;                     (else
;                      ; x is #f. Return a descriptor.
;                      (unless descriptor
;                        (set! descriptor (vector 'lambda/rest/inner 1 (body #f #f))))
;                      descriptor)))))))
;
;        (vector 'lambda/rest 1 (body #f #f)))))    

(define (lambda/rest/1/generate body)
  (lambda (k/define e/define g/define)
    (if k/define
        (let ((descriptor #f))
          (k/define 
           (lambda (k e global . rest)
             (if k
                 (body k (vector e/define rest) global)

                 (when (zero? e)
                   (return!
                    descriptor
                    (vector 'lambda/rest/inner 1 (motile/decompile body))))))))

        (vector 'lambda/rest 1 (motile/decompile body)))))


;; (lambda (a . rest) <body>)
;(define (lambda/rest/2/generate body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((descriptor #f)
;              (rte rte))
;          (rtk
;           (lambda (k x . rest)
;             (if k
;                 (body k (vector rte x rest))
;                 
;                 (cond
;                   ((pair? x)
;                    ; Reset the global binding environment where a = (#(#f <globals>)).
;                    (set! rte (car x)))
;                   ((vector? x)
;                    ; Ignore any attempt to reset the nonexistent bindings for closed variables.
;                    (void))
;                   (else
;                    ; x is #f. Return a descriptor.
;                    (unless descriptor
;                      (set! descriptor (vector 'lambda/rest/inner 2 (body #f #f))))
;                    descriptor))))))
;
;        (vector 'lambda/rest 2 (body #f #f)))))

(define (lambda/rest/2/generate body)
  (lambda (k/define e/define g/define)
    (if k/define
        (let ((descriptor #f))
          (k/define
           (case-lambda
             ((k _ global x . rest) (body k (vector e/define x rest) global))
             ((k n _)
              (when (zero? n)
                (return!
                 descriptor
                 (vector 'lambda/rest/inner 2 (motile/decompile body))))))))

        (vector 'lambda/rest 2 (motile/decompile body)))))


;; (lambda (a b . rest) <body>).
;(define (lambda/rest/3/generate body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((descriptor #f)
;              (rte rte))
;          (rtk
;           (case-lambda
;             ((k a b . rest)
;              (body k (vector rte a b rest)))
;             ((k x)
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Ignore any attempt to reset the nonexistent bindings for closed variables.
;                   (void))
;                  (else
;                   ; x is #f. Return a descriptor.
;                   (unless descriptor
;                     (set! descriptor (vector 'lambda/rest/inner 3 (body #f #f))))
;                   descriptor)))))))
;
;        (vector 'lambda/rest 3 (body #f #f)))))

(define (lambda/rest/3/generate body)
  (lambda (k/define e/define g/define)
    (if k/define
        (let ((descriptor #f))
          (k/define
           (case-lambda
             ((k _ global x y . rest) (body k (vector e/define x y rest) global))
             ((k n _)
              (when (zero? n)
                (return!
                 descriptor
                 (vector 'lambda/rest/inner 3 (motile/decompile body))))))))

        (vector 'lambda/rest 3 (motile/decompile body)))))

;; (lambda (a b c . rest) <body>)
;(define (lambda/rest/4/generate body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((descriptor #f)
;              (rte rte))
;          (rtk
;           (case-lambda
;             ((k a b c . rest)
;              (body k (vector rte a b c rest)))
;             ((k x)
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Ignore any attempt to reset the nonexistent bindings for closed variables.
;                   (void))
;                  (else
;                   ; x is #f. Return a descriptor.
;                   (unless descriptor
;                     (set! descriptor (vector 'lambda/rest/inner 4 (body #f #f))))
;                   descriptor)))))))
;
;        (vector 'lambda/rest 4 (body #f #f)))))


(define (lambda/rest/4/generate body)
  (lambda (k/define e/define g/define)
    (if k/define
        (let ((descriptor #f))
          (k/define
           (case-lambda
             ((k _ global x y z . rest) (body k (vector e/define x y z rest) global))
             ((k n _)
              (when (zero? n)
                (return!
                 descriptor
                 (vector 'lambda/rest/inner 4 (motile/decompile body))))))))

        (vector 'lambda/rest 4 (motile/decompile body)))))

;; The general case with n > 3 parameters, for which the last is a rest parameter.
;(define (lambda/rest/N/generate n body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((descriptor #f)
;              (rte rte))
;          (rtk
;           (case-lambda 
;             ((k a b c . rest)
;              (let ((frame (make-vector (add1 n))))
;                (vector-set! frame 0 rte)
;                (vector-set! frame 1 a)
;                (vector-set! frame 2 b)
;                (vector-set! frame 3 c)
;                (let loop ((i 4) (rest rest))
;                  (cond
;                    ((< i n)
;                     (vector-set! frame i (car rest))
;                     (loop (add1 i) (cdr rest)))
;                    (else
;                     (vector-set! frame n rest))))
;                (body k frame)))
;             ((k x)
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Ignore any attempt to reset the nonexistent bindings for closed variables.
;                   (void))
;                  (else
;                   ; x is #f. Return a descriptor.
;                   (unless descriptor
;                     (set! descriptor (vector 'lambda/rest/inner n (body #f #f))))
;                   descriptor)))))))             
;
;        (vector 'lambda/rest n (body #f #f)))))

;; The general case with n > 4 parameters, for which the last is a rest parameter.
(define (lambda/rest/N/generate n body)
  (lambda (k/define e/define g/define)
    (if k/define
        (let ((descriptor #f))
          (k/define
           (case-lambda
             ((k _ global a b c . rest)
              (let ((frame (make-vector (add1 n))))
                (vector-set! frame 0 e/define)
                (vector-set! frame 1 a)
                (vector-set! frame 2 b)
                (vector-set! frame 3 c)
                (let loop ((i 4) (rest rest))
                  (cond
                    ((< i n)
                     (vector-set! frame i (car rest))
                     (loop (add1 i) (cdr rest)))
                    (else
                     (vector-set! frame n rest))))
                (body k frame global)))
             ((k n _)
              (when (zero? n)
                (return!
                 descriptor
                 (vector 'lambda/rest/inner n (motile/decompile body))))))))

        (vector 'lambda/rest n (motile/decompile body)))))


;; Return #t if symbol is a lambda parameter in the topmost lexical frame and #f otherwise.
(define (lexical/variable/parameter? lexical symbol)
  (and lexical (set/member? symbol (lexical/frame/parameters lexical))))

;; Return #t if symbol is a closed variable in the topmost lexical frame and #f otherwise.
;; lexical: lexical environment stack
;; symbol: variable appearing in an expression
(define (lexical/variable/closed? lexical symbol)
  (and lexical (set/member? symbol (lexical/frame/closed lexical))))

;; Topmost frame at run time is #(rte p_1 ... p_M c_1 ... c_N), where M > 0.
;; Offset (index) of p_i in frame is i.
;; Generate the code to fetch the argument value bound to parameter p_i.
(define (reference/variable/parameter/generate lexical symbol)
  (let* ((parameters (lexical/frame/parameters lexical)) ; (p_1 ... p_M), M > 0.
         (tail       (memq symbol parameters))           ; (p_i ... p_M), i <= M.
         (offset (add1 (- (length parameters) (length tail)))))
    (lambda (rtk rte _) ; Motile mod 2011.06.18
      (if rtk
          (rtk (vector-ref rte offset))
          (vector 'reference/parameter offset)))))

;; Generate the code to fetch the binding of a closed variable from within a lambda body.
(define (reference/variable/closed/generate lexical symbol)
    (let* ((parameters (lexical/frame/parameters lexical))
           (closed     (lexical/frame/closed lexical))
           (tail       (memq symbol closed))
           (offset (+ 1 (length parameters) (- (length closed) (length tail))))) ; Offset from start of frame.
        (lambda (k e _)
          (if k
              (k (vector-ref e offset))
              (vector 'reference/closed offset)))))

(define (combination/compile e lexical)
  (let ((operator (car e))
        (arguments (map (lambda (a) (scheme/compile a lexical)) (cdr e))))
    (cond
      ((symbol? operator)
       (cond
         ((lexical/variable/parameter? lexical operator)
          (combination/generate
           (reference/variable/parameter/generate lexical operator) ; Generate a reference to a lambda parameter.
           arguments))
         ((lexical/variable/closed? lexical operator)
          (combination/generate
           (reference/variable/closed/generate lexical operator)    ; Generate a reference to a closed variable.
           arguments))
;         ((baseline/variable? operator)
;          (combination/base/generate (baseline/generate operator) arguments))
         (else
          ;(combination/global/generate (reference/global/variable/generate operator) arguments))))
          (combination/generate (reference/global/variable/generate operator) arguments)))) ; Generate a reference to a global variable.
      ((pair? operator) ; Operator is a complex expression though not necessarily appropriate for this position.
       (combination/generate (scheme/compile operator lexical) arguments))
      (else ; Must be a constant.
       (error (format "Inappropriate operator ~a in combination" e))))))

(define (combination/generate operator arguments)
  (let ((n (length arguments)))
    (cond
      ((= n 0)
       (motile/combination/0/generate operator)) ;(combination/0/generate operator))
      ((= n 1)
       (motile/combination/1/generate operator (car arguments)))
      ((= n 2)
       (motile/combination/2/generate operator (car arguments) (cadr arguments)))
      ((= n 3)
       (motile/combination/3/generate operator (car arguments) (cadr arguments) (caddr arguments)))
      (else
       (motile/combination/N/generate operator arguments)))))


;; The decompilation of a closure is generated exactly once on demand and cached by the closure,
;; thereby guaranteeing that if a closure C is referenced in more than one location then the serialization
;; will correctly detect that C is shared or an element of a cycle. This in turn guarantees that
;; the deserialization will correctly reconstruct the original reference graph where closure C is
;; reconstructed exactly once and referenced thereafter.

;; Since the caching of a closure descriptor (that data structure that contains the closure decompilation)
;; is not thread-safe the island infrastructure for message transmission must ensure that exactly one
;; thread is responsible for serialization and that no other thread on the island performs serialization.
;; For now this is both feasible and efficient however when and if we get to the point of saving closures
;; to disk independent of message transmission we will have to make some changes to the lower level island
;; infrastructure. One option would be to offer serialization as an "island service" for both the outgoing
;; messaging layer and any closure archive.




(define-syntax-rule (return! variable value)
  (begin
    (unless variable (set! variable value))
    variable))

(define-syntax-rule (bind! variable value)
  (unless variable (set! variable value)))
      

;; The following five generators produce the code for a combination, (operator a_1 ... a_N).
;; where operator is a source procedure.
;(define (combination/0/generate operator)
;  (lambda (rtk rte)
;    (if rtk
;        (operator (lambda (o) (o rtk)) rte)
;        (vector 'combination 0 (operator #f #f) null))))

(define (motile/combination/0/generate operator)
  (let ((descriptor #f))
    (lambda (rtk rte global)
      (if rtk
          (operator (lambda (o) (o rtk rte global)) rte global)
          
          (return! descriptor (vector 'combination 0 (motile/decompile operator) null))))))

;(define (combination/1/generate operator x)
;  (lambda (rtk rte)
;    (if rtk
;        (operator
;         (lambda (o)
;           (x (lambda (a) (o rtk a)) rte))
;         rte)
;        (vector 'combination 1 (operator #f #f) (list (x #f #f))))))

(define (motile/combination/1/generate operator x)
  (let ((descriptor #f))
    (lambda (k e g)
      (if k
          (operator
           (lambda (o) (x (lambda (a) (o k e g a)) e g)) ; Continuation for operator.
           e
           g)
          
          (return! descriptor (vector 'combination 1 (motile/decompile operator) (list (motile/decompile x))))))))

;(define (combination/2/generate operator x y)
;  (lambda (rtk rte)
;    (if rtk
;        (operator
;         (lambda (o)
;           (x
;            (lambda (a)
;              (y (lambda (b) (o rtk a b)) rte))
;            rte))
;         rte)
;        
;        (vector 'combination 2 (operator #f #f) (list (x #f #f) (y #f #f))))))

(define (motile/combination/2/generate operator x y)
  (let ((descriptor #f))
    (lambda (k/apply e/apply g/apply)
      (if k/apply
          (operator
           (lambda (o)
             (x
              (lambda (a)
                (y (lambda (b) (o k/apply e/apply g/apply a b)) e/apply g/apply))
              e/apply
              g/apply))
           e/apply
           g/apply)
          
          (return!
           descriptor
           (vector 'combination 2 (motile/decompile operator) (list (motile/decompile x) (motile/decompile y))))))))

;(define (combination/3/generate operator x y z)
;  (lambda (rtk rte)
;    (if rtk
;        (operator
;         (lambda (o)
;           (x
;            (lambda (a)
;              (y
;               (lambda (b)
;                 (z (lambda (c) (o rtk a b c)) rte))
;               rte))
;            rte))
;         rte)
;
;        (vector 'combination 3 (operator #f #f) (list (x #f #f) (y #f #f) (z #f #f))))))

(define (motile/combination/3/generate operator x y z)
  (let ((descriptor #f))
    (lambda (k/apply e/apply g/apply)
      (if k/apply 
          (operator
           (lambda (o)
             (x
              (lambda (a)
                (y
                 (lambda (b) (z (lambda (c) (o k/apply e/apply g/apply a b c)) e/apply g/apply)) ; Continuation for y.
                 e/apply
                 g/apply)) 
              e/apply
              g/apply))
           e/apply
           g/apply)
          
          (return!
           descriptor
           (vector 'combination 3 (motile/decompile operator) (list (motile/decompile x) (motile/decompile y) (motile/decompile z))))))))

;; (operator a_1 ... a_N) where N > 3.
;(define (combination/N/generate operator arguments)
;  (lambda (rtk rte)
;    (if rtk
;        (operator
;         (lambda (o)
;           (map/k
;            (lambda (a k) (a k rte))
;            arguments
;            (lambda (values) (apply o (cons rtk values)))))
;         rte)
;
;        (let ((decompiles (map (lambda (a) (a #f #f)) arguments)))
;          (vector 'combination (length arguments) (operator #f #f) decompiles)))))

(define (motile/combination/N/generate operator arguments)
  (let ((descriptor #f))
    (lambda (k/apply e/apply g/apply)
      (if k/apply
          (operator
           (lambda (o)
             (map/k
              (lambda (a k) (a k e/apply g/apply))
              arguments
              (lambda (values) (apply o (list* k/apply e/apply g/apply values)))))
           e/apply
           g/apply)
          
          (return!
           descriptor
           (let ((decompiles (map (lambda (a) (motile/decompile a)) arguments)))
             (vector 'combination (length arguments) (motile/decompile operator) decompiles)))))))

(define (quasiquote/compile e lexical) ; e is (quasiquote <object>)
  (quasiquotation/compile (cadr e) 1 lexical)) ; (cadr e) is <object>.

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
        (quasiquotation/list/compile form (+ level 1) lexical))
       
       ((eq? (car form) 'unquote) ; form is (unquote <object>) or more commonly ,<object>.
        (if (= level 1)
            ; Compile <object> as we are situated at the outermost quasiquotation.
            (scheme/compile (cadr form) lexical)
            (quasiquotation/list/compile form (- level 1) lexical))) ; 
       
       ((eq? (car form) 'unquote-splicing) ; form is (unquote-splicing <object>) or more commonly ,@<object>.
        (when (= level 1)
          (error (format "Ill-placed 'unquote-splicing' ~a" form)))
        (quasiquotation/list/compile form (- level 1) lexical))
       
       (else
        (quasiquotation/list/compile form level lexical))))
    
    ((vector? form)
     (form/vector/generate
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
            (form/append/generate
             (scheme/compile (cadr first) lexical)
             (quasiquotation/compile (cdr l) 1 lexical)))

          (form/cons/generate
           (quasiquotation/compile first level lexical)
           (quasiquotation/compile (cdr l) level lexical)))

        ; level > 1.
        (form/cons/generate
         (quasiquotation/compile first level lexical)
         (quasiquotation/compile (cdr l) level lexical))))

    (quasiquotation/compile l level lexical)))



(define (unquote/compile e lexical)
  (error (format "Ill-placed unquote ~a" e)))

(define (unquote-splicing/compile e lexical)
  (error (format "Ill-placed unquote-splicing: ~a" e)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (form/append/generate head tail)
  (lambda (rtk rte)
    (head
     (lambda (h)
       (tail
        (lambda (t) (rtk (append h t)))
        rte))
     rte)))

;(define (gen-append-form code1 code2)
;  (lambda (rte) (append (code1 rte) (code2 rte))))

(define (form/cons/generate alpha beta)
  (lambda (rtk rte)
    (alpha
     (lambda (a)
       (beta
        (lambda (b) (rtk (cons a b)))
        rte))
     rte)))

;(define (gen-cons-form code1 code2)
;  (lambda (rte) (cons (code1 rte) (code2 rte))))

(define (form/vector/generate c)
  (lambda (rtk rte)
    (c
     (lambda (x) (rtk (list->vector x)))
     rte)))

;(define (gen-vector-form code)
;  (lambda (rte) (list->vector (code rte))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (rte/reveal rtk rte)  
  (rtk
   (let loop ((frame rte))
     (if (vector-ref frame 0)
         (cons
          (cdr (vector->list frame)) ; Topmost frame. 
          (loop (vector-ref frame 0)))
         'GLOBAL))))

(define (rte/frame/reveal rtk rte n)
  (cond
    ((= n 0)
     (rtk (cdr (vector->list rte))))
    (else
     (rte/frame/reveal rtk (vector-ref rte 0) (sub1 n)))))

(define (rtk/RETURN x) x) ; The trivial continuation.
;(define RTE (vector #f BASELINE))
(define RTE (vector #f ENVIRON/TEST)) ; BASELINE + mutable Racket vectors + simple output functions.
(define (compile e) (scheme/compile e #f))
;; Compile expression e returning its local host closure representation.
(define (mischief/compile e) (scheme/compile e #f))
;; Given an executable Mischief closure c returns the assembly language representation of the closure
;; as a graph (of mostly vectors). If closure c contains cyclic data structures in its closed variable bindings
;; or recursive definitions (via a letrec) then the graph will contain cycles otherwise the graph will be acyclic
;; though some (sub)structures may be shared (referenced in multiple distinct locations within the graph).
(define (mischief/decompile c) (c #f 0 #f)) ; DEPRECATED! Use motile/decompile instead.

(define (decompile e)
  (let* ((seen (make-hasheq))
         (outcome (e #f seen)))
    (list outcome seen)))

(define-syntax-rule (motile/compile e) (scheme/compile e #f))
(define-syntax-rule (motile/decompile c) (c #f #f #f))
(define rte/BASE (vector #f))
(define e/BASE (vector #f)) ; The empty frame at the bottom of the stack.
(define k/RETURN (lambda (x) x)) ; The triial continuation.
;; Execute Motile function f in the context of Motile global binding environment E.
(define-syntax-rule (motile/start f E) (f k/RETURN e/BASE E))
(define (motile/start* f E . rest)
  (apply f (list* k/RETURN e/BASE E rest)))

;; One or more closed variables in lambda body.
;; m: total number of formal parameters
;; n: total number of closed variables
;; addresses: list of frame addresses of closed variables
;; body: code body of lambda expression
(define (closure/generate m n addresses body)
    (cond
      ((= m 0) (xclosure/0/N/generate n addresses body))
      ((= m 1) (xclosure/1/N/generate n addresses body))
      ((= m 2) (xclosure/2/N/generate n addresses body))
      ((= m 3) (xclosure/3/N/generate n addresses body))
      (else    (xclosure/M/N/generate m n addresses body))))

;; Used only in recompilation during deserialization.
;; The closure bindings are reset separately at the very end of the deserialization.
(define (closure/inner/generate m body)
  (let
      ; Regenerate the closure with a fake binding arity, 0, and an empty list of frame addresses.
      ((outer (closure/generate m 0 null body)))
    ; Obtain the inner closure by evaluating it as one would at the point of definition but with an empty run-time stack.
    ; Note that this is safe since we delay generating bindings from frame addresses until application or serialization.
    ; In any case, since the list of frame addresses is null, the inner closure will never crawl the run-time stack.
    (outer rtk/RETURN #f)))

;; Used only in recompilation during deserialization.
;; The closure bindings are reset separately at the very end of the deserialization.
(define (closure/rest/inner/generate m body)
  (let
      ; Regenerate the closure with a fake binding arity, 0, and an empty list of frame addresses.
      ((outer (closure/rest/generate m 0 null body)))
    ; Obtain the inner closure by evaluating it as one would at the point of definition but with an empty run-time stack.
    ; Note that this is safe since we delay generating bindings from frame addresses until either application or serialization,
    ; whichever comes first. Even so, as the list of frame addresses is null, the inner closure will never crawl the run-time stack.
    (outer rtk/RETURN #f)))

;; No closed variables in lambda body.
;; m: total number of formal parameters
;; body: code body of lambda expression.
(define (lambda/generate m body)
  (cond
    ((= m 0) (motile/lambda/0/generate body)) ;; !! Experimental 2011.06.16     ;(lambda/0/generate body))
    ((= m 1) (motile/lambda/1/generate body))
    ((= m 2) (motile/lambda/2/generate body))
    ((= m 3) (motile/lambda/3/generate body))
    (else    (motile/lambda/N/generate m body))))

;; One or closed variables in the lambda body and the last formal parameter is a rest argument.
;; m: total number of formal parameters including the rest argument
;; addresses: list of lexical addresses of closed variables
;; body: code body of lambda expression
(define (closure/rest/generate m n addresses body)
  (cond
    ((= m 1) (closure/rest/1/N/generate n addresses body))
    ((= m 2) (closure/rest/2/N/generate n addresses body))
    ((= m 3) (closure/rest/3/N/generate n addresses body))
    ((= m 4) (closure/rest/4/N/generate n addresses body))
    (else    (closure/rest/M/N/generate m n addresses body))))

;; No closed variables in the lambda body and the last formal parameter is a rest argument.
;; m: total number of formal parameters including the rest argument
;; body: code body of lambda expression
(define (lambda/rest/generate m body)
  (cond
    ((= m 1) (lambda/rest/1/generate body))
    ((= m 2) (lambda/rest/2/generate body))
    ((= m 3) (lambda/rest/3/generate body))
    ((= m 4) (lambda/rest/4/generate body))
    (else    (lambda/rest/N/generate m body))))

(define (lambda/rest/inner/generate m body)
  (let ((code (lambda/rest/generate m body)))
    (code rtk/RETURN #f)))

;; Given a run-time environ and a set of references to that environ construct
;; a vector of the values of those references.
(define (closure/values/make rte addresses)
  (let ((values (make-vector (length addresses))))
    ; Collect the values of the free variables in the context of the definition environment.
    (let loop ((i 0) (addresses addresses))
      (unless (null? addresses)
        (vector-set! values i (reference/rte/free rte (car addresses)))
        (loop (add1 i) (cdr addresses))))
    values))

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


;(define-syntax-rule (bind! bindings e addresses)
;  (unless bindings
;    (set! bindings (closure/values/make e addresses))))

;; Zero lambda parameters + N > 0 closed variables.
(define (closure/0/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)   ; No closure bindings have been constructed.
               (descriptor #f) ; No closure descriptor has been constructed.
               (n n))
           
           (lambda (k e g) 
             (if k
                 (let ((frame (make-vector (add1 n))))
                   (vector-set! frame 0 e/define)
                   ; Delay construction of closure bindings until after the letrec bindings (if any) are fully resolved
                   ; by resolving the bindings (exactly once) at closure application time NOT at closure definition time.
                   (bind! bindings (closure/values/make e/define addresses))
                   (vector-copy! frame 1 bindings) ; Add the closure bindings to the tail of the frame.
                   (body k frame g))
                 
                 ; k is #f so we are either decompiling or recompiling.
                 (when (number? e)
                   (cond
                     ((zero? e)
                      ; We are decompiling.
                      (bind! bindings (closure/values/make e/define addresses))
                      (return!
                       descriptor
                       (vector 'closure/inner 0 n bindings (motile/decompile body))))
                     ((positive? e)
                      ; We are recompiling. e > 0 is the total number of closed variables and g is a vector of bindings for the closed variables.
                      (set! bindings g)
                      (set! n e))))))))
        
        ; We are decompiling the closure at point of definition. 
        (vector 'closure 0 n addresses (motile/decompile body)))))

;; Let s be a frame skeleton where slot 0 is reserved for the environment at point of definition,
;; slots 1, ..., offset-1, are reserved for the calling arguments,
;; and slots offset, offset+1, ... are reserved for the closed variable bindings.
;; Insert the closed variable bindings into the tail of s and return s.
(define (skeleton/fill! s offset e addresses)
  (vector-set! s 0 e)
  (let loop ((i 0) (addresses addresses))
    (unless (null? addresses)
      (vector-set! s (+ i offset) (reference/rte/free e (car addresses)))
      (loop (add1 i) (cdr addresses))))
  s)

;; Create a fresh empty frame skeleton.
;; m - the number of arguments >= 0
;; n - the number of closed variables > 0
;; A frame skeleton has the structure:
;;   slot 0 - prior lexical environ
;;   slots 1 ... m - the m lambda arguments
;;   slots m+1 ... m+n - the n closed variable bindings
(define-syntax-rule (skeleton/new m n) (make-vector (+ 1 m n) #f))

(define-syntax-rule (error/closure/arity tag m n)
  (error tag "closure expects ~a argument(s) but received ~a" m n))

;; For decompilation k = #f, e = 0, and g = #f
(define-syntax-rule (decompile? k e g) (and (not k) (not e) (not g)))


;; Zero lambda parameters + N > 0 closed variables.
(define (xclosure/0/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((frame/skeleton #f) ; No closure bindings have been constructed.
               (descriptor     #f) ; No closure descriptor has been constructed.
               (n n))              ; Total number of closed variables.
           
           (lambda (k e g) 
             (cond
               (k
                ; Delay construction of closure bindings until after any letrec bindings are fully defined
                ; by resolving the closure bindings (exactly once) at closure application time NOT at closure definition time.
                (unless frame/skeleton
                  (set! frame/skeleton (skeleton/fill! (skeleton/new 0 n) 1 e/define addresses)))
                (body k frame/skeleton g)) ; Since there are no arguments it is perfectly safe to use the frame/skeleton itself.
               
               ((decompile? k e g)
                (return!
                 descriptor
                 (vector-immutable 'closure/inner 0 n e/define addresses (motile/decompile body))))
               
               (else
                (error 'closure/0/N/generate "Internal error"))))))
        
        ; We are decompiling the closure at point of definition. 
        (vector 'closure 0 n addresses (motile/decompile body)))))

(define (xclosure/1/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((frame/skeleton #f)   ; No closure bindings have been constructed.
               (descriptor     #f)   ; No closure descriptor has been constructed.
               (n n))
           (case-lambda
             ((k _ g a)
              ; Delay construction of closure bindings until after the letrec bindings (if any) are fully defined
              ; by resolving the closure bindings (exactly once) at closure application time NOT at closure definition time.
              (unless frame/skeleton
                (set! frame/skeleton (skeleton/fill! (skeleton/new 1 n) 2 e/define addresses)))
              (let ((frame (vector-copy frame/skeleton)))
                (vector-set! frame 1 a)
                (body k frame g)))

              ((k e g)
               (if (decompile? k e g)
                   (return!
                    descriptor
                    (vector-immutable 'closure/inner 1 n e/define addresses (motile/decompile body)))

                   (error/closure/arity 'closure/1/N/generate 1 0)))

              (rest (error/closure/arity 'closure/1/N/generate 1 (length rest))))))
        
        ; We are decompiling the closure at point of definition. 
        (vector 'closure 1 n addresses (motile/decompile body)))))

(define (xclosure/2/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((frame/skeleton #f)   ; No closure bindings have been constructed.
               (descriptor     #f)   ; No closure descriptor has been constructed.
               (n n))
           (case-lambda
             ((k _ g a b)
              ; Delay construction of closure bindings until after the letrec bindings (if any) are fully defined
              ; by resolving the closure bindings (exactly once) at closure application time NOT at closure definition time.
              (unless frame/skeleton
                (set! frame/skeleton (skeleton/fill! (skeleton/new 2 n) 3 e/define addresses)))
              (let ((frame (vector-copy frame/skeleton)))
                (vector-set! frame 1 a)
                (vector-set! frame 2 b)
                (body k frame g)))

              ((k e g)
               (if (decompile? k e g)
                   (return!
                    descriptor
                    (vector-immutable 'closure/inner 2 n e/define addresses (motile/decompile body)))

                   (error/closure/arity 'closure/2/N/generate 2 0)))

              (rest (error/closure/arity 'closure/2/N/generate 2 (length rest))))))
        
        ; We are decompiling the closure at point of definition. 
        (vector 'closure 1 n addresses (motile/decompile body)))))

(define (xclosure/3/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((frame/skeleton #f)   ; No closure bindings have been constructed.
               (descriptor     #f)   ; No closure descriptor has been constructed.
               (n n))
           (case-lambda
             ((k _ g a b c)
              ; Delay construction of closure bindings until after the letrec bindings (if any) are fully defined
              ; by resolving the closure bindings (exactly once) at closure application time NOT at closure definition time.
              (unless frame/skeleton
                (set! frame/skeleton (skeleton/fill! (skeleton/new 3 n) 4 e/define addresses)))
              (let ((frame (vector-copy frame/skeleton)))
                (vector-set! frame 1 a)
                (vector-set! frame 2 b)
                (vector-set! frame 3 c)
                (body k frame g)))

              ((k e g)
               (if (decompile? k e g)
                   (return!
                    descriptor
                    (vector-immutable 'closure/inner 3 n e/define addresses (motile/decompile body)))

                   (error/closure/arity 'closure/3/N/generate 3 0)))

              (rest (error/closure/arity 'closure/3/N/generate 3 (length rest))))))
        
        ; We are decompiling the closure at point of definition. 
        (vector 'closure 1 n addresses (motile/decompile body)))))

;; For closures that have m > 3 required arguments and n > 0 closed variables.
(define (xclosure/M/N/generate m n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((frame/skeleton #f)   ; No closure bindings have been constructed.
               (descriptor     #f)   ; No closure descriptor has been constructed.
               (n n))
           (case-lambda
             ((k _ g a b c . rest)
              (when (or (null? rest) (not (= (length rest) (- m 3))))
                (error/closure/arity 'closure/M/N/generate m (+ m (length rest))))
              ; Delay construction of closure bindings until after the letrec bindings (if any) are fully defined
              ; by resolving the closure bindings (exactly once) at closure application time NOT at closure definition time.
              (unless frame/skeleton
                (set! frame/skeleton (skeleton/fill! (skeleton/new m n) (add1 m) e/define addresses)))
              (let ((frame (vector-copy frame/skeleton)))
                (vector-set! frame 1 a)
                (vector-set! frame 2 b)
                (vector-set! frame 3 c)
                (let loop ((i 4) (rest rest))
                  (when (<= i m)
                    (vector-set! frame i (car rest))
                    (loop (add1 i) (cdr rest))))
                (body k frame g)))

             ((k e g)
               (if (decompile? k e g)
                   (return!
                    descriptor
                    (vector-immutable 'closure/inner m n e/define addresses (motile/decompile body)))

                   (error/closure/arity 'closure/M/N/generate m 0))))))
        
        ; We are decompiling the closure at point of definition. 
        (vector 'closure m n addresses (motile/decompile body)))))

;; One lambda parameter + n > 0 closed variables.
;(define (closure/1/N/generate n addresses body)
;  (lambda (rtk rte) ; rtk and rte are the continuation and environment respectively at point of DEFINITION.
;    (if rtk
;        (let ((bindings #f)   ; No closure bindings have been constructed.
;              (descriptor #f) ; No closure descriptor has been constructed.
;              (rte rte)       ; To reset the global binding environment per closure on reconstruction.
;              (n n))
;          ; Return the definition of the closure to the point of definition.
;          (rtk
;           (lambda (k a) ; k and a are the continuation and argument respectively at point of APPLICATION.
;             (if k
;                 (let ((frame (make-vector (+ 2 n))))
;                   (vector-set! frame 0 rte)
;                   (vector-set! frame 1 a)
;                   ; Delay construction of closure bindings until after the letrec bindings (if any) are fully resolved.
;                   ; Note that the construction occurs at most once!
;                   (unless bindings
;                     (set! bindings (closure/values/make rte addresses)))
;                   (vector-copy! frame 2 bindings) ; Add the closure bindings to the tail of the frame.
;                   (body k frame))
;                 
;                 (cond
;                   ((vector? a)
;                    ; Reset the bindings to the recompiled values.
;                    (set! bindings a)
;                    (set! n (vector-length a)))
;                   ((pair? a)
;                    ; Reset the global binding environment where a = (#(#f <globals>)).
;                    (set! rte (car a)))
;                   (else
;                    ; If necessary generate the bindings for the closure.
;                    (unless bindings
;                      (set! bindings (closure/values/make rte addresses)))
;                    (unless descriptor
;                      (set! descriptor (vector 'closure/inner 1 n bindings (body #f #f))))
;                    descriptor))))))
;        
;        (vector 'closure 1 n addresses (body #f #f)))))

(define (closure/1/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)   ; No closure bindings have been constructed.
               (descriptor #f) ; No closure descriptor has been constructed.
               (n n))
           (case-lambda
             ((k e g a)
                 (let ((frame (make-vector (+ 2 n))))
                   (vector-set! frame 0 e)
                   (vector-set! frame 1 a)
                   ; Delay construction of closure bindings until after the letrec bindings (if any) are fully resolved
                   ; by resolving the bindings (exactly once) at closure application time NOT at closure definition time.
                   (bind! bindings (closure/values/make e/define addresses))
                   (vector-copy! frame 2 bindings) ; Add the closure bindings to the tail of the frame.
                   (body k frame g)))
             ((k e g)
                 ; k is #f so we are either decompiling or recompiling.
                 (when (and (not k) (number? e))
                   (cond
                     ((zero? e)
                      ; We are decompiling
                      (bind! bindings (closure/values/make e/define addresses))
                      (return!
                       descriptor
                       (vector 'closure/inner 1 n bindings (motile/decompile body))))
                     ((positive? e)
                      ; We are recompiling. e > 0 is the total number of closed variables and g is a vector of bindings for the closed variables.
                      (set! bindings g)
                      (set! n e))))))))
        
        ; We are decompiling the closure at point of definition. 
        (vector 'closure 1 n addresses (motile/decompile body)))))

;; Two lambda parameters + N > 0 closed variables.
;(define (closure/2/N/generate n addresses body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((bindings #f)
;              (descriptor #f)
;              (rte rte)
;              (n n))
;          (rtk
;           (case-lambda
;             ((k a b)
;              (let ((frame (make-vector (+ 3 n))))
;                (vector-set! frame 0 rte)
;                (vector-set! frame 1 a)
;                (vector-set! frame 2 b)
;                (unless bindings
;                  (set! bindings (closure/values/make rte addresses)))
;                (vector-copy! frame 3 bindings)
;                (body k frame)))
;             ((k x)
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Reset the bindings to the recompiled values.
;                   (set! bindings x)
;                   (set! n (vector-length x)))
;                  (else
;                   ; If necessary generate the bindings for the closure.
;                   (unless bindings
;                     (set! bindings (closure/values/make rte addresses)))
;                   (unless descriptor
;                     (set! descriptor (vector 'closure/inner 2 n bindings (body #f #f))))
;                   descriptor)))))))
;        
;        (vector 'closure 2 n addresses (body #f #f)))))

(define (closure/2/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)   ; No closure bindings have been constructed.
               (descriptor #f) ; No closure descriptor has been constructed.
               (n n))
           (case-lambda
             ((k e g a b)
                 (let ((frame (make-vector (+ 3 n))))
                   (vector-set! frame 0 e)
                   (vector-set! frame 1 a)
                   (vector-set! frame 2 b)
                   ; Delay construction of closure bindings until after the letrec bindings (if any) are fully resolved
                   ; by resolving the bindings (exactly once) at closure application time NOT at closure definition time.
                   (bind! bindings (closure/values/make e/define addresses))
                   (vector-copy! frame 3 bindings) ; Add the closure bindings to the tail of the frame.
                   (body k frame g)))
             ((k e g)
                 ; k is #f so we are either decompiling or recompiling.
                 (when (and (not k) (number? e))
                   (cond
                     ((zero? e)
                      ; We are decompiling
                      (bind! bindings (closure/values/make e/define addresses))
                      (return!
                       descriptor
                       (vector 'closure/inner 2 n bindings (motile/decompile body))))
                     ((positive? e)
                      ; We are recompiling. e > 0 is the total number of closed variables and g is a vector of bindings for the closed variables.
                      (set! bindings g)
                      (set! n e))))))))
        
        ; We are decompiling the closure at point of definition. 
        (vector 'closure 2 n addresses (motile/decompile body)))))
  
;; Three lambda parameters + N > 0 closed variables.
;(define (closure/3/N/generate n addresses body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((bindings #f)
;              (descriptor #f)
;              (rte rte)
;              (n n))
;          (rtk
;           (case-lambda
;             ((k a b c)
;              (let ((frame (make-vector (+ 4 n))))
;                (vector-set! frame 0 rte)
;                (vector-set! frame 1 a)
;                (vector-set! frame 2 b)
;                (vector-set! frame 3 c)
;                (unless bindings
;                  (set! bindings (closure/values/make rte addresses)))
;                (vector-copy! frame 4 bindings)
;                (body k frame)))
;             ((k x)
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Reset the bindings to the recompiled values.
;                   (set! bindings x)
;                   (set! n (vector-length x)))
;                  (else
;                   ; If necessary generate the bindings for the closure.
;                   (unless bindings
;                     (set! bindings (closure/values/make rte addresses)))
;                   (unless descriptor
;                     (set! descriptor (vector 'closure/inner 3 n bindings (body #f #f))))
;                   descriptor)))))))
;        
;        (vector 'closure 3 n addresses (body #f #f)))))

(define (closure/3/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)   ; No closure bindings have been constructed.
               (descriptor #f) ; No closure descriptor has been constructed.
               (n n))
           (case-lambda
             ((k e g a b c)
                 (let ((frame (make-vector (+ 4 n))))
                   (vector-set! frame 0 e)
                   (vector-set! frame 1 a)
                   (vector-set! frame 2 b)
                   (vector-set! frame 3 c)
                   ; Delay construction of closure bindings until after the letrec bindings (if any) are fully resolved
                   ; by resolving the bindings (exactly once) at closure application time NOT at closure definition time.
                   (bind! bindings (closure/values/make e/define addresses))
                   (vector-copy! frame 4 bindings) ; Add the closure bindings to the tail of the frame.
                   (body k frame g)))
             ((k e g)
                 ; k is #f so we are either decompiling or recompiling.
                 (when (and (not k) (number? e))
                   (cond
                     ((zero? e)
                      ; We are decompiling
                      (bind! bindings (closure/values/make e/define addresses))
                      (return!
                       descriptor
                       (vector 'closure/inner 3 n bindings (motile/decompile body))))
                     ((positive? e)
                      ; We are recompiling. e > 0 is the total number of closed variables and g is a vector of bindings for the closed variables.
                      (set! bindings g)
                      (set! n e))))))))
        
        ; We are decompiling the closure at point of definition. 
        (vector 'closure 3 n addresses (motile/decompile body)))))




;; M > 3 lambda parameters + N > 0 closed variables.
;(define (closure/M/N/generate m n addresses body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((bindings #f)
;              (descriptor #f)
;              (rte rte)
;              (n n))
;          (rtk
;           (case-lambda
;             ((k a b c . d)
;              (let ((frame (make-vector (+ 1 m n))))
;                (vector-set! frame 0 rte)
;                (vector-set! frame 1 a)
;                (vector-set! frame 2 b)
;                (vector-set! frame 3 c)
;                (let loop ((i 4) (rest d))
;                  (when (<= i m)
;                    (vector-set! frame i (car rest))
;                    (loop (add1 i) (cdr rest))))
;                (unless bindings
;                  (set! bindings (closure/values/make rte addresses)))
;                (vector-copy! frame (add1 m) bindings)
;                (body k frame)))
;             ((k x)
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Reset the bindings to the recompiled values.
;                   (set! bindings x)
;                   (set! n (vector-length x)))
;                  (else
;                   ; If necessary generate the bindings for the closure.
;                   (unless bindings
;                     (set! bindings (closure/values/make rte addresses)))
;                   (unless descriptor
;                     (set! descriptor (vector 'closure/inner m n bindings (body #f #f))))
;                   descriptor)))))))
;
;        (vector 'closure m n addresses (body #f #f)))))

(define (closure/M/N/generate m n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)   ; No closure bindings have been constructed.
               (descriptor #f) ; No closure descriptor has been constructed.
               (n n))
           (case-lambda
             ((k e g a b c . d)
                 (let ((frame (make-vector (+ 1 m n))))
                   (vector-set! frame 0 e)
                   (vector-set! frame 1 a)
                   (vector-set! frame 2 b)
                   (vector-set! frame 3 c)
                   (let loop ((i 4) (rest d))
                     (when (<= i m)
                       (vector-set! frame i (car rest))
                       (loop (add1 i) (cdr rest))))
                   ; Delay construction of closure bindings until after the letrec bindings (if any) are fully resolved
                   ; by resolving the bindings (exactly once) at closure application time NOT at closure definition time.
                   (bind! bindings (closure/values/make e/define addresses))
                   (vector-copy! frame (add1 m) bindings) ; Add the closure bindings to the tail of the frame.
                   (body k frame g)))
             ((k e g)
                 ; k is #f so we are either decompiling or recompiling.
                 (when (and (not k) (number? e))
                   (cond
                     ((zero? e)
                      ; We are decompiling
                      (bind! bindings (closure/values/make e/define addresses))
                      (return!
                       descriptor
                       (vector 'closure/inner m n bindings (motile/decompile body))))
                     ((positive? e)
                      ; We are recompiling. e > 0 is the total number of closed variables and g is a vector of bindings for the closed variables.
                      (set! bindings g)
                      (set! n e))))))))
        
        ; We are decompiling the closure at point of definition. 
        (vector 'closure m n addresses (motile/decompile body)))))



;; One rest lambda parameter + N > 0 closed variables.
;(define (closure/rest/1/N/generate n addresses body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((bindings #f)
;              (descriptor #f)
;              (rte rte)
;              (n n))
;          (rtk
;           (lambda (k . rest)
;             (if k
;                 (let ((frame (make-vector (+ 2 n))))
;                   (vector-set! frame 0 rte)
;                   (vector-set! frame 1 rest)
;                   (unless bindings
;                     (set! bindings (closure/values/make rte addresses)))
;                   (vector-copy! frame 2 bindings)
;                   (body k frame))
;                 
;                 (let ((x (car rest)))
;                   (cond
;                     ((pair? x)
;                      ; Reset the global binding environment where a = (#(#f <globals>)).
;                      (set! rte (car x)))
;                     ((vector? x)
;                      ; Reset the bindings to the recompiled values.
;                      (set! bindings x)
;                      (set! n (vector-length x)))
;                     (else
;                      ; If necessary generate the bindings for the closure.
;                      (unless bindings
;                        (set! bindings (closure/values/make rte addresses)))
;                      (unless descriptor
;                        (set! descriptor (vector 'closure/rest/inner 1 n bindings (body #f #f))))
;                      descriptor)))))))
;        
;        (vector 'closure/rest 1 n addresses (body #f #f)))))

(define (closure/rest/1/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)
               (descriptor #f)
               (n n))
           
           (lambda (k e g . rest)
             (if k
                 (let ((frame (make-vector (+ 2 n))))
                   (vector-set! frame 0 e)
                   (vector-set! frame 1 rest)
                   (bind! bindings (closure/values/make e/define addresses))
                   (vector-copy! frame 2 bindings)
                   (body k frame g))
                 
                 (when (number? e)
                   (cond
                     ((zero? e)
                      ; We are decompiling
                      (bind! bindings (closure/values/make e/define addresses))
                      (return!
                       descriptor
                       (vector 'closure/rest/inner 1 n bindings (motile/decompile body))))
                     ((positive? e)
                      ; We are recompiling. e > 0 is the total number of closed variables and g is a vector of bindings for the closed variables.
                      (set! bindings g)
                      (set! n e))))))))
        
        (vector 'closure/rest 1 n addresses (motile/decompile body)))))

;; Two lambda parameters including one rest parameter + N > 0 closed variables.
;(define (closure/rest/2/N/generate n addresses body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((bindings #f)
;              (descriptor #f)
;              (rte rte)
;              (n n))
;          (rtk
;           (lambda (k a . rest)
;             (if k
;                 (let ((frame (make-vector (+ 3 n))))
;                   (vector-set! frame 0 rte)
;                   (vector-set! frame 1 a)
;                   (vector-set! frame 2 rest)
;                   (unless bindings
;                     (set! bindings (closure/values/make rte addresses)))
;                   (vector-copy! frame 3 bindings)
;                   (body k frame))
;                 
;                 (cond
;                   ((pair? a)
;                    ; Reset the global binding environment where a = (#(#f <globals>)).
;                    (set! rte (car a)))
;                   ((vector? a)
;                    ; Reset the bindings to the recompiled values.
;                    (set! bindings a)
;                    (set! n (vector-length a)))
;                   (else
;                    ; If necessary generate the bindings for the closure.
;                    (unless bindings
;                      (set! bindings (closure/values/make rte addresses)))
;                    (unless descriptor
;                      (set! descriptor (vector 'closure/rest/inner 2 n bindings (body #f #f))))
;                    descriptor))))))
;
;        (vector 'closure/rest 2 n addresses (body #f #f)))))

(define (closure/rest/2/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)
               (descriptor #f)
               (n n))
           
           (lambda (k e g a . rest)
             (if k
                 (let ((frame (make-vector (+ 3 n))))
                   (vector-set! frame 0 e)
                   (vector-set! frame 1 a)
                   (vector-set! frame 2 rest)
                   (bind! bindings (closure/values/make e/define addresses))
                   (vector-copy! frame 3 bindings)
                   (body k frame g))
                 
                 (when (number? e)
                   (cond
                     ((zero? e)
                      ; We are decompiling
                      (bind! bindings (closure/values/make e/define addresses))
                      (return!
                       descriptor
                       (vector 'closure/rest/inner 2 n bindings (motile/decompile body))))
                     ((positive? e)
                      ; We are recompiling. e > 0 is the total number of closed variables and g is a vector of bindings for the closed variables.
                      (set! bindings g)
                      (set! n e))))))))
        
        (vector 'closure/rest 2 n addresses (motile/decompile body)))))

;; Three lambda parameters including one rest parameter + N > 0 closed variables.
;(define (closure/rest/3/N/generate n addresses body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((bindings #f)
;              (descriptor #f)
;              (rte rte)
;              (n n))
;          (rtk
;           (case-lambda
;             ((k a b . rest)
;              (let ((frame (make-vector (+ 4 n))))
;                (vector-set! frame 0 rte)
;                (vector-set! frame 1 a)
;                (vector-set! frame 2 b)
;                (vector-set! frame 3 rest)
;                (unless bindings
;                  (set! bindings (closure/values/make rte addresses)))
;                (vector-copy! frame 4 bindings)
;                (body k frame)))
;             ((k x)
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Reset the bindings to the recompiled values.
;                   (set! bindings x)
;                   (set! n (vector-length x)))
;                  (else
;                   ; If necessary generate the bindings for the closure.
;                   (unless bindings
;                     (set! bindings (closure/values/make rte addresses)))
;                   (unless descriptor
;                     (set! descriptor (vector 'closure/rest/inner 3 n bindings (body #f #f))))
;                   descriptor)))))))
;        
;        (vector 'closure/rest 3 n addresses (body #f #f)))))


(define (closure/rest/3/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)
               (descriptor #f)
               (n n))
           
           (lambda (k e g a b . rest)
             (if k
                 (let ((frame (make-vector (+ 4 n))))
                   (vector-set! frame 0 e)
                   (vector-set! frame 1 a)
                   (vector-set! frame 2 b)
                   (vector-set! frame 3 rest)
                   (bind! bindings (closure/values/make e/define addresses))
                   (vector-copy! frame 4 bindings)
                   (body k frame g))
                 
                 (when (number? e)
                   (cond
                     ((zero? e)
                      ; We are decompiling
                      (bind! bindings (closure/values/make e/define addresses))
                      (return!
                       descriptor
                       (vector 'closure/rest/inner 3 n bindings (motile/decompile body))))
                     ((positive? e)
                      ; We are recompiling. e > 0 is the total number of closed variables and g is a vector of bindings for the closed variables.
                      (set! bindings g)
                      (set! n e))))))))
        
        (vector 'closure/rest 3 n addresses (motile/decompile body)))))

;; Four lambda parameters including one rest parameter + N > 0 closed variables.
;(define (closure/rest/4/N/generate n addresses body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((bindings #f)
;              (descriptor #f)
;              (rte rte)
;              (n n))
;          (rtk
;           (case-lambda 
;             ((k a b c . rest)
;              (let ((frame (make-vector (+ 5 n))))
;                (vector-set! frame 0 rte)
;                (vector-set! frame 1 a)
;                (vector-set! frame 2 b)
;                (vector-set! frame 3 c)
;                (vector-set! frame 4 rest)
;                (unless bindings
;                  (set! bindings (closure/values/make rte addresses)))
;                (vector-copy! frame 5 bindings)
;                (body k frame)))
;             ((k x)
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Reset the bindings to the recompiled values.
;                   (set! bindings x)
;                   (set! n (vector-length x)))
;                  (else
;                   ; If necessary generate the bindings for the closure.
;                   (unless bindings
;                     (set! bindings (closure/values/make rte addresses)))
;                   (unless descriptor
;                     (set! descriptor (vector 'closure/rest/inner 4 n bindings (body #f #f))))
;                   descriptor)))))))
;
;        (vector 'closure/rest 4 n addresses (body #f #f)))))


(define (closure/rest/4/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)
               (descriptor #f)
               (n n))
           
           (lambda (k e g a b c . rest)
             (if k
                 (let ((frame (make-vector (+ 5 n))))
                   (vector-set! frame 0 e)
                   (vector-set! frame 1 a)
                   (vector-set! frame 2 b)
                   (vector-set! frame 3 c)
                   (vector-set! frame 4 rest)
                   (bind! bindings (closure/values/make e/define addresses))
                   (vector-copy! frame 5 bindings)
                   (body k frame g))
                 
                 (when (number? e)
                   (cond
                     ((zero? e)
                      ; We are decompiling
                      (bind! bindings (closure/values/make e/define addresses))
                      (return!
                       descriptor
                       (vector 'closure/rest/inner 4 n bindings (motile/decompile body))))
                     ((positive? e)
                      ; We are recompiling. e > 0 is the total number of closed variables and g is a vector of bindings for the closed variables.
                      (set! bindings g)
                      (set! n e))))))))
        
        (vector 'closure/rest 4 n addresses (motile/decompile body)))))

;; M > 4 lambda parameters including one rest parameter + N > 0 closed variables.
;(define (closure/rest/M/N/generate m n addresses body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((bindings #f)
;              (descriptor #f)
;              (rte rte)
;              (n n))
;          (rtk
;           (case-lambda 
;             ((k a b c . rest)
;              (let ((frame (make-vector (+ 1 m n))))
;                (vector-set! frame 0 rte)
;                (vector-set! frame 1 a)
;                (vector-set! frame 2 b)
;                (vector-set! frame 3 c)
;                (let loop ((i 4) (rest rest))
;                  (cond
;                    ((< i m)
;                     (vector-set! frame i (car rest))
;                     (loop (add1 i) (cdr rest)))
;                    (else
;                     (vector-set! frame m rest))))
;                (unless bindings
;                  (set! bindings (closure/values/make rte addresses)))
;                (vector-copy! frame (add1 m) bindings)
;                (body k frame)))
;             ((k x)
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Reset the bindings to the recompiled values.
;                   (set! bindings x)
;                   (set! n (vector-length x)))
;                  (else
;                   ; If necessary generate the bindings for the closure.
;                   (unless bindings
;                     (set! bindings (closure/values/make rte addresses)))
;                   (unless descriptor
;                     (set! descriptor (vector 'closure/rest/inner m n bindings (body #f #f))))
;                   descriptor)))))))
;        
;    (vector 'closure/rest m n addresses (body #f #f)))))

(define (closure/rest/M/N/generate m n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)
               (descriptor #f)
               (n n))
           
           (lambda (k e g a b c . rest)
             (if k
                 (let ((frame (make-vector (+ 1 m n))))
                   (vector-set! frame 0 e)
                   (vector-set! frame 1 a)
                   (vector-set! frame 2 b)
                   (vector-set! frame 3 c)
                   (let loop ((i 4) (rest rest))
                     (cond
                       ((< i m)
                        (vector-set! frame i (car rest))
                        (loop (add1 i) (cdr rest)))
                       (else
                        (vector-set! frame m rest))))
                   (bind! bindings (closure/values/make e/define addresses))
                   (vector-copy! frame (add1 m) bindings)
                   (body k frame g))
                 
                 (when (number? e)
                   (cond
                     ((zero? e)
                      ; We are decompiling
                      (bind! bindings (closure/values/make e/define addresses))
                      (return!
                       descriptor
                       (vector 'closure/rest/inner m n bindings (motile/decompile body))))
                     ((positive? e)
                      ; We are recompiling. e > 0 is the total number of closed variables and g is a vector of bindings for the closed variables.
                      (set! bindings g)
                      (set! n e))))))))
        
        (vector 'closure/rest m n addresses (motile/decompile body)))))

;; Used only by the recompiler.
(define (lambda/inner/generate m body)
  (let ((code (lambda/generate m body)))
    (code rtk/RETURN #f)))

;; In the following four code generation routines
;; rte: the run-time-environment at the point of DEFINITION of the source procedure lambda body
;; body: the code generated for the source procedure lambda body
;; A source procedure f is invoked (f k rte global a_1 ... a_N) where k is the continuation and a_i is an argument to f

;; Zero lambda parameters + zero closed variables.
;(define (lambda/0/generate body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((descriptor #f)
;              (rte rte))
;          (rtk
;           (case-lambda
;             ((k) (body k rte))
;             ((k x) ; Always invoked as (#f X).
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Ignore any attempt to reset the nonexistent bindings for closed variables.
;                   (void))
;                  (else
;                   ; x is #f. Return a descriptor.
;                   (unless descriptor
;                     (set! descriptor (vector 'lambda/inner 0 (body #f #f))))
;                   descriptor)))))))
;
;        (vector 'lambda 0 (body #f #f)))))

(define (motile/lambda/0/generate body)
  ; This outermost (lambda (k/define e/define g/define) ...) is evaluated at the point of lambda DEFINITION and if the
  ; continuation k/define is not #f returns the closure (lambda (k n global) ...) suitable for use at point of APPLICATION.
  ; k/define - continuation at point of DEFINTION or #f
  ; e/define - run-time environ (stack) at point of DEFINITION or #f.
  ; g/define - global binding environment at point of DEFINITION (used)
    (lambda (k/define e/define g/define)
      (if k/define
          (let ((descriptor #f))
            (k/define
             (lambda (k e g)
               (cond
                 (k (body k e/define g))
                 ((decompile? k e g)
                  (return!
                   descriptor
                   (vector 'lambda/inner 0 e/define (motile/decompile body))))))))

        ; Return decompilation for lambda at point of lambda DEFINITION.
        (vector 'lambda 0 (motile/decompile body)))))

;; One lambda parameter + zero closed variables.
;(define (lambda/1/generate body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((descriptor #f)
;              (rte rte))
;          (rtk
;           (lambda (k x)
;             (if k
;                 (body k (vector rte x))
;                 
;                 (cond
;                   ((pair? x)
;                    ; Reset the global binding environment where x = (#(#f <globals>)).
;                    (set! rte (car x)))
;                   ((vector? x)
;                    ; Ignore any attempt to reset the nonexistent bindings for closed variables.
;                    (void))
;                   (else
;                    ; x is #f. Return a descriptor.
;                    (unless descriptor
;                      (set! descriptor (vector 'lambda/inner 1 (body #f #f))))
;                    descriptor))))))
;
;        (vector 'lambda 1 (body #f #f)))))

(define (motile/lambda/1/generate body)
  (lambda (k/define e/define g/define)
    (if k/define
        (let ((descriptor #f))
          (k/define
           (case-lambda
             ((k e g x) (body k (vector e/define x) g))
             ((k e g)
              (when (decompile? k e g)
                (return!
                 descriptor
                 (vector 'lambda/inner 1 e/define (motile/decompile body)))))
             (rest
              (error/closure/arity 'motile/lambda/1/generate 1 (- (length rest) 3))))))
        
        (vector 'lambda 1 (motile/decompile body)))))

;; Two lambda parameters + zero closed variables.
;(define (lambda/2/generate body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((descriptor #f)
;              (rte rte))
;          (rtk 
;           (case-lambda
;             ((k a b) (body k (vector rte a b)))
;             ((k x) ; Always invoked as (#f X).
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Ignore any attempt to reset the nonexistent bindings for closed variables.
;                   (void))
;                  (else
;                   ; x is #f. Return a descriptor.
;                   (unless descriptor
;                     (set! descriptor (vector 'lambda/inner 2 (body #f #f))))
;                   descriptor)))))))
;        
;        (vector 'lambda 2 (body #f #f)))))

(define (motile/lambda/2/generate body)
  (lambda (k/define e/define g/define)
    (if k/define
        (let ((descriptor #f))
          (k/define
           (case-lambda
             ((k _ global a b) (body k (vector e/define a b) global))
             ((k n _)
              (when (and (not k) (zero? n))
                (return!
                 descriptor
                 (vector 'lambda/inner 2 (motile/decompile body))))))))
            
        (vector 'lambda 2 (motile/decompile body)))))

;; Three lambda parameters + zero closed variables.
;(define (lambda/3/generate body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((descriptor #f)
;              (rte rte))
;          (rtk 
;           (case-lambda
;             ((k a b c) (body k (vector rte a b c)))
;             ((k x) ; Always invoked as (#f X).
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Ignore any attempt to reset the nonexistent bindings for closed variables.
;                   (void))
;                  (else
;                   ; x is #f. Return a descriptor.
;                   (unless descriptor
;                     (set! descriptor (vector 'lambda/inner 3 (body #f #f))))
;                   descriptor)))))))
;
;        (vector 'lambda 3 (body #f #f)))))

(define (motile/lambda/3/generate body)
  (lambda (k/define e/define g/define)
    (if k/define
        (let ((descriptor #f))
          (k/define
           (case-lambda
             ((k e global a b c) (body k (vector e a b c) global))
             ((k e _)
              (when (and (not k) (zero? e))
                (return!
                 descriptor
                 (vector 'lambda/inner 3 (motile/decompile body))))))))
        
        (vector 'lambda 3 (motile/decompile body)))))

;; N > 3 lambda parameters + zero closed variables.
;(define (lambda/N/generate n body)
;  (lambda (rtk rte)
;    (if rtk
;        (let ((descriptor #f)
;              (rte rte))
;          (rtk
;           (case-lambda
;             ((k a b c . d)
;              (let ((frame (make-vector (add1 n))))
;                (vector-set! frame 0 rte)
;                (vector-set! frame 1 a)
;                (vector-set! frame 2 b)
;                (vector-set! frame 3 c)
;                (let loop ((i 4) (rest d))
;                  (if (<= i n)
;                      (begin
;                        (vector-set! frame i (car rest))
;                        (loop (add1 i) (cdr rest)))
;                      (body k frame)))))
;             ((k x) ; Always invoked as (#f X).
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where a = (#(#f <globals>)).
;                   (set! rte (car x)))
;                  ((vector? x)
;                   ; Ignore any attempt to reset the nonexistent bindings for closed variables.
;                   (void))
;                  (else
;                   ; x is #f. Return a descriptor.
;                   (unless descriptor
;                     (set! descriptor (vector 'lambda/inner n (body #f #f))))
;                   descriptor)))))))             
;        
;        (vector 'lambda n (body #f #f)))))

(define (motile/lambda/N/generate n body)
  (lambda (k/define e/define g/define)
    (if k/define
        (let ((descriptor #f))
          (k/define
           (case-lambda
             ((k e global a b c . d)
              (let ((frame (make-vector (add1 n))))
                (vector-set! frame 0 e)
                (vector-set! frame 1 a)
                (vector-set! frame 2 b)
                (vector-set! frame 3 c)
                (let loop ((i 4) (rest d))
                  (if (<= i n)
                      (begin
                        (vector-set! frame i (car rest))
                        (loop (add1 i) (cdr rest)))
                      (body k frame global)))))
             ((k e _)
              (when (and (not k) (zero? e))
                (return!
                 descriptor
                 (vector 'lambda/inner n (motile/decompile body))))))))
        
        (vector 'lambda n (motile/decompile body)))))


(define (start f)
  (f rtk/RETURN RTE))

(define (mischief/start x)
  (x rtk/RETURN RTE))








(define (test/frame/bug/1)
  (let ((code
         (motile/compile
          '(let ((f (lambda ()
                      (let ((v (+ 2 1 3)))
                        (let loop ()
                          (let ((z 1))
                            (+ z v)))))))
             (f)))))
    (motile/start code BASELINE)))

;; A simpler version of test/frame/bug/1.
(define (test/frame/bug/1a)
  (let ((code
         (motile/compile
          '(let ((f (lambda ()
                      (let ((v 6))
                        (let loop ()
                          (let ((z 1))
                            (+ z v)))))))
             (f)))))
    (motile/start code BASELINE)))

;; Generates same kind of error as test/frame/bug/1a
(define (test/frame/bug/1b)
  (let ((code
         (motile/compile
          '(let ((v 6))
             (let loop ()
               (let ((z 1))
                 (+ z v)))))))
    (motile/start code BASELINE)))


(define (test/frame/bug/1b.1)
  (let ((code
         (motile/compile
          '((lambda (v)
              (((lambda (loop) ((lambda (loop.381838) 'setter-goes-here ((lambda () (loop))))
                                (lambda () ((lambda (z) (+ z v)) 1))))
                #f)))
            6)
          )))
    (pretty-display (motile/decompile code))
    (motile/start code BASELINE)))
   


(define EXPAND
  ((lambda (v)
     ((lambda (loop)
        ((lambda (f)
           (set! loop f)
           (loop))
         (lambda ()
           ((lambda (z) (+ z v)) 1))))
      #f))
   6)
  )

;; A cheatsheet for the routines below.
;;    ((lambda (v_1 ... v_N)        <--- letrec/outer with letrec variables v_1, ..., v_N
;;       ((lambda (t_1 ... t_N)       <--- letrec/inner
;;          (setter N)                  <--- setter
;;          (let () . <body>))            <--- letrec/body
;;          e_1 ... e_N))             <--- letrec/e_1 letrec/e_2 ...
;;     #f ... #f)                   <--- junk values for v_1 ... v_N of letrec/outer




(define (test/frame/bug/1c)
  (let ((code
         (motile/compile
          '(let ((v 6))
             (let loop ()
               (let ((z 1))
                 (+ z v)))))))
    (pretty-display (motile/decompile code))))

(define (test/closure)
  (define (test/closure/1)
    (let ((code
           (motile/compile
            '(let ((f (lambda (n) (lambda () n))))
               (let ((f100 (f 100))
                     (f200 (f 200))
                     (f300 (f 300)))
                 (list (f100) (f200) (f300)))))))
      (pretty-display (motile/start code BASELINE))))

  (define (test/closure/2)
    (let* ((code
           (motile/compile
            '(let ((f (lambda (n) (lambda () n))))
               (let ((f100 (f 100))
                     (f200 (f 200))
                     (f300 (f 300)))
                 (vector  f100 f200 f300)))))
           (closures (motile/start code ENVIRON/TEST)))
      (pretty-display (motile/decompile (vector-ref closures 0)))
      (newline)
      (pretty-display (motile/decompile (vector-ref closures 1)))
      (newline)
      (pretty-display (motile/decompile (vector-ref closures 1)))))
  
  (test/closure/1)
  (test/closure/2))

(define (test/map/1)
  (let ((code
         (motile/compile
          '(let ((f (lambda (n) (+ n 3))))
             (map f '(5 10 15 20 25))))))
    (motile/start code BASELINE)))

(define (test/map/2)
  (let ((code
         (motile/compile
          '(let ((f (lambda (n) (+ n 3))))
             (map f '(5 10 15 20 25))))))
    (pretty-display (motile/decompile code))))

(define (test/for-each)
  (let ((code
         (motile/compile
          '(let ((f (lambda (n)
                      (display (+ n 3)) (newline))))
             (for-each f '(5 10 15 20 25))))))
    (motile/start code ENVIRON/TEST)))

 (define (test/apply)
  (let ((code
         (motile/compile
          '(apply + 5 10 15 20 25))))
    (motile/start code BASELINE)))
 
 (define (test/sort)
   (let ((code
          (motile/compile
           '(sort '(25 20 5 15 10) <))))
     (motile/start code BASELINE)))
