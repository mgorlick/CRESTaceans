#lang racket

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

(require srfi/1)
(require
 "baseline.rkt"
 "dissect.ss"
 "free.ss"
 (only-in
  "persistent/hash.rkt"
  hash/ref)
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
 should-be
 start
 rtk/RETURN
 RTE
 
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

    ((not (pair? e))
     (constant/generate e))
    
    ((macro? e lexical)
     (scheme/compile (macro/expand e lexical) lexical))
    
    ((quote? e)
     (constant/generate (quotation e)))
    
    ((if? e)
     (if/compile e lexical))
    
    ((cond? e)
     (cond/compile e lexical))

    ((case? e)
     (case/compile e lexical))
    
    ((do? e)
     (do/compile e lexical))

    ((when? e)
     (when/compile e lexical))
    
    ((unless? e)
     (unless/compile e lexical))
    
    ((begin? e)
     (begin/compile e lexical))

    ; (lambda <formals> <body>).
    ((lambda? e)
     (lambda/compile e lexical))
    
    ; (let ((s_1 v_1) ... (s_N v_N)) <body>).
    ((let? e)
     (let/compile e lexical))
    
    ((let*? e)
     (let*/compile e lexical))
    
    ((letrec? e)
     (letrec/compile e lexical))

    ((and? e)
     (and/compile e lexical))
    
    ((or? e)
     (or/compile e lexical))
    
    ; Special forms for binding environments.
    
    ; (environ/cons <environ> <symbol_1> ... <symbol_n>)
    ((environ/cons? e)
     (environ/cons/compile e lexical))
    
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
     (combination/compile e lexical))))

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
      (start procedure))))  ; Evaluate the procedure at its point of definition.
      
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
;;          (let () <body>))
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
  (let loop ((e e))
    (when (vector-ref e 0)
      (pretty-display (cdr (vector->list e)))
      (loop (e/pop e)))))
  

;; Fetch a value from a frame of the run-time environ.
(define (reference/rte/free rte address)
  (let loop ((rte rte)
             (frame  (car address))
             (offset (cdr address)))
    (display (format "frame: ~s offset: ~s\n" frame offset))
    ;(pretty/e rte)
    (newline)
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
    ;(display (format "combination/compile: e: ~a\n" e))
    ;(lexical/dump lexical)
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
  ;(display (format "combination/generate operator: ~a arguments: ~a\n" operator arguments))
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
(define-syntax-rule (motile/decompile c) (c #f 0 #f))
(define rte/BASE (vector #f))
(define e/BASE (vector #f)) ; The empty frame at the bottom of the stack.
(define k/RETURN (lambda (x) x)) ; The triial continuation.
;; Execute Motile function f in the context of Motile global binding environment E.
(define-syntax-rule (motile/start f E) (f k/RETURN e/BASE E))

;; One or more closed variables in lambda body.
;; m: total number of formal parameters
;; n: total number of closed variables
;; addresses: list of frame addresses of closed variables
;; body: code body of lambda expression
(define (closure/generate m n addresses body)
    (cond
      ((= m 0) (closure/0/N/generate n addresses body))
      ((= m 1) (closure/1/N/generate n addresses body))
      ((= m 2) (closure/2/N/generate n addresses body))
      ((= m 3) (closure/3/N/generate n addresses body))
      (else    (closure/M/N/generate m n addresses body))))

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


;; Zero lambda parameters + N > 0 closed variables.
;(define (closure/0/N/generate n addresses body)
;  (lambda (rtk rte) ; rtk and rte are the continuation and environment respectively at point of DEFINITION.
;    (if rtk
;        (let ((bindings #f)   ; No closure bindings have been constructed.
;              (descriptor #f) ; No closure descriptor has been constructed.
;              (rte rte)       ; To reset the global binding environment per closure on reconstruction.
;              (n n))
;          ; Return the definition of the closure to the point of definition.
;          (rtk
;           (case-lambda 
;             ((k) 
;              (let ((frame (make-vector (add1 n))))
;                (vector-set! frame 0 rte)
;                ; Delay construction of closure bindings until after the letrec bindings (if any) are fully resolved.
;                ; Note that the construction occurs at most once!
;                (unless bindings
;                  (set! bindings (closure/values/make rte addresses)))
;                (vector-copy! frame 1 bindings) ; Add the closure bindings to the tail of the frame.
;                (body k frame)))
;             ((k x)
;              (unless k
;                (cond
;                  ((pair? x)
;                   ; Reset the global binding environment where x = (#(#f <globals>)), (car x) = #(#f <globals>).
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
;                     (set! descriptor (vector 'closure/inner 0 n bindings (body #f #f))))
;                   descriptor)))))))
;        
;        ; rtk is #f
;        (vector 'closure 0 n addresses (body #f #f)))))

(define (closure/0/N/generate n addresses body)
  (lambda (k/define e/define g/define)
    (if k/define
        (k/define
         (let ((bindings #f)   ; No closure bindings have been constructed.
               (descriptor #f) ; No closure descriptor has been constructed.
               (n n))
           
           (lambda  (k e g) 
             (if k
                 (let ((frame (make-vector (add1 n))))
                   (vector-set! frame 0 e)
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
             (lambda (k e global)
               (if k
                   (body k e global) ; NB: we capture the stack at point of DEFINITION!
                   
                   ; Return decompilation at point of APPLICATION.
                   (when (zero? e)
                     (return!
                      descriptor
                      (vector 'lambda/inner 0 (motile/decompile body))))))))

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
             ((k e global x) (body k (vector e x) global))
             ((k e _)
              (when (and (not k) (zero? e))
                (return!
                 descriptor
                 (vector 'lambda/inner 1 (motile/decompile body))))))))
        
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


(define (should-be id expected outcome)
  (if (equal? expected outcome)
      (display (format "PASS id: ~a expected: ~s outcome: ~s\n" id expected outcome))
      (display (format "FAIL id: ~a expected: ~s outcome: ~s\n" id expected outcome))))

(define (test/constants)
  (define (test/constant/0)
    (let ((f (compile 0)))
      (should-be 'constant/0 0 (start f))))
  
  (define (test/constant/1)
    (let ((f (compile 1)))
      (should-be 'constant/1 1 (start f)))) 
  
  (define (test/constant/2)
    (let ((f (compile 2)))
      (should-be 'constant/2 2 (start f))))
  
  (define (test/constant/true)
    (let ((f (compile #t)))
      (should-be 'constant/true #t (start f))))
  
  (define (test/constant/false)
    (let ((f (compile #f)))
      (should-be 'constant/false #f (start f))))
  
  (define (test/constant/nil)
    (let ((f (compile null)))
      (should-be 'constant/nil '() (start f))))
  
  (define (test/constant/string)
    (let ((f (compile "foobar")))
      (should-be 'constant/string "foobar" (start f))))
  
  (define (test/constant/symbol)
    (let ((f (compile ''redondo)))
      (should-be 'constant/symbol 'redondo (start f))))

  (define (test/constant/list)
    (let ((f (compile ''(1 2 3))))
      (should-be 'constant/list '(1 2 3) (start f))))

  (define (test/constant/vector)
    (let ((f (compile #(1 2 3))))
      (should-be 'constant/vector #(1 2 3) (start f))))

  (test/constant/0)
  (test/constant/1)
  (test/constant/2)
  (test/constant/true)
  (test/constant/false)
  (test/constant/nil)
  (test/constant/string)
  (test/constant/symbol)
  (test/constant/list)
  (test/constant/vector))

;; The individual functions in the global baseline namespace.
(define (test/base)
  (define (test/base/cons)
    (let ((f (compile '((lambda () (cons 29 82))))))
      (should-be 'base/cons (cons 29 82) (start f))))
  
  (define (test/base/car)
    (let ((f (compile '((lambda ()(car (cons 29 82)))))))
      (should-be 'base/car 29 (start f))))
  
  (define (test/base/cdr)
    (let ((f (compile '((lambda () (cdr (cons 29 82)))))))
      (should-be 'base/cdr 82 (start f))))

  (define (test/base/null?)
    (let ((f (compile '(lambda () (null? '()))))
          (g (compile '((lambda () (null? '(1 2 3)))))))
      (should-be 'base/null?/1 #t (start f))
      (should-be 'base/null?/2 #f (start g))))
  
  (define (test/base/not)
    (let ((f (compile '(not #f)))
          (g (compile '(not #t))))
      (should-be 'base/not/1 #t (start f))
      (should-be 'base/not/2 #f (g rtk/RETURN RTE))))
  
  (define (test/base/<)
    (let ((f (compile '(< 8 31)))
          (g (compile '(< -1 -7))))
      (should-be 'base/</1 #t (start f))
      (should-be 'base/</2 #f (g rtk/RETURN RTE))))

  (define (test/base/=)
    (let ((f (compile '(= 8 8.0)))
          (g (compile '(= -1 -7))))
      (should-be 'base/=/1 #t (start f))
      (should-be 'base/=/2 #f (g rtk/RETURN RTE))))

  (define (test/base/add1)
    (let ((f (compile '(add1 93051))))
      (should-be 'base/add1 93052 (start f))))

  (define (test/base/sub1)
    (let ((f (compile '(sub1 93051))))
      (should-be 'base/sub1 93050 (start f))))
  
  (define (test/base/+)
    (let ((f (compile '(+ 93051)))
          (g (compile '(+ 812 23)))
          (h (compile '(+ 812 23 -10)))
          (i (compile '(+ 812 23 -10 17))))
      (should-be 'base/+/1 93051 (start f))
      (should-be 'base/+/2 835 (g rtk/RETURN RTE))
      (should-be 'base/+/3 825 (h rtk/RETURN RTE))
      (should-be 'base/+/4 842 (i rtk/RETURN RTE))))
      
  (define (test/base/*)
    (let ((f (compile '(* 93051)))
          (g (compile '(* 812 23)))
          (h (compile '(* 812 23 -10)))
          (i (compile '(* 812 23 -10 17))))
      (should-be 'base/*/1 93051 (start f))
      (should-be 'base/*/2 18676 (g rtk/RETURN RTE))
      (should-be 'base/*/3 -186760 (h rtk/RETURN RTE))
      (should-be 'base/*/4 -3174920 (i rtk/RETURN RTE))))
  
  (define (test/global/display)
    (let ((f (compile '(display "hello world\n"))))
      (start f)
      (should-be 'global/display "hello world" "hello world"))) ; Bogus but suggests what the tester should see.

  (define (test/global/format)
    (let ((f (compile '(format "~a ~a ~a ~s" 99 "hello" '(1 2 3) '|a weird name|))))
      (should-be 'global/format "99 hello (1 2 3) |a weird name|" (start f))))          
  
  (test/base/cons)
  (test/base/car)
  (test/base/cdr)
  (test/base/null?)
  (test/base/not)
  (test/base/<)
  (test/base/=)
  (test/base/add1)
  (test/base/sub1)
  (test/base/+)
  (test/base/*)
  (test/global/display)
  (test/global/format))

(define (test/lambda)
  (define (test/lambda/1)
    (let ((f (motile/compile '((lambda () 1))))) ; Trivial zero argument lambda expression.
      (should-be 'lambda/1 1 (motile/start f ENVIRON/TEST))))

  (define (test/lambda/2)
    (let ((f (motile/compile '((lambda (x) x) 17)))) ; The identity function.
      (should-be 'lambda/2 17 (motile/start f ENVIRON/TEST))))

  (define (test/lambda/3)
    (let ((f (motile/compile '((lambda (x) (1+ x)) 99)))) ; Single argument invoking a primitive in the global namespace.
      (should-be 'lambda/3 100 (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/4)
    (let ((f (motile/compile '((lambda (x y) (+ x y)) 99 33)))) ; Two arguments.
      (display 'lambda/4) (newline)
      (pretty-display (motile/decompile f))
      (should-be 'lambda/4 132 (motile/start f ENVIRON/TEST))))

  (define (test/lambda/5)
    (let ((f (motile/compile '((lambda (x y z) (+ x y z)) 1 2 3)))) ; Three arguments.
      (display 'lambda/4) (newline)
      (pretty-display (motile/decompile f))
      (should-be 'lambda/5 6 (motile/start f ENVIRON/TEST))))

  (define (test/lambda/6)
    (let ((f (motile/compile '((lambda (w x y z) (+ w x y z)) 1 2 3 4)))) ; Four arguments.
      (should-be 'lambda/6 10 (motile/start f ENVIRON/TEST))))

  (define (test/lambda/7)
    (let ((f (motile/compile '((lambda (v w x y z) (+ v w x y z)) 1 2 3 4 5)))) ; Five arguments.
      (should-be 'lambda/7 15 (motile/start f ENVIRON/TEST))))    
  
  (define (test/lambda/8)
    (let ((f (motile/compile
              '((lambda (x y z) ; In-order evaluation of a series of expressions in a lambda body.
                  (display (format "~a\n" x))
                  (display (format "~a\n" y))
                  (display (format "~a\n" z))
                  (+ x y z))
                10 20 30))))
      (should-be 'lambda/8 60 (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/9)
    (let ((f (motile/compile '((lambda rest rest) 3 17 22 87)))) ; Rest argument.
      (should-be 'lambda/9 '(3 17 22 87) (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/10a)
    (let ((f (motile/compile '((lambda (x . rest) (cons x rest)) 3 17 22 87)))) ; One positional argument + rest argument.
      (should-be 'lambda/10a '(3 17 22 87) (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/10b)
        (let ((f (motile/compile '((lambda (x y . rest) (cons (+ x y) rest)) 3 17 22 87)))) ; Two positional arguments + rest argument.
      (should-be 'lambda/10b '(20 22 87) (motile/start f ENVIRON/TEST))))

  (define (test/lambda/11a)
    (let ((f (motile/compile
              '((lambda (x y z . rest)  ; Three positional arguments + rest argument.
                  (cons (+ x y z) rest)) 3 17 22 87 127))))
      (should-be 'lambda/11a '(42 87 127) (motile/start f ENVIRON/TEST))))
  
    (define (test/lambda/11b)
    (let ((f (motile/compile
              '((lambda (a b c d . rest)  ; Three positional arguments + rest argument.
                  (cons (+ a b c d) rest)) 3 17 22 87 127 999))))
      (should-be 'lambda/11b '(129 127 999) (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/12a)
    (let ((f (motile/compile
              '(((lambda (a) (lambda () (add1 a))) 33)) ; One closed variable + zero parameters.
              )))
      (display 'lambda/12a) (newline)
      (pretty-display (motile/decompile f))
      (should-be 'lambda/12a 34 (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/12b)
    (let ((f (motile/compile
              '((lambda (a)
                  ((lambda (b) (+ a b)) 33)) ; One closed variable + one parameter.
                11))))
      (pretty-display (motile/decompile f))
      (should-be 'lambda/12b 44 (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/12c)
    (let ((f (motile/compile
              '((lambda (u v) ; Lexical scope.
                  ((lambda (w x) (+ u v x)) 300 400)) ; This lambda has two closed variables, u and v and two parameters, w and x.
                19 1))))
      (should-be 'lambda/12c 420 (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/13)
    (let ((f (motile/compile
              '((lambda (a) ; Deeply nested lexical scope.
                  ((lambda (b)
                     ((lambda (c)
                        ((lambda (d)
                           ((lambda (e)
                              ((lambda (f) (list a b c d e f))
                               6))
                            5))
                         4))
                      3))
                   2))
                1))))
      (should-be 'lambda/13 '(1 2 3 4 5 6) (motile/start f ENVIRON/TEST))))
           
  (define (test/lambda/14)
    (let ((f (motile/compile
              '((lambda (a b) ; Inner lexical scope shadows outer lexical scope.
                 ((lambda (b)
                    ((lambda (a) (+ a b)) 85))
                  22))
               5 10))))
      (should-be 'lambda/14 107 (motile/start f ENVIRON/TEST))))
  
  ; Combining rest arguments with closed variables.
  (define (test/lambda/15a)
    (let ((f (motile/compile '((lambda rest ((lambda (a) (cons a rest)) 33)) 'alpha 'beta 'gamma))))
      (should-be 'lambda/15 '(33 alpha beta gamma) (motile/start f ENVIRON/TEST))))
  
  (define (test/lambda/15b)
    (let ((f (motile/compile '((lambda (a b) ((lambda (x y . rest) (list* (+ a x) (+ b y) rest)) 100 200 300 400 500)) 11 13))))
      (display 'lambda/15b) (newline)
      (pretty-display (motile/decompile f)) (newline)
      (should-be 'lambda/15b '(111 213 300 400 500) (motile/start f ENVIRON/TEST))))

  (define (test/lambda/99)
    (let ((f (motile/compile '((lambda lambda lambda) 7 8 9 10)))) ; lambda must not be a reserved symbol.
      (should-be 'lambda/99 '(7 8 9 10) (motile/start f ENVIRON/TEST))))  
    
  (test/lambda/1)
  (test/lambda/2)
  (test/lambda/3)
  (test/lambda/4)
  (test/lambda/5)
  (test/lambda/6)
  (test/lambda/7)
  (test/lambda/8)
  (test/lambda/9)
  (test/lambda/10a)
  (test/lambda/10b)
  (test/lambda/11a)
  (test/lambda/11b)
  (test/lambda/12a)
  (test/lambda/12b)
  (test/lambda/12c)
  (test/lambda/13)
  (test/lambda/14)
  (test/lambda/15a)
  (test/lambda/15b)
  (test/lambda/99))

(define (test/decompile)
  ; Helper routine.
  ; Show the original source and then the decompilation.
  (define (show title source)
    (display title) (newline)
    (pretty-display source) (newline)
    (pretty-display (decompile (compile source))) (newline))
    
  (define (test/decompile/1)
    (let ((source '((lambda (x y z . rest)  ; Three positional arguments + rest argument.
                          (cons (+ x y z) rest)) 3 17 22 87 127)))
      (show 'decompile/1 source)))

  (define (test/decompile/2a)
    (let ((source '((lambda (a)
                      ((lambda (b) (+ a b)) 33))
                    11)))
      (show 'decompile/2a source)))

  (define (test/decompile/2b)
    (let* ((source '((lambda (a)
                       (lambda (b) (+ a b)))
                     11))
           (f (compile source))
           (inner (start f)))
      (display "decompile/2b (innner closure)\n")
      (pretty-display source)
      (newline)
      (pretty-display (decompile inner))
      (newline)))

  (define (test/decompile/2c)
    (let* ((source '((lambda (a)
                       (lambda (b) (list a b)))
                     (vector 'constant/generate 11)))
           (f (compile source))
           (inner (start f)))
      (display "decompile/2c (innner closure)\n")
      (pretty-display source)
      (newline)
      (pretty-display (decompile inner))
      (newline)))             
             
  (define (test/decompile/3a)
    (let* ((source '((lambda (u v) (lambda (w x) (+ u v x))) 19 1))
           (f (compile source))
           (inner (start f)))
      (display "decompile/3a (innner closure)\n")
      (pretty-display source)
      (newline)
      (pretty-display (decompile inner))
      (newline)))

  (define (test/decompile/3b)
    (let* ((source '((lambda (u v) (lambda (w x) (+ u v x) (* u v w))) 19 1))
           (f (compile source))
           (inner (start f)))
      (display "decompile/3b (innner closure)\n")
      (pretty-display source)
      (newline)
      (pretty-display (decompile inner))
      (newline))) 
  
  (define (test/decompile/4a)
    (let ((source
           '((lambda (a) ; Deeply nested lexical scope.
               ((lambda (b)
                  ((lambda (c)
                     ((lambda (d)
                        ((lambda (e)
                           ((lambda (f) (list a b c d e f))
                            6))
                         5))
                      4))
                   3))
                2))
             1)))
      (show 'decompile/4a source)))
  
  (test/decompile/1)
  (test/decompile/2a)
  (test/decompile/2b)
  (test/decompile/2c)
  (test/decompile/3a)
  (test/decompile/3b)
  (test/decompile/4a)
  )

(define (test/let)
  (define (test/let/1)
    (let ((e (motile/compile '(let ((a 1) (b 2) (c 3)) (list a b c)))))
      (should-be 'let/1 '(1 2 3) (motile/start e ENVIRON/TEST))))
  
  ; The lexical scope established by a let is seen only by the expressions in the body of the let.
  ; Here the cons in the body of the lambda should resolve to cons in the global namespace and
  ; the cons in the let body should resolve to the lambda expression.
  (define (test/let/2)
    (let ((e (motile/compile '(let ((cons (lambda (x y)
                                     (cons (cons x '()) (cons y '()))))) ; Rebinding of cons in body of let.
                         (cons 'foo 'bar))))) ; Body of let
      (should-be 'let/2 '((foo) bar) (motile/start e ENVIRON/TEST))))
  
  (define (test/let/3)
    (let ((e (motile/compile '(let ((a 1) (b 2) (c 3) (d "silly") (e #(8 9 10)) (f (list 'x 'y)))
                         (list a b c d e f)))))
      (should-be 'let/3 '(1 2 3 "silly" #(8 9 10) (x y)) (motile/start e ENVIRON/TEST))))
  
  (define (test/let/4)
    (let ((e (motile/compile (let () (display "alpha\n") (display "beta\n") "gamma")))) ; Let with no definitions.
      (should-be 'let/4 "gamma" (motile/start e ENVIRON/TEST))))
  
  (test/let/1)
  (test/let/2)
  (test/let/3)
  (test/let/4))

(define (test/foo)
  (let ((e (compile '(let*
                         ((a 11)
                          (b (lambda ()
                               (display "INSIDE b\n\n")
                               (display (format "\nRTE inside b ~a\n\n" (rte/reveal)))
                               (+ a 13))))
                       (display (format "\nRTE before b ~a\n\n" (rte/reveal)))
                       (b)))))
    (start e)))


;; setter is used internally to implement letrec. It is not available as a Mischief primitive and is not correct
;; with respect to Mischief closure construction (the side effect of setting a variable in an
;; outer scope A from within an inner scope B will NOT be seen within the inner scope B). The implementation
;; is sufficient to implement letrec and NOTHING MORE!
(define (test/setter)
  (define (test/setter/1)
    (let ((e (compile `(let ((a 1))
                         (,(setter/tag) a 2)
                         a))))
      (pretty-display (decompile e))
      (should-be 'setter/1 2 (start e))))

  (define (test/setter/2)
    (let ((e (compile `(let ((a 1))
                         (list
                          (let ((b 17))
                            (,(setter/tag) a 12) ; This side effect is invisible to this inner scope.
                            (,(setter/tag) b 15)
                            b)
                          a))))) ; The reset of the value of a is seen here.

      (pretty-display (decompile e))
      (should-be 'test/setter/2 '(15 12) (start e))))

  (test/setter/1)
  (test/setter/2)
  )

(define (test/letrec)
  (define (test/letrec/1)
    (let ((e (motile/compile '(letrec
                           ((a 11)
                            (b (lambda () (+ a 13))))
                         (b)))))
      (should-be 'letrec/1 24 (motile/start e ENVIRON/TEST))))

  (define (test/letrec/2a)
    (let* ((source '(letrec
                           ((f (lambda (n)
                                 (if (= n 1) 1 (* n (f (sub1 n))))))) ; Factorial.
                         (f 5)))
           (e (motile/compile source)))
      (should-be 'letrec/2a 120 (motile/start e ENVIRON/TEST))))
  
  (define (test/letrec/2b)
    (let ((e (motile/compile
              '(letrec
                   ((f (lambda (n)
                         (if (= n 1) 1 (* n (f (sub1 n))))))) ; Factorial.
                 f))))
      (display "letrec/2b\n")
      (pretty-display (motile/decompile e))))
  
  (define (test/letrec/2c)
    (let ((e (motile/compile
              '(letrec
                   ((f (lambda (n)
                         (if (= n 1) 1 (* n (f (sub1 n))))))) ; Factorial.
                 f))))
      (display "letrec/2c\n")
      (motile/decompile (motile/start e ENVIRON/TEST))))
  
  ;; Recursive definition of two functions.
  (define (test/letrec/3a)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1))))))
                 (list
                  (even? 12) (even? 3)
                  (odd? 17)  (odd? 8))))))

      (should-be 'letrec/3a '(#t #f #t #f) (motile/start e ENVIRON/TEST))))
  
  ;; Recursive definition of two functions.
  (define (test/letrec/3b)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1))))))
                 (list
                  (even? 12) (even? 3)
                  (odd? 17)  (odd? 8))))))

      (display "letrec/3b\n")
      (pretty-display (motile/decompile e))))
  
  ;; Test of decompilation combining recursive function definitions with multiple (> 1) closed variables per function.
  (define (test/letrec/3c)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n zero) #t (odd? (- n one)))))
                    (odd?
                     (lambda (n)
                       (if (= n zero) #f (even? (- n one)))))
                    (zero 0)
                    (one  1))
                 (list
                  (even? 12) (even? 3)
                  (odd? 17)  (odd? 8))))))

      (display "letrec/3c\n")
      (pretty-display (motile/decompile e))))


  ;; Recursive definition of three functions.
  (define (test/letrec/4)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1)))))
                    (factorial
                     (lambda (n)
                       (if (= n 1) 1 (* n (factorial (sub1 n)))))))

                 (list
                  (even? 12) (even? 3)
                  (odd? 17)  (odd? 8)
                  (factorial 5))))))

      (should-be 'letrec/4 '(#t #f #t #f 120) (motile/start e ENVIRON/TEST))))


  ;; Recursive definition of five bindings.
  (define (test/letrec/5)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1)))))
                    (factorial
                     (lambda (n)
                       (if (= n 1) 1 (* n (factorial (sub1 n))))))
                    (a 11)
                    (b (lambda () (+ a 13))))

                 (list
                  (even? 12) (even? 3)
                  (odd?  a)  (odd? 8)
                  (factorial 5)
                  (b))))))

      (should-be 'letrec/5 '(#t #f #t #f 120 24) (motile/start e ENVIRON/TEST))))
  
  (define (test/letrec/6)
    (let ((e (motile/compile
              '(letrec ((foo (lambda (x) (eq? x foo))))
                 (foo foo)))))
      (should-be 'letrec/6 #t (motile/start e ENVIRON/TEST))))
  
  ; Letrec with (define ...) in the body.
  (define (test/letrec/7)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1)))))
                    (factorial
                     (lambda (n)
                       (if (= n 1) 1 (* n (factorial (sub1 n)))))))

                 ; Definitions in the letrec body.
                 (define a 11)
                 (define b (lambda () (+ a 13)))
                 (list
                  (even? 12) (even? 3)
                  (odd?  a)  (odd? 8)
                  (factorial 5)
                  (b))))))
      (should-be 'letrec/7 '(#t #f #t #f 120 24) (motile/start e ENVIRON/TEST))))

  (define (test/letrec/tak) ; The (in)famous Takeuchi function.
    (let ((e (motile/compile
              '(letrec
                   ((tak
                     (lambda (x y z)
                       (if (not (< y x))
                           z
                           (tak (tak (- x 1) y z)
                                (tak (- y 1) z x)
                                (tak (- z 1) x y))))))
                 (tak 18 12 6)))))
      (should-be 'letrec/tak 7 (motile/start e ENVIRON/TEST))))
  
  (test/letrec/1)
  (test/letrec/2a)
  (test/letrec/2b)
  (test/letrec/2c)
  (test/letrec/3a)
  (test/letrec/3b)
  (test/letrec/3c)
  (test/letrec/4)
  (test/letrec/5)
  (test/letrec/6)
  (test/letrec/7)
  (test/letrec/tak))

(define (test/closure/inner)
  ;; Definition of two mutually recursive functions.
  (define (closure/inner/1)
    (let ((e (motile/compile
           '(letrec
                ((fact (lambda (n) (if (= n 1) 1 (* n (fact (sub1 n)))))))
              fact))))
      (display "closure/inner/1\n")
      (display "decompile of factorial\n")
      (pretty-display (motile/decompile (motile/start e ENVIRON/TEST)))))
  
  (define (closure/inner/2)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1))))))
                 even?))))

      (display "closure/inner/2\n")
      (display "decompile of complete letrec\n")
      (pretty-display (motile/decompile e))))
  
    (define (closure/inner/3)
    (let ((e (motile/compile
              '(letrec ; Mutually recursive functions.
                   ((even?
                     (lambda (n)
                       (if (= n 0) #t (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= n 0) #f (even? (- n 1))))))
                 (list even? odd?)))))

      (display "closure/inner/3\n")
      (display "decompile of even? and odd\n")
      (let* ((x (motile/start e ENVIRON/TEST))
             (even? (list-ref x 0))
             (odd?  (list-ref x 1)))
        (pretty-display (motile/decompile even?))
        (pretty-display (motile/decompile odd?)))))
  
  (closure/inner/1)
  (closure/inner/2)
  (closure/inner/3))

(define (test/let*)
  (define (test/let*/1)
    (let ((e (motile/compile
              '(let* ((a 1)
                      (b (+ a 13))
                      (c (+ a b 20)))
                 (+ a b c)))))
      (should-be 'let*/1 50 (motile/start e ENVIRON/TEST))))
  
    (define (test/let*/2)
    (let ((e (motile/compile
              '(let* ((a 1)
                      (b (+ a 13))
                      (c (lambda () (+ a b 20))))
                 (c)))))
      (should-be 'let*/2 35 (motile/start e ENVIRON/TEST))))
  
  (test/let*/1)
  (test/let*/2))

(define (test/named-let)
  (define (test/named-let/1)
    (let ((e (motile/compile
              '(let loop ((numbers '(3 -2 1 6 -5))
                          (nonnegative '())
                          (negative '()))
                 (cond ((null? numbers)
                        (list nonnegative negative))
                       ((>= (car numbers) 0)
                        (loop (cdr numbers)
                              (cons (car numbers) nonnegative)
                              negative))
                       ((< (car numbers) 0)
                        (loop (cdr numbers)
                              nonnegative
                              (cons (car numbers) negative))))))))
      (should-be 'named-let/1 '((6 1 3) (-5 -2)) (motile/start e ENVIRON/TEST))))
  
  (test/named-let/1))

(define (test/define)
  (define (test/define/1) ; A lambda may contain one or more definitions.
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a+) (add1 a))
                  (define (b-) (sub1 b))
                  (* (a+) (b-)))
                8 -1))))
      (should-be 'define/1 -18 (motile/start f ENVIRON/TEST))))
  
  (define (test/define/2a) ; A definition may contain nested definitions.
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a+) ; Define within lambda body.
                    (define (b-) (sub1 b)) ; Nested definition within definition of a+
                    (* (add1 a) (b-))) ; Body of a+
                  (a+))))))
      (display "\ndefine/2a\n")
      (pretty-display (motile/decompile f))))  
  

   (define (test/define/2b) ; A definition may contain nested definitions.
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a+) ; Define within lambda body.
                    (define (f) b) ; Nested definition within definition of a+
                    (+ a (f))) ; Body of a+
                  (a+))))))
      (display "\ndefine/2b\n")
      (pretty-display (motile/decompile f))))
  
  (define (test/define/2c) ; A definition may contain nested definitions.
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a*) ; Define within lambda body.
                    (define (b-) (sub1 b)) ; Nested definition within definition of a*
                    (* a (b-))) ; Body of a*
                  (a*))
                8 -1))))
      (display "\ndefine/2c\n")
      (pretty-display (motile/decompile f))
      (should-be 'define/2c -16 (motile/start f ENVIRON/TEST))))
  
  (define (test/define/3) ; A definition may be recursive.
    (let ((f (motile/compile
              '((lambda (a)
                  (define (fib n)
                    (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
                  (fib a))
                13))))
      (should-be 'define/3 233 (motile/start f ENVIRON/TEST))))
  
  (define (test/define/4) ; A let may contain one or more definitions.
    (let ((f (motile/compile
              '(let ((a 8) (b -1))
                  (define (a+) (add1 a))
                  (define (b-) (sub1 b))
                  (* (a+) (b-))))))
      (should-be 'define/4 -18 (motile/start f ENVIRON/TEST))))
  
  (define (test/define/5) ; A let* may contain one or more definitions.
    (let ((f (motile/compile
              '(let* ((a 8) (b (+ a 4)))
                 (define (fib n)
                   (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
                 (list a (fib a) b (fib b))))))
      (should-be 'define/5 '(8 21 12 144) (motile/start f ENVIRON/TEST))))
  
  (define (test/define/6) ; A letrec may contain one or more definitions.
    (let ((f (motile/compile
              '(letrec ((a 8) (b (lambda () (+ a 4))))
                 (define (fib n)
                   (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
                 (list a (fib a) (b) (fib (b)))))))
      (should-be 'define/6 '(8 21 12 144) (motile/start f ENVIRON/TEST))))
  
  (define (test/define/7) ; A letrec may contain one or more definitions.
    (let ((f (motile/compile
              '(letrec ((a 8) (b -1))
                  (define (a+) (add1 a))
                  (define (b-) (sub1 b))
                  (* (a+) (b-))))))
      (should-be 'define/7 -18 (motile/start f ENVIRON/TEST))))  
  
  (test/define/1)
  (test/define/2a)
  (test/define/2b)
  (test/define/2c)
  (test/define/3)
  (test/define/4)
  (test/define/5)
  (test/define/6)
  (test/define/7)
  )

(define (test/begin)
  (define (test/begin/1)
    (let ((f (compile
              '(begin 88))))
      (should-be 'begin/1 88 (start f))))
  
  (define (test/begin/2)
    (let ((f (compile
              '(begin
                 (display "begin/2 first\n")
                 (display "begin/2 second\n")
                 (display "begin/2 third\n")
                 99))))
      (should-be 'begin/2 99 (start f))))
  
  (test/begin/1)
  (test/begin/2)
  )

(define (test/when)
  (define (test/when/1)
    (let ((e (compile '(when #t 'hello))))
      (should-be 'when/1 'hello (start e))))
  
  (define (test/when/2)
    (let ((e (compile '(when #f 'wrong))))
      (should-be 'when/2 (void) (start e))))
  
  (define (test/when/3)
    (let ((e (compile
              '((lambda (n)
                  (when (> n 3) (display "when/3 first\n") (display "when/3 second\n") n))
                17))))
      (should-be 'when/3 17 (start e))))
  
  (test/when/1)
  (test/when/2)
  (test/when/3))

(define (test/unless)
  (define (test/unless/1)
    (let ((e (compile '(unless #f 'hello))))
      (should-be 'unless/1 'hello (start e))))
  
  (define (test/unless/2)
    (let ((e (compile '(unless #t 'wrong))))
      (should-be 'unless/2 (void) (start e))))
  
  (define (test/unless/3)
    (let ((e (compile
              '((lambda (n)
                  (unless (> n 3) (display "unless/3 first\n") (display "unless/3 second\n") n))
                2))))
      (should-be 'unless/3 2 (start e))))
  
  (test/unless/1)
  (test/unless/2)
  (test/unless/3))

(define (test/cond)
  (define (test/cond/1)
    (let* ((source 
            '(lambda (a b)
               (define (fib n)
                 (if (= n 0) 0 (if (= n 1) 1 (+ (fib (sub1 n)) (fib (- n 2))))))
               (cond
                 ((> a 9) (fib a))
                 ((> (+ a b) 11) (fib (+ a b)))
                 (else (+ (fib a) (fib b))))))
           (a (compile `(,source 10 0))) ; Exercise first clause of cond.
           (b (compile `(,source 8 5)))  ; Exercise second clause of cond.
           (c (compile `(,source 5 6)))) ; Exercise third clause of cond.

      (should-be 'cond/1a 55  (start a))
      (should-be 'cond/1b 233 (start b))
      (should-be 'cond/1c 13  (start c))))
           
  (define (test/cond/2) ; Exercises (test => procedure) clause.
    (let* ((source 
            '(lambda (a b)
               (define (fib n)
                 (cond
                   ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (fib (sub1 n)) (fib (- n 2))))))
               (cond
                 ((> a 9) (fib a))
                 ((> (+ a b) 11) (fib (+ a b)))
                 ((+ a b) => (lambda (n)  (* 2 (fib n)))))))
           (a (compile `(,source 10 0))) ; Exercise first clause of cond.
           (b (compile `(,source 8 5)))  ; Exercise second clause of cond.
           (c (compile `(,source 5 6)))) ; Exercise third clause of cond.

      (should-be 'cond/2a 55  (start a))
      (should-be 'cond/2b 233 (start b))
      (should-be 'cond/2c 178 (start c))))  

  (define (test/cond/3) ; Exercises the trivial (test) clause.
    (let* ((source 
            '(lambda (a b)
               (define (fib n)
                 (cond
                   ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (fib (sub1 n)) (fib (- n 2))))))
               (cond
                 ((> a 9) (fib a))
                 ((> (+ a b) 11) (fib (+ a b)))
                 ((+ a b)))))
           (a (compile `(,source 10 0))) ; Exercise first clause of cond.
           (b (compile `(,source 8 5)))  ; Exercise second clause of cond.
           (c (compile `(,source 5 6)))) ; Exercise third clause of cond.

      (should-be 'cond/3a 55  (start a))
      (should-be 'cond/3b 233 (start b))
      (should-be 'cond/3c 11  (start c))))

  (define (test/cond/4) ; Exercises (test) clause.
    (let* ((source 
            '(lambda (a b)
               (define (fib n)
                 (cond
                   ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (fib (sub1 n)) (fib (- n 2))))))
               (cond
                 ((> a 9) (fib a))
                 ((+ a b)) ; If the prior clause fails then this clause should ALWAYS execute.
                 ((> (+ a b) 11) (fib (+ a b))))))
           (a (compile `(,source 10 0))) ; Exercise first clause of cond.
           (b (compile `(,source 8 5)))  ; Exercise second clause of cond.
           (c (compile `(,source 5 6)))) ; Exercise third clause of cond.

      (should-be 'cond/4a 55  (start a))
      (should-be 'cond/4b 13 (start b))
      (should-be 'cond/4c 11  (start c))))

  (test/cond/1)
  (test/cond/2)
  (test/cond/3)
  (test/cond/4))

(define (test/case)
  (define (test/case/1)
    (let ((e (compile
              '(case (* 2 3)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8 9) 'composite)))))
      (should-be 'case/1 'composite (start e))))
  
  (define (test/case/2)
    (let ((e (compile
              '(case (car '(c d))
                 ((a) 'a)
                 ((b) 'b)))))
      (should-be 'case/2 (void) (start e))))
  
  (define (test/case/3)
    (let ((e (compile
              '(case (car '(c d))
                 ((a e i o u) 'vowel)
                 ((w y) 'semivowel)
                 (else 'consonant)))))
      (should-be 'case/3 'consonant (start e))))

  (test/case/1)
  (test/case/2)
  (test/case/3))
                
(define (test/do)
  (define (test/do/1)
    (let* ((source
           '(do ((vec (make-vector 5))
                 (i 0 (+ i 1)))
              ((= i 5) vec)
              (vector-set! vec i i)))
           (e (compile source)))
      (display "do/1\n")
      (pretty-display source)
      (do/translate source)
      (should-be 'do/1 #(0 1 2 3 4) (start e))))
  
  (define (test/do/2)
    (let* ((source
            '(do ((vec (make-vector 5))
                  (i 0 (+ i 1)))
               ((= i 5)) ; No expressions following <test>.
               (vector-set! vec i i)))
           (e (compile source)))
      (display "do/2\n")
      (pretty-display source)
      (pretty-display (do/translate source))
      (should-be 'do/2 (void) (start e)))) 

  (define (test/do/3)
    (let* ((source
            '(let ((x '(1 3 5 7 9)))
               (do ((x x (cdr x))
                    (sum 0 (+ sum (car x))))
                 ((null? x) sum)))) ; No commands in do body.
           (e (compile source)))
      (display "do/3\n")
      (pretty-display source)
      (should-be 'do/3 25 (start e))))
  
  (test/do/1)
  (test/do/2)
  (test/do/3))

(define (test/and)
  (define (test/and/1)
    (let ((e (compile '(and))))
      (should-be 'test/and/1 #t (start e))))
  
  (define (test/and/2)
    (let ((e (compile '(if (and) "good" "bad"))))
      (should-be 'test/and/2 "good" (start e))))
  
  (define (test/and/3)
    (let ((e (compile '((lambda (a b) (and (< a b)))
                        3 5))))
      (should-be 'test/and/3 #t (start e))))
  
  (define (test/and/4)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (and (element 1 3 5) (element 2 5 3) (element 3 7 8)))))) ; Only elements 1 and 2 should execute.
      (should-be 'test/and/4 #f (start e))))

  
  (define (test/and/5)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (and (element 1 3 5) (element 2 2 3) (element 3 11 17)))))) ; All elements, 1 thru 3, should execute.
      (should-be 'test/and/5 #t (start e))))

  (define (test/and/6)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (define (happy flag)
                   (if flag "happy" "unhappy"))
                 
                 (happy (and (element 1 3 5) (element 2 5 3) (element 3 7 8)))))))
      (should-be 'test/and/6 "unhappy" (start e))))  
  
  (test/and/1)
  (test/and/2)
  (test/and/3)
  (test/and/4)
  (test/and/5)
  (test/and/6))

(define (test/or)
  (define (test/or/1)
    (let ((e (compile '(or))))
      (should-be 'test/or/1 #f (start e))))
  
  (define (test/or/2)
    (let ((e (compile '(if (or) "bad" "good"))))
      (should-be 'test/or/2 "good" (start e))))
  
  (define (test/or/3)
    (let ((e (compile '((lambda (a b) (or (< a b)))
                        3 5))))
      (should-be 'test/or/3 #t (start e))))
  
  (define (test/or/4)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (or (element 1 5 3) (element 2 3 5) (element 3 7 8)))))) ; Only elements 1 and 2 should execute.
      (should-be 'test/or/4 #t (start e))))

  
  (define (test/or/5)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (or (element 1 5 3) (element 2 3 2) (element 3 11 17)))))) ; All elements, 1 thru 3, should execute.
      (should-be 'test/or/5 #t (start e))))

  (define (test/or/6)
    (let ((e (compile
              '(let ()
                 (define (element n a b)
                   (display (format "element: ~a\n" n))
                   (< a b))
                 (define (happy flag)
                   (if flag "happy" "unhappy"))
                 
                 (happy (or (element 1 5 3) (element 2 3 2) (element 3 7 8)))))))
      (should-be 'test/or/6 "happy" (start e))))  
  
  (test/or/1)
  (test/or/2)
  (test/or/3)
  (test/or/4)
  (test/or/5)
  (test/or/6))

(define (test/sort)
  (let ((e
         (compile
          '(let ()
             (define (sort-list obj pred)
               
               (define (loop l)
                 (if (and (pair? l) (pair? (cdr l)))
                     (split l '() '())
                     l))
               
               (define (split l one two)
                 (if (pair? l)
                     (split (cdr l) two (cons (car l) one))
                     (merge (loop one) (loop two))))
               
               (define (merge one two)
                 (cond
                   ((null? one) two)
                   ((pred (car two) (car one))
                    (cons (car two)
                          (merge (cdr two) one)))
                   (else
                    (cons (car one)
                          (merge (cdr one) two)))))
               
               (loop obj))
             
             (sort-list '("one" "two" "three" "four" "five" "six"
                                "seven" "eight" "nine" "ten" "eleven" "twelve")
                        string<?)))
         ))
    (should-be 'test/sort
               '("eight" "eleven" "five" "four" "nine" "one" "seven" "six" "ten" "three" "twelve" "two")
               (start e))))


(define (test/quasiquotation)
  (define (test/quasiquotation/1)
    (let ((e (compile '`(1 (+ 2 3) 17))))
      (should-be 'quasiquotation/1 '(1 (+ 2 3) 17) (start e))))

  (define (test/quasiquotation/2)
    (let ((e (compile '`(1 ,(+ 2 3) 17))))
      (should-be 'quasiquotation/2 '(1 5 17) (start e))))

  (define (test/quasiquotation/3)
    (let ((e (compile '`(1 ,@'(a b c) 17))))
      (should-be 'quasiquotation/3 '(1 a b c 17) (start e))))
  
  (define (test/quasiquotation/4)
    (let ((e (compile 
              '(let ((a 99)) `(1 ,a 17)))))
      (should-be 'quasiquotation/4 '(1 99 17) (start e))))

   (define (test/quasiquotation/5)
    (let ((e
           (compile '(let ((a 13)
                           (b 19))
                       `(front ,(+ a b) ,(list a b (* 2 a) (* 2 b)) rear)))))
      (should-be 'quasiquotation/5 '(front 32 (13 19 26 38) rear) (start e)))) 

  (define (test/quasiquotation/6)
    (let ((e
           (compile '(let ((a 13)
                           (b 19))
                       `(front ,(+ a b) ,@(list a b (* 2 a) (* 2 b)) rear)))))
      (should-be 'quasiquotation/6 '(front 32 13 19 26 38 rear) (start e))))

  (define (test/quasiquotation/7)
    (let ((e (compile '`(1 `,(+ 1 ,(+ 2 3)) 4))))
      (should-be 'quasiquotation/7 '(1 `,(+ 1 5)  4) (start e))))

  (define (test/quasiquotation/8)
    (let ((e (compile '`(1 ```,,@,,@(list (+ 1 2)) 4))))
      (should-be 'quasiquotation/8 '(1 ```,,@,3    4) (start e))))
  
  (test/quasiquotation/1)
  (test/quasiquotation/2)
  (test/quasiquotation/3)
  (test/quasiquotation/4)
  (test/quasiquotation/5)
  (test/quasiquotation/6)
  (test/quasiquotation/7)
  (test/quasiquotation/8))

(define (test/macro)
  (define (test/macro/1)
    (let ((e (compile
              '(let ()
                 (define-macro (eleven) 11)
                 (eleven)))))
      (pretty-display (decompile e))
      (should-be 'macro/1 11 (start e))))

  ; Repeated application of a single macro.
  (define (test/macro/2)
    (let ((e (compile
              '(let ()
                 (define-macro (eleven) 11)
                 (+ (eleven) (eleven))))))
      (pretty-display (decompile e))
      (should-be 'macro/2 22 (start e))))

  ; Repeated nested application of macros.
  (define (test/macro/3)
    (let ((e (compile
              '(let ()
                 (define-macro (eleven) '(ten+1))
                 (define-macro (ten+1)  '(add1 10))
                 (+ (eleven) (eleven))))))
      (pretty-display (decompile e))
      (should-be 'macro/3 22 (start e))))

  ; Lexical scoping of macros.
  (define (test/macro/4)
    (let ((e (compile
              '(let ()
                 (define-macro (eleven) '(ten+1))
                 (define-macro (ten+1)  '(add1 10))
                 (let ()
                   (define-macro (ten+1) '(+ 10 2))
                   (+ (eleven) (eleven)))))))
      (pretty-display (decompile e))
      (should-be 'macro/4 24 (start e))))
  
  ; Macro contains a local function.
  (define (test/macro/5)
    (let ((e (compile
              '(let ()
                 (define-macro (<let> bindings . body)
                   ; Local function inside macro definition body.
                   (define (unzip bindings variables values)
                     (if (null? bindings)
                         (cons variables values)
                         (let ((binding (car bindings)))
                           (unzip (cdr bindings)
                                  (cons (car binding) variables)
                                  (cons (cadr binding) values)))))

                   (let* ((unzipping (unzip bindings null null))
                          (variables (car unzipping))
                          (values    (cdr unzipping)))
                     `((lambda ,variables ,@body) ,@values)))
                 
                 (<let> ((a 3) (b 7)) (* a b)))))) ; This should be macro translated.
                 
      (pretty-display (decompile e))
      (should-be 'macro/5 21 (start e))))

  (test/macro/1)
  (test/macro/2)
  (test/macro/3)
  (test/macro/4)
  (test/macro/5))

(define (test/call/cc)
  (define (test/call/cc/1)
    (let ((e (compile
              '(call/cc
                (lambda (k)
                  (* 5 4))))))
      (should-be 'call/cc/1 20 (start e))))
  
  (define (test/call/cc/2)
    (let ((e (compile
              '(call/cc
                (lambda (k)
                  (* 5 (k 4)))))))
      (should-be 'call/cc/2 4 (start e))))

  (define (test/call/cc/3)
    (let ((e (compile
              '(+ 2
                  (call/cc
                   (lambda (k)
                     (* 5 (k 4))))))))
      (should-be 'call/cc/3 6 (start e))))

  (define (test/call/cc/4)
    (let ((e (compile
              '(let ((product
                      (lambda (ls)
                        (call/cc
                         (lambda (break)
                           (let f ([ls ls])
                             (cond
                               [(null? ls) 1]
                               [(= (car ls) 0) (break 999)]
                               [else (* (car ls) (f (cdr ls)))])))))))
                 (list
                  (product '(1 2 3 4 5))
                  (product '(7 3 8 0 1 9 5)))))))
      (should-be 'call/cc/4 '(120 999) (start e))))

  (define (test/call/cc/5)
    (let ((e (compile
              '(let ([x (call/cc (lambda (k) k))])
                 (x (lambda (ignore) "hi"))))))
      (should-be 'call/cc/5 "hi" (start e))))
    

(define (test/call/cc/6a)
  (let ((e (compile
            '(let ((saved (box #f))
                   (f (lambda (a b c) (+ a b c)))) ; Trivial function.

               (display
                (format "\tcall/cc/6a: ~a\n"
                        (f 3 
                           (call/cc (lambda (k) (set-box! saved k) 7)) ; Continuation capture in argument of f.
                           11)))

               (let ((k (unbox saved)))
                 (when k
                   (set-box! saved #f)
                   (k 11)))))))
    (display "call/cc/6a: output should be 21 25\n")
    (start e)))


  (define (test/call/cc/6b)
    (let ((e (compile
              '(let ((saved (box #f)))
                 (let ()
                   (define (factorial n)
                     (if (= n 0)
                         (call/cc
                          (lambda (k) (set-box! saved k) 1))
                         (* n (factorial (sub1 n)))))

                   (display (format "\tcall/cc/6b: ~a\n" (factorial 4))))
                 
                 (let ((k (unbox saved)))
                   (when k
                     (set-box! saved #f)
                     (k 2)))))))

      (display "call/cc/6b: output should be 24 48\n")
      (start e)))

  (define (test/call/cc/7)
    (let ((e (compile
              '(let ((saved (box #f))
                     (boxes (vector #f #f)))
                 (letrec ((x (box (call/cc (lambda (c) (set-box! saved c) 0))))
                          (y (box (call/cc (lambda (c) (set-box! saved c) 0)))))
                   (if (unbox saved)
                       (let ((k (unbox saved)))
                         (set-box! saved #f)
                         (vector-set! boxes 0 x)
                         (vector-set! boxes 1 y)
                         (set-box! x 1)
                         (set-box! y 1)
                         (k 0))
                       (begin
                         (display (format "x: ~a y: ~a\n"
                                          (eq? x (vector-ref boxes 0))
                                          (eq? y (vector-ref boxes 1))))
                         (+ (unbox x) (unbox y)))))))))
      (pretty-display (decompile e))
      (should-be 'call/cc/7 1 (start e))))
  
  
  ;; This test is the brainchild of Al Petrofsky (May 20, 2001 in comp.lang.scheme).
  ;; In an implementation which evaluates the letrec initializers prior to the assignments
  ;; (as required by R5RS) it returns #t. In letrec implementations in which initializer evaluation
  ;; is intermingled with assignment it returns #f.
  (define (test/call/cc/8)
    (let ((e (compile
              '(letrec ((x (call/cc list)) (y (call/cc list)))
                 (cond ((procedure? x) (x (pair? y)))
                       ((procedure? y) (y (pair? x))))
                 (let ((x (car x)) (y (car y)))
                   (and (call/cc x) (call/cc y) (call/cc x)))))))
      (should-be 'call/cc/8 #t (start e))))

  (test/call/cc/1)
  (test/call/cc/2)
  (test/call/cc/3)
  (test/call/cc/4)
  (test/call/cc/5)
  (test/call/cc/6a)
  (test/call/cc/6b)
  (test/call/cc/7)
  (test/call/cc/8))

(define (test/higher)
  (define (test/higher/map)
    (let ((e (compile
              '(let () (map (lambda (x) (* 3 x)) '(0 1 2 3 4 5))))))
      (should-be 'higher/map/1 '(0 3 6 9 12 15) (start e))))

  (define (test/higher/apply/1)
    (let ((e (compile
              '(let () (apply + 1 2 3 4 5)))))
      (should-be 'higher/apply/1 15 (start e))))

  (define (test/higher/apply/2)
    (let ((e (compile
              '(let () (apply (lambda (x y) (+ x y 13)) 4 5)))))
      (should-be 'higher/apply/2 22 (start e))))

  (define (test/higher/for-each)
    (let ((e (compile
              '(let () (for-each (lambda (x) (display (add1 x)) (display "\n")) '(0 1 2 3 4))))))
      (start e)))

  (test/higher/map)
  (test/higher/apply/1)
  (test/higher/apply/2)
  (test/higher/for-each))
        
(define (test/scope)
  (let ((e (compile
            '(let ((f cons)) (f 19 22)))))
    (pretty-display (mischief/decompile e))))

(define (test/all)
  (test/constants)
  (test/base)
  (test/lambda)
  (test/let)
  ;(test/setter)
  (test/letrec)
  (test/let*)
  (test/named-let)
  (test/define)
  (test/begin)
  (test/when)
  (test/unless)
  (test/cond)
  (test/case)
  (test/do)
  (test/and)
  (test/or)
  (test/sort)
  (test/quasiquotation)
  (test/macro))

;; A few tests for the integration of persistent vectors into Mischief.
(define (test/vector)
  (define (test/list/vector)
    (let ((e (compile
              '(let ((v (list/vector vector/null '(2 4 6 8 10))))
                 (vector/list v)))))
      (should-be 'list/vector '(2 4 6 8 10) (mischief/start e))))

  (define (test/vector/build)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
                 (vector/list v)))))
      (should-be 'vector/build '(1 2 3 4 5 6 7) (mischief/start e))))

  (define (test/vector/fold/left)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
                 (vector/fold/left v (lambda (x seed) (cons x seed)) null)))))
      (should-be 'vector/fold/left '(7 6 5 4 3 2 1) (mischief/start e))))

  (define (test/vector/fold/right)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
                 (vector/fold/right v (lambda (x seed) (cons x seed)) null)))))
      (should-be 'vector/fold/right '(1 2 3 4 5 6 7) (mischief/start e))))    

  (define (test/vepersist?)
    (let ((e (compile
              '(let ((v (list/vector vector/null '(2 4 6 8))))
                 (list (vepersist? v) (vepersist? (vector/list v)))))))
      (should-be 'vector/is? '(#t #f) (mischief/start e))))

  (define (test/vector/length)
    (let ((e (compile
              '(let* ((a (list/vector vector/null '(1 2 3)))
                      (b (list/vector a           '(4 5)))
                      (c (list/vector b           '(6 7 8 9))))
                 (list (vector/length vector/null) (vector/length a) (vector/length b) (vector/length c))))))
      (should-be 'vector/length '(0 3 5 9) (mischief/start e))))


  (define (test/vector/null?)
    (let ((e (compile
              '(let* ((a (list/vector vector/null '(1 2 3)))
                      (b (list/vector a           '(4 5)))
                      (c (list/vector b           '(6 7 8 9))))
                 (list (vector/null? vector/null) (vector/null? a) (vector/null? b) (vector/null? c))))))
      (should-be 'vector/length '(#t #f #f #f) (mischief/start e))))   

  (define (test/vector/cons)
    (let ((e (compile
              '(let* ((a (vector/cons vector/null 33))
                      (b (vector/cons a 44))
                      (c (vector/cons b 55)))
                 (vector/list c)))))
      (should-be 'vector/cons '(33 44 55) (mischief/start e))))

  (define (test/vector/cdr)
    (let ((e (compile
              '(let* ((a (list/vector vector/null '(33 44)))
                      (b (vector/cdr a))
                      (c (vector/cdr b)))
                 (vector/null? c)))))
      (should-be 'vector/cdr #t (mischief/start e))))

  (define (test/vector/filter)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
                 (vector/list (vector/filter v (lambda (x) (odd? x))))))))
      (should-be 'vector/filter '(1 3 5 7) (mischief/start e))))     

  (define (test/vector/map)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) (add1 i)))))
                 (vector/list (vector/map v (lambda (x) (* 2 x))))))))
      (should-be 'vector/map '(2 4 6 8 10 12 14) (mischief/start e))))

  (define (test/vector/ref)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) i))))
                 (+ (vector/ref v 1) (vector/ref v 3) (vector/ref v 6))))))
      (should-be 'vector/ref 10 (mischief/start e))))
  
  (define (test/vector/subvector)
    (let ((e (compile
              '(let ((v (vector/build 7 (lambda (i) i))))
                 (list (vector/list (vector/subvector v 3)) (vector/list (vector/subvector v 0 3)))))))
      (should-be 'vector/subvector '((3 4 5 6) (0 1 2)) (mischief/start e))))
      
  (define (test/vector/update)
    (let ((e (compile
              '(let* ((a (vector/build 5 (lambda (i) i)))
                      (b (vector/update a 3 33))
                      (c (vector/update b 4 44)))
                 (list (vector/list a) (vector/list b) (vector/list c))))))
      (should-be 'vector/subvector '((0 1 2 3 4) (0 1 2 33 4) (0 1 2 33 44)) (mischief/start e))))


  (test/list/vector)
  (test/vector/build)
  (test/vector/fold/left)
  (test/vector/fold/right)
  (test/vepersist?)
  (test/vector/length)
  (test/vector/null?)
  (test/vector/cons)
  (test/vector/cdr)
  (test/vector/filter)
  (test/vector/map)
  (test/vector/ref)
  (test/vector/subvector)
  (test/vector/update))

;; A suite of tests for Mischief persistent hash tables.
(define (test/hash)
  (define (test/hash/pairs)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
                 (sort (hash/pairs h/26) less?)))))
      
      (should-be 'hash/pairs
                 '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8) (i . 9) (j . 10)
                   (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                   (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))
                 (mischief/start e))))

  (define (test/hash/remove)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
                 ; Remove the vowels.
                 (let loop ((h h/26) (vowels '(a e i o u)))
                   (if (null? vowels)
                       (sort (hash/pairs h) less?) ; Sorted pairs but without the vowels.
                       (loop (hash/remove h (car vowels)) (cdr vowels))))))))

          (should-be 'hash/remove
                     '((b . 2) (c . 3) (d . 4) (f . 6) (g . 7) (h . 8) (j . 10)
                           (k . 11) (l . 12) (m . 13) (n . 14) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                           (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))
                     (mischief/start e))))

  (define (test/hash/cons)
    (let ((e (mischief/compile
              '(let ((h/21 ; The alphabet without the vowels.
                      (pairs/hash
                       hash/eq/null
                       '((b . 2) (c . 3) (d . 4) (f . 6) (g . 7) (h . 8) (j . 10)
                         (k . 11) (l . 12) (m . 13) (n . 14) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                         (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
                 ; Add the vowels.
                 (let loop ((h h/21) (vowels '(a e i o u)))
                   (if (null? vowels)
                       (sort (hash/pairs h) less?)
                       (loop (hash/cons h (car vowels) #t) (cdr vowels))))))))
          (should-be 'hash/cons
                     '((a . #t) (b . 2) (c . 3) (d . 4) (e . #t) (f . 6) (g . 7) (h . 8) (i . #t) (j . 10)
                       (k . 11) (l . 12) (m . 13) (n . 14) (o . #t) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                       (u . #t) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))
                     (mischief/start e))))
                   


  (define (test/hash/merge)
    (let ((e (mischief/compile
              '(let ((h/21 ; The alphabet without the vowels.
                      (pairs/hash
                       hash/eq/null
                       '((b . 2) (c . 3) (d . 4) (f . 6) (g . 7) (h . 8) (j . 10)
                         (k . 11) (l . 12) (m . 13) (n . 14) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                         (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))
                     (h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
                 (sort (hash/pairs (hash/merge h/21 h/vowels)) less?)))))

          (should-be 'hash/merge
                     '((a . #t) (b . 2) (c . 3) (d . 4) (e . #t) (f . 6) (g . 7) (h . 8) (i . #t) (j . 10)
                       (k . 11) (l . 12) (m . 13) (n . 14) (o . #t) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                       (u . #t) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))
                     (mischief/start e))))

  (define (test/hash/keys)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 (sort (hash/keys h/26) less?)))))
      (should-be 'hash/keys '(a b c d e f g h i j k l m n o p q r s t u v w x y z) (mischief/start e))))


  ; Reference every key/value pair in a hash table.
  (define (test/hash/ref)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26))))
                 (let loop ((alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
                            (values null))
                   (if (null? alphabet)
                       (reverse values)
                       (loop (cdr alphabet) (cons (hash/ref h/26 (car alphabet) #f) values))))))))
      (should-be 'hash/ref '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26) (mischief/start e))))


  ; Using hash/car and hash/cdr desconstruct a hash table.
  (define (test/hash/car)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string (car alpha)) (symbol->string (car beta))))))
                 (let loop ((h h/26) (pairs null))
                   (if (hash/empty? h)
                       (sort pairs less?)
                       (loop (hash/cdr h) (cons (hash/car h) pairs))))))))
      (should-be 'hash/car
                 '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8) (i . 9) (j . 10)
                   (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                   (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))
                 (mischief/start e))))
                 
  (define (test/hash/length)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t))
                     (h/1 (hash/new hash/eq/null 'foo 'bar)))
                 (list (hash/length (hash/cdr h/1)) (hash/length h/1) (hash/length h/vowels) (hash/length h/26))))))
      (should-be 'hash/length '(0 1 5 26) (mischief/start e))))

  (define (test/hash/empty?)
    (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                     (h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t))
                     (h/1 (hash/new hash/eq/null 'foo 'bar)))
                 (list (hash/empty? (hash/cdr h/1)) (hash/empty? h/1) (hash/empty? h/vowels) (hash/empty? h/26))))))
      (should-be 'hash/empty? '(#t #f #f #f) (mischief/start e))))

  (define (test/hash/contains?)
    (let ((e (mischief/compile
              '(let ((h/vowels (hash/new hash/eq/null 'a #t 'e #t 'i #t 'o #t 'u #t)))
                 (list
                  (hash/contains? h/vowels 'a)
                  (hash/contains? h/vowels 'e)
                  (hash/contains? h/vowels 'i)
                  (hash/contains? h/vowels 'o)
                  (hash/contains? h/vowels 'u)
                  (hash/contains? h/vowels 'z))))))
      (should-be 'hash/contains? '(#t #t #t #t #t #f) (mischief/start e))))

  (define (test/hash/fold)
        (let ((e (mischief/compile
              '(let ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26))))
                 (hash/fold
                  h/26
                  (lambda (pair seed)
                    (if (< (cdr pair) seed)
                        (cdr pair)
                        seed))
                  9999)))))
          (should-be 'hash/fold 1 (mischief/start e))))

  (define (test/hash/map)
    (let ((e (mischief/compile
              '(let* ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                      (map (hash/map
                            h/26
                            (lambda (pair)
                              (cons
                               (string-append (symbol->string (car pair)) "." (number->string (cdr pair)))
                               #t)))))
                 (sort (hash/keys map) string<?)))))
      (should-be 'hash/map 
                 '("a.1" "b.2" "c.3" "d.4" "e.5" "f.6" "g.7" "h.8" "i.9" "j.10"
                   "k.11" "l.12" "m.13" "n.14" "o.15" "p.16" "q.17" "r.18" "s.19" "t.20"
                   "u.21" "v.22" "w.23" "x.24" "y.25" "z.26")
                 (mischief/start e))))
  

  (define (test/hash/filter)
    (let ((e (mischief/compile
              '(let* ((h/26
                      (list/hash
                       hash/eq/null
                       '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                           k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                           u 21 v 22 w 23 x 24 y 25 z 26)))
                      (h/odd (hash/filter h/26 (lambda (pair) (odd? (cdr pair)))))
                      (less? (lambda (alpha beta) (< (cdr alpha) (cdr beta)))))
                 (sort (hash/pairs h/odd) less?)))))
      (should-be 'hash/filter
                 '((a . 1) (c . 3) (e . 5)  (g . 7) (i . 9)
                   (k . 11) (m . 13) (o . 15) (q . 17) (s . 19)
                   (u . 21) (w . 23) (y . 25))
                 (mischief/start e))))
  
  (define (test/hash/partition)
    (let ((e (mischief/compile
              '(let* ((h/26
                       (list/hash
                        hash/eq/null
                        '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
                            k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
                            u 21 v 22 w 23 x 24 y 25 z 26)))
                      (partition (hash/partition h/26 (lambda (pair) (odd? (cdr pair)))))
                      (less? (lambda (alpha beta) (< (cdr alpha) (cdr beta))))
                      (odd  (sort (hash/pairs (car partition)) less?))
                      (even (sort (hash/pairs (cdr partition)) less?)))
                 (list odd even)))))
      (should-be 'hash/partition
                 '(((a . 1) (c . 3) (e . 5)  (g . 7) (i . 9)
                            (k . 11) (m . 13) (o . 15) (q . 17) (s . 19)
                            (u . 21) (w . 23) (y . 25))
                   
                   ((b . 2) (d . 4) (f . 6) (h . 8) (j . 10)
                            (l . 12) (n . 14) (p . 16) (r . 18) (t . 20)
                            (v . 22) (x . 24) (z . 26)))
                 (mischief/start e))))

  (test/hash/pairs)
  (test/hash/remove)
  (test/hash/cons)
  (test/hash/merge)
  (test/hash/keys)
  (test/hash/ref)
  (test/hash/car)
  (test/hash/length)
  (test/hash/empty?)
  (test/hash/contains?)
  (test/hash/fold)
  (test/hash/map)
  (test/hash/filter)
  (test/hash/partition))

(define (test/set)
  (define (test/set/new)
    (let ((e (mischief/compile
              '(let ((s/alphabet
                      (set/new set/eq/null 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z))
                     (less? (lambda (alpha beta)
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 (sort (set/list s/alphabet) less?)))))
      
      (should-be 'set/new
                 '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
                 (mischief/start e))))
  
  (define (test/set/remove)
    (let ((e (mischief/compile
              '(let ((s/alphabet
                      (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 ; Remove the vowels.
                 (let loop ((s s/alphabet) (vowels '(a e i o u)))
                   (if (null? vowels)
                       (sort (set/list s) less?) ; Sorted alphabet but without the vowels.
                       (loop (set/remove s (car vowels)) (cdr vowels))))))))

          (should-be 'set/remove
                     '(b c d f g h j k l m n p q r s t v w x y z)
                     (mischief/start e))))
  
  
  (define (test/set/cons)
    (let ((e (mischief/compile
              '(let ((consonants
                      (list/set set/eq/null '(b c d f g h j k l m n p q r s t v w x y z)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 ; Add the vowels.
                 (let loop ((s consonants) (vowels '(a e i o u)))
                   (if (null? vowels)
                       (sort (set/list s) less?) ; Sorted complete alphabet.
                       (loop (set/cons s (car vowels)) (cdr vowels))))))))

          (should-be 'set/cons
                     '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
                     (mischief/start e))))    

 (define (test/set/union)
    (let ((e (mischief/compile
              '(let ((consonants
                      (list/set set/eq/null '(b c d f g h j k l m n p q r s t v w x y z)))
                     (vowels (list/set set/eq/null '(a e i o u)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 (sort (set/list (set/union consonants vowels)) less?)))))

          (should-be 'set/union
                     '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
                     (mischief/start e))))

 (define (test/set/intersection)
    (let ((e (mischief/compile
              '(let ((alphabet
                      (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                     (other (list/set set/eq/null '(a 1 e 5 i 9 o 15 u 21)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 (sort (set/list (set/intersection alphabet other)) less?)))))

          (should-be 'set/intersection '(a e i o u) (mischief/start e))))

 (define (test/set/difference)
    (let ((e (mischief/compile
              '(let ((alphabet
                      (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                     (other (list/set set/eq/null '(a 1 e 5 i 9 o 15 u 21)))
                     (less? (lambda (alpha beta) ; Return #t iff symbol alpha < symbol beta in a lexical ordering.
                              (string<? (symbol->string alpha) (symbol->string beta)))))
                 (sort (set/list (set/difference alphabet other)) less?)))))

          (should-be 'set/difference '(b c d f g h j k l m n p q r s t v w x y z) (mischief/start e))))

  (define (test/set/contains?)
    (let ((e (mischief/compile
              '(let* ((alphabet
                      (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                      (other '(a 1 e 5 i 9 o 15 u 21))
                      (outside ; Collect all members of other not appearing in alphabet (namely, the digits).
                       (let loop ((other other) (contains null))
                         (if (null? other)
                             contains
                             (if (not (set/contains? alphabet (car other)))
                                 (loop (cdr other) (cons (car other) contains))
                                 (loop (cdr other) contains))))))
                 (sort outside <)))))
      (should-be 'set/contains? '(1 5 9 15 21) (mischief/start e))))

  (define (test/set/car)
    (let ((e (mischief/compile
              '(let loop ((digits (list/set set/eq/null '(1 5 9 15 21)))
                          (outcome null))
                 (if (set/empty? digits)
                     (sort outcome <)
                     (loop (set/cdr digits) (cons (set/car digits) outcome)))))))
      (should-be 'set/car '(1 5 9 15 21) (mischief/start e))))

  (define (test/set/length)
    (let ((e (mischief/compile
              '(let ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                     (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
                     (empty    (set/new set/equal/null)))
                 (list (set/length alphabet) (set/length digits) (set/length empty))))))
      (should-be 'set/length '(26 10 0) (mischief/start e))))
  
  (define (test/set/subset?)
    (let ((e (mischief/compile
              '(let ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                     (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
                     (empty    (set/new set/equal/null))
                     (vowels   (list/set set/eq/null '(a e i o u)))
                     (mixed    (list/set set/eq/null '(a e 7))))
                 (list
                  (set/subset? alphabet set/eq/null) ; The empty set is a subset of every set.
                  (set/subset? alphabet digits)      ; Nope.
                  (set/subset? alphabet vowels)      ; Yup.
                  (set/subset? alphabet mixed))))))  ; Nope.
      (should-be 'set/subset? '(#t #f #t #f) (mischief/start e))))


  (define (test/set/fold)
    (let ((e (mischief/compile
               '(let* ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                       (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
                       (empty    (set/new set/equal/null))
                       (vowels   (list/set set/eq/null '(a e i o u)))
                       (mixed    (list/set set/eq/null '(a e 7)))
                       (all      (set/new set/equal/null alphabet digits empty vowels mixed)))
                  ; Sum the lengths of all sets whose cardinality > 3.
                  (set/fold
                   all
                   (lambda (x seed)
                     (if (> (set/length x) 3)
                         (+ (set/length x) seed)
                         seed))
                   0)))))
      (should-be 'set/fold 41 (mischief/start e))))
                  

  (define (test/set/map)
    (let ((e (mischief/compile
               '(let* ((alphabet (list/set set/eq/null '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
                       (digits   (set/new set/eq/null 0 1 2 3 4 5 6 7 8 9))
                       (empty    (set/new set/equal/null))
                       (vowels   (list/set set/eq/null '(a e i o u)))
                       (mixed    (list/set set/eq/null '(a e 7)))
                       (colors   (list/set set/eq/null '(red white blue)))
                       (all      (set/new set/equal/null alphabet digits empty vowels mixed colors)))
                  
                  (sort
                   (set/list
                    ; Map all into the set lengths of its members.
                    (set/map all (lambda (x) (set/length x))))
                   <)))))
      (should-be 'set/map '(0 3 5 10 26) (mischief/start e))))

  (define (test/set/filter)
    (let ((e (mischief/compile
              '(let ((s (set/new set/eq/null 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26)))
                 (sort (set/list (set/filter s odd?)) <)))))
      (should-be 'set/filter '(1 3 5 7 9 11 13 15 17 19 21 23 25) (mischief/start e))))

  (define (test/set/partition)
    (let ((e (mischief/compile
              '(let* ((all  (set/new set/eq/null 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
                      (odd  (set/filter all odd?))
                      (even (set/filter all even?))
                      (both (set/partition all odd?))) ; Should be (odd . even).
                 (list
                  (set/empty? (set/intersection odd even))
                  (set/subset? (car both) odd)
                  (set/subset? odd (car both))
                  (set/subset? (cdr both) even)
                  (set/subset? even (cdr both)))))))
      (should-be 'set/partition '(#t #t #t #t #t) (mischief/start e))))
  
  (test/set/new)
  (test/set/remove)
  (test/set/cons)
  (test/set/union)
  (test/set/intersection)
  (test/set/difference)
  (test/set/contains?)
  (test/set/car)
  (test/set/length)
  (test/set/subset?)
  (test/set/fold)
  (test/set/map)
  (test/set/filter)
  (test/set/partition))

(define (test/tuple)
  (define (test/tuple/tuple)
    (let ((e (mischief/compile
              '(let ((t (tuple 'a 'b 'c 'd 'e)))
                 (tuple/list t)))))
      (should-be 'tuple '(a b c d e) (mischief/start e))))


  (define (test/tuple/build)
    (let ((e (mischief/compile
              '(let ((t (tuple/build 5 (lambda (i) (* i 3)))))
                 (tuple/list t)))))
      (should-be 'tuple/build '(0 3 6 9 12) (mischief/start e))))

  (define (test/tuple/filter)
    (let ((e (mischief/compile '(tuple/list (tuple/filter (tuple 1 2 3 4 5)  (lambda (x) (even? x)))))))
      (should-be 'tuple/filter '(2 4) (mischief/start e))))

  (define (test/tuple/map)
    (let ((e (mischief/compile '(tuple/list (tuple/map (tuple 1 2 3 4 5) (lambda (n) (1- n)))))))
      (should-be 'test/tuple/map '(0 1 2 3 4) (mischief/start e))))

  (define (test/tuple/partition)
    (let ((e (mischief/compile
              '(let ((p (tuple/partition (tuple 1 2 3 4 5) (lambda (n) (even? n)))))
                 (cons (tuple/list (car p)) (tuple/list (cdr p)))))))
      (should-be 'test/tuple/partition '((2 4) 1 3 5) (mischief/start e))))

  (test/tuple/tuple)
  (test/tuple/build)
  (test/tuple/filter)
  (test/tuple/map)
  (test/tuple/partition))

(define (test/box)
  (let ((e (mischief/compile
            '(let* ((a (box 99))
                    (b (box "foobar"))
                    (c (string=? (unbox b) "foobar")))
               (box! b "zzz")
               (list (box? a) (unbox a) c (unbox b))))))
    (should-be 'test/box '(#t 99 #t "zzz") (mischief/start e))))
               
(define (test/reglobal)

  (let ((e (mischief/compile
            '(let ((x (lambda () foobar)))
               x)))
        (alpha (make-immutable-hasheq '((foobar . alpha))))
        (beta  (make-immutable-hasheq '((foobar . beta)))))
    (pretty-display (mischief/decompile e))
    (display alpha) (display "\n")
    (display beta) (display "\n")
    (let ((a (e rtk/RETURN (vector #f alpha)))
          (b (e rtk/RETURN (vector #f beta))))
      (display (eq? a b)) (display "\n")
      (display (a rtk/RETURN))
      (display "\n")
      (display (b rtk/RETURN))
      (thread
       (lambda ()
         (let loop ()
           (display "a:")
           (display (a rtk/RETURN))
           (display "\n")
           (sleep 0.2)
           (loop))))
      
      (thread
       (lambda ()
         (let loop ()
           (display "b:")
           (display (b rtk/RETURN))
           (display "\n")
           (sleep 0.2)
           (loop)))))))

(define (test/environ)

  (define (test/environ/1)
    (let* ((e (mischief/compile '(lambda () (random))))
           (E (environ/cons ENVIRON/TEST 'random (lambda (k _rte _global) (k (random 1000)))))
           (f (motile/start e E)))
      
      (display 'test/environ/1) (newline)
      (pretty-display (motile/decompile e)) (newline) (newline)
      (pretty-display (motile/decompile f)) (newline) (newline)
      (display (motile/start f E)) (newline)))
  
  (define (test/environ/2)
    (let ((e (mischief/compile
               '((lambda (x) (random x)) 17)))
          (E (environ/cons ENVIRON/TEST 'random (lambda (k _rte _global x) (k (random x))))))
      (display 'test/environ/2) (newline)
      (display (motile/decompile e)) (newline) (newline)
      (display (motile/start e E)) (newline)))
      
  (define (test/environ/3)
    (let ((e (mischief/compile
             '((lambda (n) (add1 n)) 33))))
      (display 'test/environ/3) (newline)
      (display (motile/decompile e)) (newline) (newline)
      (display (motile/start e ENVIRON/TEST)) (newline)))

  (test/environ/1)
  (test/environ/2)
  (test/environ/3))


;; There is a serious bug in the compiler with regard to nested defines (and hence nested letrecs) that is illustrated
;; by some of the tests in test/xdefine.
;; I've decided to let it go for now (2011.06.23) since it is possible to write around it by avoiding nested defines
;; but intend to come back to it when time permits.
(define (test/xdefine)
  (define (test/xdefine/1)
    (let ((f (motile/compile
              '((lambda ()
                  (define (fact n) (if (= n 1) 1 (* n (fact (sub1 n)))))
                  (fact 5))))))
      (display "test/xdefine/1\n")
      (display (motile/start f ENVIRON/TEST))))
  
  ;; Hand compilation of the nested define's in a lambda body.
  (define (test/xdefine/2a)
    (let ((f (motile/compile
              '((lambda (a b)
                  (letrec ((a* (lambda () (* a (b-))))
                           (b- (lambda () (sub1 b))))
                    (a*)))
                8 -1))))
      (should-be 'test/xdefine/2a -16 (motile/start f ENVIRON/TEST))))
  
  (define (test/xdefine/2b)
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a*) (* a (b-)))
                  (define (b-) (sub1 b))
                  (a*))
                8 -1))))
      (should-be 'test/xdefine/2b -16 (motile/start f ENVIRON/TEST))))

  
  ; !!! This test exposes a bug in the code generation for nested defines. !!!
  (define (test/xdefine/2d.1) 
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a*)
                    (define (b-) -2)
                    (* a (b-)))
                  (a*))
                8 -1))))
      (should-be 'test/xdefine/2d.1 -16 (motile/start f ENVIRON/TEST))))
  

  ; !!! This test fails as well---returning a compiler-generated closure---indicating a problem with stack accounting
  ; and address generation for closed variables.
  (define (test/xdefine/2d.2) 
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a*)
                    (define (b-) -2)
                    a)
                  (a*))
                8 -1))))
      (should-be 'test/xdefine/2d.2 8 (motile/start f ENVIRON/TEST))))
  
  ; !!! This test fails catastrophically with an exception in stack addressing.
  (define (test/xdefine/2d.3) 
    (let ((f (motile/compile
              '((lambda (a b)
                  (define (a*)
                    (define (b-) -2)
                    b)
                  (a*))
                8 -1))))
      (should-be 'test/xdefine/2d.3 -1 (motile/start f ENVIRON/TEST))))

  
  ; This test is a hand-compilation of test 2d above into the letrec representation used by the compiler.
  ; It fails in exactly the same way as test 2d.
  ; There is an error in the stack accounting and address generation.
  (define (test/xdefine/2c)
    (let ((f (motile/compile
              '((lambda (a b)
                  (letrec
                      ((a* (lambda ()
                             (letrec ((b- (lambda () (sub1 b))))
                               (* a (b-))))))
                    (a*)))
                8 -1))))
      (should-be 'test/xdefine/2c -16 (motile/start f ENVIRON/TEST))))
    
  (test/xdefine/1)
  (test/xdefine/2a)
  (test/xdefine/2b)
  (test/xdefine/2d.2)
  (test/xdefine/2d.3))

(define (test/hash/null)
  (let ((f (motile/compile 'hash/eq/null)))
    (motile/start f ENVIRON/TEST)))
    
    
