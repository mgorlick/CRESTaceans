#lang racket/base

(require
 rackunit "compile.rkt"
 racket/pretty
 "baseline.rkt"
 "persistent/environ.rkt"
 "persistent/hash.rkt")


(test-case
 "Binding Environments"
 
 (check-eq?
  (let ((f (motile/compile '(begin environ/null))))
    (motile/start f BASELINE))
  environ/null
  "environ/null is available in the Motile BASELINE enviroment")
 
 (check-exn
  exn:fail?
  (lambda ()
    (let ((f (motile/compile
              '(let ((a "first")
                     (b "second")
                     (c 3))
                 (environ/cons environ/null a 1951 x))))) ; 1951 is not a legal argument.
      "The Motile compiler should issue an error exception"))
  "environ/cons requires that all arguments after environ expression are symbols")
 
 (check-exn
  exn:fail?
  (lambda ()
    (let ((f (motile/compile
              '(let ((a "first")
                     (b "second")
                     (c 3))
                 (environ/cons environ/null a b cons c))))) ; cons is not in lexical scope.
      "The Motile compiler should issue an error exception"))
  "environ/cons requires that all identifiers following the environ expression are in lexical scope")
 
 
 (check-equal?
  (let ((f (motile/compile
            '(let* ((silly 1951)
                    (E (environ/cons environ/null silly))) ; Create an environ with a single binding silly/1951.
               E))))
    (environ/value (motile/start f BASELINE) 'silly #f))
  1951
  "environ/cons adds a binding taken from lexical scope")
 
 (check-equal?
  (let* ((f (motile/compile
             '(let ((a "first")
                    (b "second")
                    (c 3))
                (environ/cons environ/null a b c))))
         (E (motile/start f BASELINE)))
    ; Here we use the version of environ/value available to Racket.
    (list (environ/value E 'c #f) (environ/value E 'b #f) (environ/value E 'a #f)))

  '(3 "second" "first")
  "Add multiple bindings at once to a binding environment")
 
 (check-equal?
  (let ((f (motile/compile
            '(let ((a 100) (b 200) (c 300))
               (let ((E (environ/cons environ/null a b c)))
                 (list (environ/value E c #f) (environ/value E a #f) (environ/value E b #f)))))))
    (motile/start f BASELINE))
  '(300 100 200)
  "Query an environ using environ/value")
 
 (check-equal?
  (let ((f (motile/compile
            '(let ((a 100) (b 200) (c 300))
               (let ((E (environ/cons environ/null a b c)))
                 (list (environ/value E c #f)
                       (environ/value E a #f)
                       (environ/value E x #f) ; x is not a binding in E.
                       (environ/value E b #f)))))))
    (motile/start f BASELINE))
  
  '(300 100 #f 200) ; The value #f comes from the query for symbol x.
  
  "environ/value using a symbol not in the binding environ")
 
 (check-equal?
  (let ((f (motile/compile
            '(let ((a 100) (b 200) (c 300))
               (let* ((E_0 (environ/cons environ/null a b c))
                      (E_1 (environ/remove E_0 b)))
                 (list (environ/value E_1 c #f)
                       (environ/value E_1 a #f)
                       (environ/value E_1 b #f)))))))
    (motile/start f BASELINE))
  '(300 100 #f) ; The value #f comes from the query for symbol b in E_1.
  "environ/remove")
 
 (check-eq?
  (let ((f (motile/compile '(let () (environ/capture)))))
    (motile/start f BASELINE))
  BASELINE
  "environ/capture")
 
 (check-equal?
  (let ((f (motile/compile
            '(let ((global (let ((a 100) (b 200) (c 300) (plus +))
                             (environ/cons environ/null a b c plus))))
               (environ/reflect global (plus a b c))))))
    (motile/start f BASELINE))
  600
  "environ/reflect"))
 
(let ((f (motile/compile
          '(let ((a 100) (b 200))
             (let ((c 300) (d 400))
               (let ((e 500) (f 600))
                 (environ/cons environ/null a c e)))))))
  (pretty-display (motile/decompile f)))

(let ((f (motile/compile
            '(let ((silly 1951))
               (environ/cons environ/null silly))))) ; Create an environ with a single binding silly/1951.
  (pretty-display (motile/decompile f)))
      


