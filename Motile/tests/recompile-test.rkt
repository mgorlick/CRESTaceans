#lang racket/base

(require
 rackunit
 "../recompile.rkt"

 (only-in
  "../persistent/environ.rkt"
  environ/value)

 (only-in
  "../compile.rkt"
  motile/compile
  motile/decompile
  motile/start)

 "../baseline.rkt")

; Motile source is:
; (let ((silly 1951))
;   (environ/cons environ/null silly))
(check-eqv?
 (let ((code (motile/recompile/unit
              '#(combination
                 1
                 #(lambda 1 #(environ/cons 1 #(reference/global environ/null) #(silly) #(#(reference/parameter 1))))
                 (#(constant/generate 1951))))))
   (environ/value (motile/start code BASELINE) 'silly #f))
 1951
 "Recompilation of environ/cons")

(check-equal?
 (let ((code
        (motile/decompile
         (motile/compile
          '(let ((E (let ((a 100) (b 200) (c 700)) (environ/cons environ/null a b c)))
                 (x 0))
             (list (environ/value E c #f) (environ/value E a #f) (environ/value E b #f) (environ/value E x "not there")))))))
   (motile/start (motile/recompile/unit code) BASELINE))
 '(700 100 200 "not there")
 "Recompilation of environ/cons and environ/value")

(check-equal?
 (let ((code
        (motile/decompile
         (motile/compile
          '(let* ((E (let ((a 100) (b 200) (c 700)) (environ/cons environ/null a b c)))
                  (F (environ/remove E a)))
             (list (environ/value F c #f) (environ/value F a #f) (environ/value F b #f)))))))
   (motile/start (motile/recompile/unit code) BASELINE))
 '(700 #f 200)
 "Recompilation of environ/remove")

(check-eqv?
 (let ((code
        (motile/decompile
         (motile/compile
          '(let ((E (let ((a 100) (b 200) (c 700) (plus +)) (environ/cons environ/null plus a b c))))
             (environ/reflect E (plus a b c)))))))
   (motile/start (motile/recompile/unit code) BASELINE))
 1000
 "Recompilation of environ/reflect")

(check-eqv?
 (let ((code
        (motile/decompile
         (motile/compile
          '(let* ((E (let ((a 100) (b 200) (c 700)) (environ/cons environ/null a b c)))
                  (F (environ/merge (environ/capture) E))) ; BASElINE + a/100, b/200, and c/700.
             (+ (environ/value F c 19) (environ/value F a 17) (environ/value F b 18)))))))
   (motile/start (motile/recompile/unit code) BASELINE))
 1000
 "Recompilation of environ/capture and environ/merge")