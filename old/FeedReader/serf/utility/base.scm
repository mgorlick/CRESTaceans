;; Miscellaneous base definitions used by the various Serf bridges to the Java classes  and libraries.

;; Copyright Michael M. Gorlick 2009.

;; Definitions
(define-java-class <java-string>  |java.lang.String|)
(define-java-class <java-string-builder> |java.lang.StringBuilder|)
(define :java-null-string:)

;; (set-generic-java-method this-example) ; Does automatic name mangling of this-example to thisExample.
;; (set-generic-java-method this-example |thisExample|) ; Equivalent to above with hand mangling.
(define-syntax set-generic-java-method
  (lambda (x)
    (syntax-case
     x ()
     ((_ name) (syntax (set! name (generic-java-method (java/mangle-method-name (quote name))))))
     ((_ name original) (syntax (set! name (generic-java-method (quote original))))))))

;; (set-java-class scheme-class-name JavaClassName)
(define-syntax set-java-class
  (lambda (x)
    (syntax-case
     x ()
     ((_ name class) (syntax (set! name (java-class (quote class))))))))

(define-generic-java-method to-string)
(define-generic-java-method length)

;; Convert the String representation of a Java object x to a Scheme string.
(define (java-to-string x) (->string (to-string x)))
;; Convert the length of a Java object x to a Scheme integer.
(define (java-to-length x) (->number (length x)))

;; Implementation
(set! :java-null-string:  (java-null <java-string>))
