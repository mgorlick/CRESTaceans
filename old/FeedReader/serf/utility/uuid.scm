;; Serf UUID generator.
;; See http://java.sun.com/j2se/1.5.0/docs/api/java/util/UUID.html for details.
;; Copyright 2009 Michael M. Gorlick

;; Exports.

;; Returns a random UUID (type 4) as a Scheme string.
(define (uuid/string)
  (->string (to-string (random-uuid :uuid-generator:))))

;; Returns a random UUID (type 4) as a Scheme symbol.
(define (uuid/symbol)
  (->symbol (to-string (random-uuid :uuid-generator:))))

;; Internals
(define-java-class <java-uuid> |java.util.UUID|)
(define-generic-java-method random-uuid |randomUUID|)
(define-generic-java-method to-string)
(define :uuid-generator:)

;; Internal implementation.
(set! :uuid-generator: (java-null <java-uuid>))

