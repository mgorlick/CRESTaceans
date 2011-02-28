#lang racket

(require (planet synx/ao/main)
         (prefix-in sf: (planet synx/ao/sample-format-struct)))
(provide (all-defined-out))

(define vorbis-format (sf:make #x10 44100 1 big-endian #"M"))

(define device (open))

(define (play* samples)
  (play device samples))