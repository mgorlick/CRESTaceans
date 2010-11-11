#lang racket

(require "generate-diagram.rkt")

(define H (peer "H" "E:Scheme" "^\\circ_2" "u/h" "B:f_3" '()))
(define I (peer "I" "E:Scheme" "^\\circ_3" "u/i" "B:f_4" '()))
(define J (peer "J" "E:Scheme" "^\\circ_4" "u/j" "B:f_5,f_6" '()))
(define K (peer "K" "E:Scheme" "^\\circ_5" "u/k" "B:f_7" '()))
(define M (peer "M" "E:Scheme" "^\\circ_7" "u/l/m" "B:f_10" '()))
(define N (peer "N" "E:Scheme" "^\\circ_8" "u/l/=n" "B:f_11,f_12" '()))
(define L (peer "L" "E:Scheme" "^\\circ_6" "u/l" "B:f_8,f_9,\\dots" (list M N)))
(define G (peer "G" "E:Scheme" "^\\circ_1" "u" "B:f_1,f_2,\\dots" (list H I J K L)))

(generate G)