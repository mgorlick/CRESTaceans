#lang racket

(require "generate-diagram.rkt")

; a peer is defined as follows:
; (define <identifier> (peer <name> <language> <TeX-compatible postfix of \lambda...> <url> <B:function list or symbol> (list of <identifier>, or empty list)

(define H (peer "H" "E:Scheme" "^\\circ_2" "u/h" "B:f_3" '()))
(define I (peer "I" "E:Scheme" "^\\circ_3" "u/i" "B:f_4" '()))
(define J (peer "J" "E:Scheme" "^\\circ_4" "u/j" "B:f_5,f_6" '()))
(define K (peer "K" "E:Scheme" "^\\circ_5" "u/k" "B:f_7" '()))
(define M (peer "M" "E:Scheme" "^\\circ_7" "u/l/m" "B:f_10" '()))
(define N (peer "N" "E:Scheme" "^\\circ_8" "u/l/n" "B:f_11,f_12" '()))
(define L (peer "L" "E:Scheme" "^\\circ_6" "u/l" "B:f_8,f_9,\\dots" (list M N)))
(define G (peer "G" "E:Scheme" "^\\circ_1" "u" "B:f_1,f_2,\\dots" (list H I J K L)))

;(generate G)

(define monitor1 (peer "S1" "E" "^\\circ monitor_1" "cli1.home/s1" "B:read\\_sensor()" '()))
(define monitor2 (peer "S2" "E" "^\\circ monitor_2" "cli1.home/s2" "B:read\\_sensor()" '()))
(define monitor3 (peer "S3" "E" "^\\circ monitor_3" "cli1.home/s3" "B:read\\_sensor()" '()))
(define home (peer "H" "E:Scheme" "^\\circ manager" "cli1.home" "B: \\alpha" (list monitor1 monitor2 monitor3)))
(define home-blank (peer "H" "E:Scheme" "^\\circ manager" "cli1.home" "B: \\alpha" '()))

(define cm1-pc (peer "CM1" "E" "^\\circ monitor_1" "s.pc/client1" "B:process\\_data()" '()))
(define cm2-pc (peer "CM2" "E" "^\\circ monitor_2" "s.pc/client2" "B:process\\_data()" '()))
(define serenapc (peer "S" "E:Scheme" " ^\\circ launcher_0" "s.pc" "B: \\beta^\\circ_0" (list cm1-pc cm2-pc)))
(define serenapc-blank (peer "S" "E:Scheme" "^\\circ launcher_0" "s.pc" "B: \\beta^\\circ_0" '()))

(define cm1-sp (peer "CM1" "E" "^\\circ monitor_1" "s.phone/cli1" "B:process\\_data()" '()))
(define cm2-sp (peer "CM2" "E" "^\\circ monitor_2" "s.phone/cli2" "B:process\\_data()" '()))
(define phone (peer "P" "E:JS" "^\\circ launcher_1" "s.phone" "B: \\beta^\\circ_1" (list cm1-sp cm2-sp)))
(define phone-blank (peer "P" "E:JS" "^\\circ launcher_1" "s.phone" "B: \\beta^\\circ_1" '()))

(generate home 'right)
;(generate serenapc 'left)
;(generate phone 'left)
;(generate serenapc-blank 'left)
;(generate phone-blank 'left)
;(generate home-blank 'right)