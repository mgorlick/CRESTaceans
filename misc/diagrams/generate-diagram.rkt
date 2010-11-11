#lang racket

(provide peer generate)

; a peer is a struct:
; (name, ee, comp, url, bev, subpeers)
; where name, ee, comp, url, bev are strings describing the
; peer name, execution engine, computation name, url, and binding environment
; all of which can corporate TeX syntax
; and subpeers is a list of peers or '()
(struct peer (name ee comp url bev subpeers))

; generate: peer -> nothing
; print out the TeX diagram specification describing a given peer
(define (generate p)
  (draw-peer p 0))

; draw-peer: peer int -> nothing
; print out the TeX diagram specification describing the peer
; at a specified indentation level
(define (draw-peer p ilvl)
  (cond
    [(childless? p) (draw-orphan p ilvl)]
    [else (draw/subpeer p ilvl)]))

; childless?: peer -> boolean
; returns #f if the peer has no subpeers, or #t if it does
(define (childless? p)
  (or (not (peer-subpeers p))
      (empty? (peer-subpeers p))))

; draw/subpeer: peer int -> nothing
; draw a peer with subpeers
(define (draw/subpeer p ilvl)
  (printi ilvl "~a\\ \\fbox{$~n" (peer-name p))
  (printi (+ 1 ilvl) "\\begin{array}{ l l || l }~n")
  (draw/subpeer/first-row p (+ 2 ilvl))
  (draw/subpeer/rest-rows p (rest (peer-subpeers p)) ilvl)
  (printi (+ 1 ilvl) "\\end{array}~n")
  (printi ilvl "$}~n"))

; draw/subpeer/first-row: peer int -> nothing
; print the first row of a peer with subpeers
(define (draw/subpeer/first-row p ilvl)
  (printi ilvl "~a & \\lambda~a &~n" (peer-ee p) (peer-comp p))
  (draw-peer (first (peer-subpeers p)) (+ 1 ilvl))
  (printi ilvl "\\\\~n")
  (printi ilvl "\\ & \\ & \\\\~n"))

; draw/subpeer/rest-rows: peer (listof peer) int -> nothing
; print all remaining rows of a peer with subpeers
(define (draw/subpeer/rest-rows p subps ilvl)
  (cond
    [(= (length subps) 0)
     (printi (+ 2 ilvl) "~a & ~a &~n" (peer-url p) (peer-bev p))]
    [(= (length subps) 1)
     (printi (+ 2 ilvl) "~a & ~a &~n" (peer-url p) (peer-bev p))
     (draw-peer (first subps) (+ 2 ilvl))
     (printi (+ 2 ilvl) "\\\\~n")]
    [else
     (printi (+ 2 ilvl) "\\ & \\ & \\~n")
     (draw-peer (first subps) (+ 2 ilvl))
     (printi (+ 2 ilvl) "\\\\~n")
     (printi (+ 2 ilvl) "\\ & \\ & \\\\~n")
     (draw/subpeer/rest-rows p (rest subps) ilvl)]
    ))

; draw-orphan: peer int -> nothing
; print a peer with no subpeers
(define (draw-orphan p ilvl)
  (printi ilvl "~a\\ \\fbox {$~n" (peer-name p))
  (printi (+ 1 ilvl) "\\begin{array}{ l l }~n")
  (printi (+ 2 ilvl) "~a & \\lambda~a \\\\~n" (peer-ee p) (peer-comp p))
  (printi (+ 2 ilvl) "~a & ~a~n" (peer-url p) (peer-bev p))
  (printi (+ 1 ilvl) "\\end{array}~n")
  (printi ilvl "$}~n"))

; printi: int string any ... -> nothing
; do a printf, appending (4 * ilvl) spaces to the beginning
(define (printi ilvl fmtstring . args)
  (cond
    [(eq? ilvl 0) (apply printf (cons fmtstring args))]
    [else
     (let ([spacestring (make-string (* 4 ilvl) #\space)])      
       (apply printf (cons (string-append spacestring fmtstring) args)))]))