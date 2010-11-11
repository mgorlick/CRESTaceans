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
(define (generate p dir)
  (draw-peer p 0 dir))

; draw-peer: peer int -> nothing
; print out the TeX diagram specification describing the peer
; at a specified indentation level
(define (draw-peer p ilvl dir)
  (cond
    [(childless? p) (draw-orphan p ilvl dir)]
    [else (draw/subpeer p ilvl dir)]))

; childless?: peer -> boolean
; returns #f if the peer has no subpeers, or #t if it does
(define (childless? p)
  (or (not (peer-subpeers p))
      (empty? (peer-subpeers p))))

; draw/subpeer: peer int -> nothing
; draw a peer with subpeers
(define (draw/subpeer p ilvl dir)
  (cond
    [(eq? dir 'right) (printi ilvl "~a\\ \\fbox{$~n" (peer-name p))
                      (printi (+ 1 ilvl) "\\begin{array}{ l l || l }~n")]
    [(eq? dir 'left) (printi ilvl "\\fbox{$~n") 
                     (printi (+ 1 ilvl) "\\begin{array}{ l || l l }~n")])
  (draw/subpeer/first-row p (+ 2 ilvl) dir)
  (draw/subpeer/rest-rows p (rest (peer-subpeers p)) (+ 2 ilvl) 2 dir)
  (printi (+ 1 ilvl) "\\end{array}~n")
  (printi ilvl "$}~n")
  (cond [(eq? dir 'left) (printi ilvl "\\ ~a\\~n" (peer-name p))])
  )

; draw/subpeer/first-row: peer int -> nothing
; print the first row of a peer with subpeers
(define (draw/subpeer/first-row p ilvl dir)
  (cond
    [(eq? dir 'right) (printi ilvl "~a & \\lambda~a & % row 1~n" (peer-ee p) (peer-comp p))
                      (draw-peer (first (peer-subpeers p)) ilvl dir)]
    [(eq? dir 'left) (draw-peer (first (peer-subpeers p)) ilvl dir)
                     (printi ilvl "& ~a & \\lambda~a % row 1~n" (peer-ee p) (peer-comp p))])
  
  (printi ilvl "\\\\~n")
  (printi ilvl "\\ & \\ & \\\\ % empty line~n"))


; draw/subpeer/rest-rows: peer (listof peer) int -> nothing
; print all remaining rows of a peer with subpeers
(define (draw/subpeer/rest-rows p subps ilvl c dir)
  (cond
    [(= (length subps) 0)
     ; there is no subpeer in this last row, so just print the 
     ; peer's url and binding environment
     (cond [(eq? dir 'left) (printi ilvl "\\ & \\ ~n")])
     (printi ilvl "~a & ~a % row ~a~n" (peer-url p) (peer-bev p) c)]
    [(= (length subps) 1)
     ; print the peer's url and binding environment, then print the 
     ; subpeer that matches the row
     (cond [(eq? dir 'right) (printi ilvl "~a & ~a & % row ~a~n" (peer-url p) (peer-bev p) c) 
                             (draw-peer (first subps) ilvl dir)
                             (printi ilvl "\\\\~n")
                             ]
           [(eq? dir 'left) (draw-peer (first subps) ilvl dir)
                            (printi ilvl "& ~n")
                            (printi ilvl "~a & ~a \\ % row ~a~n" (peer-url p) (peer-bev p) c)])
     ]
    [else
     ; empty row on the peer side; full subpeer on the other side
     (cond 
       [(eq? dir 'right) (printi ilvl "\\ & \\ & \\ % row ~a~n" c)
                         (draw-peer (first subps) ilvl dir)
                         (printi ilvl "\\\\~n")
                         (printi ilvl "\\ & \\ & \\\\ % empty line~n")
                         (draw/subpeer/rest-rows p (rest subps) ilvl (+ c 1) dir)]
       [(eq? dir 'left) (draw-peer (first subps) ilvl dir)
                        (printi ilvl "\\\\~n")
                        (printi ilvl "\\ & \\ & \\\\ % empty line~n")
                        (draw/subpeer/rest-rows p (rest subps) ilvl (+ c 1) dir)])
     ]))

; draw-orphan: peer int -> nothing
; print a peer with no subpeers
(define (draw-orphan p ilvl dir)
  (cond
    [(eq? dir 'right) (printi ilvl "~a\\ \\fbox {$~n" (peer-name p))]
    [(eq? dir 'left) (printi ilvl "\\fbox {$~n")])
  (printi (+ 1 ilvl) "\\begin{array}{ l l }~n")
  (printi (+ 2 ilvl) "~a & \\lambda~a \\\\~n" (peer-ee p) (peer-comp p))
  (printi (+ 2 ilvl) "~a & ~a~n" (peer-url p) (peer-bev p))
  (printi (+ 1 ilvl) "\\end{array}~n")
  (printi ilvl "$}~n")
  (cond
    [(eq? dir 'left) (printi ilvl "\\ ~a\\~n" (peer-name p))]))

; printi: int string any ... -> nothing
; do a printf, appending (4 * ilvl) spaces to the beginning
(define (printi ilvl fmtstring . args)
  (cond
    [(eq? ilvl 0) (apply printf (cons fmtstring args))]
    [else
     (let ([spacestring (make-string (* 4 ilvl) #\space)])
       (apply printf (cons (string-append spacestring fmtstring) args)))]))