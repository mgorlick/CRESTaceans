#lang racket/base

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

(require
 (only-in "utility.rkt" decompile? error/motile/internal/call)

 (only-in
  "frame.rkt"
  a/1 a/2 a/3
  a/1! a/2! a/3!
  frame/pop))

(provide letrec/set/generate)

(define (letrec/set/generate n)
  (cond
    ((= n 1) (letrec/set/1/generate))
    ((= n 2) (letrec/set/2/generate))
    ((= n 3) (letrec/set/3/generate))
    (else    (letrec/set/N/generate n))))

(define descriptor/letrec/set/1 (vector-immutable 'letrec/set 1))

(define (letrec/set/1/generate)
  (lambda (k e g)
    (cond
      ((procedure? k)
       (k (a/1! (frame/pop e) (a/1 e)))) ; slot (1 . 1) := slot (0 . 1).
      ((decompile? k e g) descriptor/letrec/set/1)
      (else (error/motile/internal/call 'letrec/set/1)))))

(define descriptor/letrec/set/2 (vector-immutable 'letrec/set 2))

(define (letrec/set/2/generate)
  (lambda (k e g)
    (cond
      ((procedure? k)
       (k
        (let ((prior (frame/pop e)))
          (a/1! prior (a/1 e))    ; slot (1 . 1) := slot (0 . 1).
          (a/2! prior (a/2 e))))) ; slot (1 . 2) := slot (0 . 2).
      ((decompile? k e g) descriptor/letrec/set/2)
      (else (error/motile/internal/call 'letrec/set/2)))))
      
(define descriptor/letrec/set/3 (vector-immutable 'letrec/set 3))

(define (letrec/set/3/generate)
  (lambda (k e g)
    (cond
      ((procedure? k)
       (k
        (let ((prior (frame/pop e)))
          (a/1! prior (a/1 e))    ; slot (1 . 1) := slot (0 . 1).
          (a/2! prior (a/2 e))    ; slot (1 . 2) := slot (0 . 2).
          (a/3! prior (a/3 e))))) ; slot (1 . 3) := slot (0 . 3).
      ((decompile? k e g) descriptor/letrec/set/3))))

(define (descriptor/letrec/set/N n) (vector-immutable 'letrec/set n))

(define (letrec/set/N/generate n)
  (lambda (k e g)
    (cond
      ((procedure? k)
       (k (vector-copy! (frame/pop e) 1 e 1)))
      ((decompile? k e g) (descriptor/letrec/set/N n))
      (else (error/motile/internal/call 'letrec/set/N)))))
