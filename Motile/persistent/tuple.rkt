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

;; Contact: mgorlick@acm.org

;; A tuple is a vector v = #(tuple <value> ...) where:
;;   v_0 is the literal symbol tuple
;;   v_i, i > 0 is a tuple element e_{i-1}.

(require (only-in racket/vector vector-copy))
(provide
 list/tuple
 tuple
 tuple?
 tuple/append
 tuple/build
 tuple/drop/left
 tuple/drop/right
 tuple/copy
 tuple/filter
 tuple/length
 tuple/list
 tuple/map
 tuple/partition
 tuple/ref
 tuple/take/left
 tuple/take/right)

(define (tuple? x)
  (and
   (vector? x)
   (positive? (vector-length x))
   (eq? '<tuple> (vector-ref x 0))))

;; Give a tuple with n elements return the length of its underlying vector representation.
(define-syntax-rule (length/raw n) (add1 n))

(define (tuple/raw n)
  (let ((t (make-vector (length/raw n) #f)))
    (vector-set! t 0 '<tuple>)
    ; t_1 is for optional t-specific metadata.
    t))

;; Construct a tuple from the given list of values.
(define (list/tuple values)
  (let ((t (tuple/raw (length values))))
    (let loop ((i 1) (values values))
      (if (null? values)
          t
          (begin
            (vector-set! t i (car values))
            (loop (add1 i) (cdr values)))))))

;; Given (tuple v_0 ... v_n) construct a tuple for which element e_i is v_i.
(define (tuple . rest)
  (list/tuple rest))

;; Return the length n >= 0 of tuple t.
(define (tuple/length t)
  (if (tuple? t)
      (sub1 (vector-length t))
      (error 'tuple/length "expects argument of type <tuple>; given ~s" t)))

;; Return the value of tuple element t_i.
(define (tuple/ref t i)
  (if (tuple? t)
      (if (and (integer? i) (>= i 0))
          (vector-ref t (add1 i))
          (error 'tuple/ref "expects index >= 0; given ~s" i))
      (error 'tuple/ref "expects argument of type <tuple>; given ~s" t)))

;; Return the elements t_0, ..., t_{m-1} of tuple t as a list (t_0 ... t_{m-1}).
(define (tuple/list t)
  (if (tuple? t)
      (cdr (vector->list t)) ; Redact the type information.
      (error 'tuple/list "expects argument of type <tuple>; given ~s" t)))

;; Generate a subtuple.
;; (tuple/copy t start) returns a tuple whose contents is t_start, t_{start+1}, ..., t_n where n = |t|-1.
;; (tuple/copy t start end) returns a tuple whose contents is t_start, t_{start+1}, ..., t_{end-1}.
(define tuple/copy
  (case-lambda
    ((t start)
     (if (tuple? t)
         (cond
           ((zero? start) t)
           ((> start 0)
            (let ((x (vector-copy (sub1 start))))
              (vector-set! x (sub1 start) 'tuple)
              x))
           (else
            (error 'tuple/copy "expects start >= 0; given ~s" start)))
         (error 'tuple/copy "expects argument of type <tuple>; given ~s" t)))
    ((t start end)
     (if (tuple? t)
         (cond
           ((zero? start)
            (vector-copy t start (add1 end)))
           ((> start 0)
            (let ((x (vector-copy t start (add1 end))))
              (vector-set! x 0 'tuple)
              x))
           (else
            (error 'tuple/copy "expects start >= 0; given ~s" start)))
         (error 'tuple/copy "expects argument of type <tuple>; given ~s" t)))))

;; Construct a tuple t of length n where each element t_i = (f i).
(define (tuple/build n f)
  (let ((t (tuple/raw n)))
    (let loop ((i 1))
      (when (<= i n)
        (vector-set! t i (f (sub1 i)))
        (loop (add1 i))))
    t))

;; Return a tuple u where u_i = (f t_i).
(define (tuple/map t f)
  (if (tuple? t)
      (let* ((n (sub1 (vector-length t)))
             (x (tuple/raw n)))
        (let loop ((i 1))
          (when (<= i n)
            (vector-set! x i (f (vector-ref t i)))
            (loop (add1 i))))
        x)
      (error 'tuple/map "expects argument of type <tuple>; given ~s" t)))

;(define (tuple/map t f)
;  (if (tuple? t)
;      (vector->immutable-vector (vector-map f t))
;      (error 'tuple/map "expects argument of type <tuple>; given ~s" t)))

;; For (tuple/append t_1 ... t_n), each t_i a tuple, return a tuple x where |x| = |t_1| + ... + |t_n|
;; and the sequence of elements of x is the concentation of the sequences of values of t_1, ..., t_n.
(define (tuple/append . rest)
  (let* ((n
          (let loop ((tuples rest)
                     (n 0))
            (if (null? tuples)
                n
                (loop (cdr tuples) (+ n (tuple/length (car tuples)))))))
         (x (tuple/raw n)))
    ; Copy each source tuple in turn.
    (let loop ((tuples rest)
               (i 1))
      (unless (null? tuples)
        (vector-copy! x i (car tuples) 1)
        (loop (cdr tuples) (+ i (tuple/length (car tuples))))))
    x))

;; Return a tuple x comprising the first n elements of t, that is, elements t_0, ..., t_{n-1}.
(define (tuple/take/left t n)
  (tuple/copy t 0 n))


;; Return a tuple x comprising the last m elements of t, that is, if n = |t| then t_{n-m}, ... t_{n-1}.
(define (tuple/take/right t m)
  (tuple/copy t (- (vector-length t) m)))


;; Return a tuple x comprising all but the first n elements of t.
(define (tuple/drop/left t n)
  (tuple/copy t n))

;; Return a tuple x comprising all but the last n elements of t.
(define (tuple/drop/right t n)
  (tuple/copy t 0 (- (vector-length t) n)))

;; Return a tuple x containing only those elements t_i of t for which (f t_i) is not #f.
(define (tuple/filter t f)
  (let loop ((values null)
             (i (sub1 (vector-length t))))
    (if (zero? i)
        (list/tuple values)
        (let ((x (vector-ref t i)))
          (if (f x)
              (loop (cons x values) (sub1 i))
              (loop values (sub1 i)))))))

;; Return a pair of tuples (u . v) where tuple u contains only those elements t_i of t for
;; which (f t_i) is not #f and tuple v contains only those elements t_j of t for which (f t_j) is #f.
(define (tuple/partition t f)
  (let loop ((i (sub1 (vector-length t)))
             (partition (vector null null)))
    (if (zero? i)
        (cons 
         (list/tuple (vector-ref partition 0))
         (list/tuple (vector-ref partition 1)))
        (let* ((element (vector-ref t i))
               (j (if (f element) 0 1)))
          (vector-set! partition j (cons element (vector-ref partition j)))
          (loop (sub1 i) partition)))))

;; Fold function f over the elements of t with the given seed.
(define (tuple/fold t f seed)
  (let ((n (vector-length t)))
    (let loop ((i 1)
               (seed seed))
      (if (< i n)
          (loop (add1 i) (f (vector-ref t i) seed))
          seed))))

      

