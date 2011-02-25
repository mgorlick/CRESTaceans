#lang racket

(provide
 btree/new
 btree/merge
 btree/order
 btree/priority
 btree/value
 btree/inorder-tracker
 btree/child-list
 btree/verify)


;;Creates a new binomial tree
;;Binomial trees are structured as lists, with values:
;;     slot 0 - the order k of the tree
;;     slot 1 - the priortiy (priority of the root)
;;     slot 2 - the value of the root
;;     slot 3 - the insertion order tracking int (metadata used by bheap)
;;     slot 3 - the list of children, where the first in the list is a binom tree of order k-1, the second is order k-2, etc.

(define-syntax-rule (new-btree order priority value inorder-tracker child-list)
  (list order priority value inorder-tracker child-list))

(define-syntax-rule (btree/access b index)
  (if b
      (list-ref b index)
      #f))

(define (btree/order b)
  (btree/access b 0))

(define (btree/priority b)
  (btree/access b 1))

(define (btree/value b)
  (btree/access b 2))

(define (btree/inorder-tracker b)
  (btree/access b 3))

(define (btree/child-list b)
  (btree/access b 4))


;;The two important methods for binomial trees - create a new tree of order 0 and merge two trees of equal order
(define (btree/new priority value inorder-tracker)
  (new-btree 0 priority value inorder-tracker '()))

;;Merges two trees of order k to make a tree of order k+1. The tree with lesser priority becomes the leftmost
;;child of the tree with greater priority. Persistence is easily achieved by simply creating a new root node
;;with a new child list that has the new child appended.
(define (btree/merge b1 b2)
  (if (= (btree/order b1) (btree/order b2))
      (let ([pri1 (btree/priority b1)] [pri2 (btree/priority b2)])
        (cond 
          ([or (< pri1 pri2) (and (= pri1 pri2) (> (btree/inorder-tracker b1) (btree/inorder-tracker b2)))]
           (new-btree (add1 (btree/order b1)) (btree/priority b2) (btree/value b2) (btree/inorder-tracker b2) (cons b1 (btree/child-list b2))))
          ([or (> pri1 pri2) (and (= pri1 pri2) (< (btree/inorder-tracker b1) (btree/inorder-tracker b2)))]
           (new-btree (add1 (btree/order b1)) (btree/priority b1) (btree/value b1) (btree/inorder-tracker b1) (cons b2 (btree/child-list b1))))
          (else (error "Shouldn't get here"))))
      #f))

;;Sanity check functions
(define (btree/verify t)
  (verify-all-orders t)
  (verify-heap t))

(define (verify-all-orders tree)
  (verify-child-order (btree/order tree) (btree/child-list tree))
  (map verify-all-orders (btree/child-list tree)))

(define (verify-child-order curr-order child-list)
  (if (null? child-list)
      (if (not (= 0 curr-order))
          (error "Final order not zero")
          #t)
      (if (= curr-order (add1 (btree/order (car child-list))))
          (verify-child-order (btree/order (car child-list)) (cdr child-list))
          (error "Tree orders inconsistent"))))

(define (verify-heap t)
  (verify-child-heap (btree/priority t) (btree/child-list t))
  (map verify-heap (btree/child-list t)))

(define (verify-child-heap priority child-list)
  (map (lambda (child)
         (if (> (btree/priority child) priority)
             (error "Heapiness violated")
             #t))
       child-list))