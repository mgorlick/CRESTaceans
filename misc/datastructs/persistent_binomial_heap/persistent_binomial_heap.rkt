#lang racket

(require "persistent_binomial_tree.rkt")

(provide
 bheap/new
 bheap/insert
 bheap/empty?
 bheap/peek-at-max
 bheap/remove-max
 bheap/sanity-check)

;;Binomial heaps are structured as a list of increasing-order binomial trees,
;;as well as a single piece of metadata - the number of insertions done. Each insertion
;;increases the value by 1; by tagging inserted nodes with this info we can preserve inorder
;;dequeueing even for priority collisions
(define (bheap/new-single priority value inorder-tracker)
  (list inorder-tracker (list (btree/new priority value inorder-tracker))))

(define (bheap/new)
  (list 0 '()))

(define (bheap/temp-with-parameters inorder-tracker forest)
  (list inorder-tracker forest))

(define-syntax-rule (bheap/access h index)
  (list-ref h index))

(define (bheap/inorder-tracker h)
  (bheap/access h 0))

(define (bheap/forest h)
  (bheap/access h 1))

;;Inserts a new value by creating a temporary heap with a single value and mergin it with the given heap
(define (bheap/insert h priority value)
  (bheap/merge h (bheap/new-single priority value (add1 (bheap/inorder-tracker h)))))

;;Finds the lowest order tree in the specified forest. Used for the merge function.
(define-syntax-rule (bheap/lowest-order forest)
  (if (not (null? forest))
      (btree/order (car forest))
      -1))

;;An empty heap is the same as the empty list
(define (bheap/empty? h)
  (null? (bheap/forest h)))

;;Merges two binomial heaps into one leveraging the tree merge function.
(define (bheap/merge heap1 heap2)
  ;;(printf "H1: ~s H2: ~s~n" heap1 heap2)
  (list (max (bheap/inorder-tracker heap1) (bheap/inorder-tracker heap2)) (merge-helper '() (bheap/forest heap1) (bheap/forest heap2) '())))

;;Merges two forests - used primarily for node insertion/deletion
;;The current tree parameter is used to track and handle "carries" in the
;;process of merging - for example, if we merge two trees of order 0, we need to check
;;before we add it to the result if the heaps we're merging have any trees of order 1
(define (merge-helper acc f1 f2 current-tree)
  ;;(printf "F1: ~s F2: ~s~n" f1 f2)
  (let* ([low-order1 (bheap/lowest-order f1)]
         [low-order2 (bheap/lowest-order f2)]
         [curr-tree-order (if (null? current-tree)
                              -1
                              (btree/order current-tree))])
    (if (null? current-tree) ;;No carry to deal with in this case
        (cond
          ([null? f1] (append (reverse acc) f2))
          ([null? f2] (append (reverse acc) f1))
          ;;In the following two cases, no merge takes place - we simply add the the one of the next two trees with lower order to the result and continue the merge from there
          ([< low-order1 low-order2]
           (merge-helper (cons (car f1) acc) (cdr f1) f2 current-tree))
          ([> low-order1 low-order2]
           (merge-helper (cons (car f2) acc) f1 (cdr f2) current-tree))
          ;;When the next two trees have the same order, rather than add anything to the result, we merge them and pass them as the result of a carry back to the merge-helper function
          ([= low-order1 low-order2]
           (merge-helper acc (cdr f1) (cdr f2) (btree/merge (car f1) (car f2)))))
        (cond ;;Deal with carries in this case
          ;;If we have a carry but one of the heaps is empty, simply make the carry result its own heap and merge it with the other
          ([null? f1]
           (merge-helper acc (list current-tree) f2 '()))
          ([null? f2]
           (merge-helper acc (list current-tree) f1 '()))
          ;;If the next two trees will themselves merge OR if the carry has order lower than both the next trees, we can go ahead and add the carry to the result and continue
          ([or (= low-order1 low-order2) (and (< curr-tree-order low-order1) (< curr-tree-order low-order2))]
           (merge-helper (cons current-tree acc) f1 f2 '()))
          ;;In the two cases below, we need to do another carry since the order of the carry matches with one but not both of the next trees. Merge the carry with the matching order tree and pass that as the carry argument to the next call of merge-helper
          ([= low-order1 curr-tree-order]
           (merge-helper acc (cdr f1) f2 (btree/merge current-tree (car f1))))
          ([= low-order2 curr-tree-order]
           (merge-helper acc f1 (cdr f2) (btree/merge current-tree (car f2))))))))
            
            
;;Returns the tree whose root is max amongst all the trees in the heap
;;or #f if the heap is empty
(define (bheap/max-tree h)
  (if (bheap/empty? h)
      #f
      (max-tree-finder (bheap/forest h) (car (bheap/forest h)))))

;;Finds the tree with the max root amongst all the roots
(define (max-tree-finder forest current-max)
  (if (null? forest)
      current-max
      (if (or (> (btree/priority (car forest)) (btree/priority current-max))
              (and (= (btree/priority (car forest)) (btree/priority current-max)) (< (btree/inorder-tracker (car forest)) (btree/inorder-tracker current-max))))
          (max-tree-finder (cdr forest) (car forest))
          (max-tree-finder (cdr forest) current-max))))
        

;;Returns a the max priority currently in the heap and the value associated with that priority,
;;or #f if the heap is empty
(define (bheap/peek-at-max h)
  (let ([max-tree (bheap/max-tree h)])
    (if (not max-tree)
        #f
        (values (btree/priority max-tree) (btree/value max-tree)))))

;;Returns the value associated with max priority and the updated version of the heap,
;;or #f if the heap is empty
(define (bheap/remove-max h)
  (let ([max-tree (bheap/max-tree h)])
    (if (not max-tree)
        #f
        (values (btree/value max-tree) (bheap/merge (bheap/temp-with-parameters (bheap/inorder-tracker h) (remq max-tree (bheap/forest h))) 
                                                  (bheap/temp-with-parameters (bheap/inorder-tracker h) (reverse (btree/child-list max-tree))))))))

;;Sanity check for the heap
(define (bheap/sanity-check h)
  (bheap/orders-check (bheap/forest h))
  (bheap/verify-trees h))

(define (bheap/orders-check forest)
  (let check-helper ([curr (bheap/lowest-order forest)] [f (cdr forest)])
    (if (null? f)
        #t
        (if (> curr (bheap/lowest-order f))
            (begin
              (printf "Orders property violated - ~s follows ~s~n" (bheap/lowest-order f) curr)
              (error "Sanity check failed"))
            (check-helper (bheap/lowest-order f) (cdr f))))))

(define (bheap/verify-trees h)
  (map (lambda (tree)
         (btree/verify tree))
       (bheap/forest h)))
    
            
      