(module
 foo
 (<sample>
  up
  tell)
 (import generic-procedures)
 (import oo)
 (import type-system)
 (define-generic :x)
 (define-generic :x!)
 (define-generic up)
 (define-generic tell)
 (define-class (<sample>)
   (x :x :x!))
 (define-method (initialize (<sample> self))
   (:x! self 0))
 (define-method (up (<sample> self) (<number> i))
   (:x! self (+ (:x self) i)))
 (define-method (tell (<sample> self)) (:x self)))
