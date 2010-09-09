#lang racket

(require ffi/unsafe
         "../base/lib.rkt")
(provide (except-out (all-defined-out) allegro-func defallegrop liballegro-p))

(define liballegro-p (ffi-lib "liballegro_primitives" "4.9"))

(define-syntax allegro-func
  (syntax-rules (:)
    [(_ id : x ...)
     (get-ffi-obj (regexp-replaces 'id '((#rx"-" "_"))) liballegro-p (_fun x ...)) ]))

(define-syntax defallegrop
  (syntax-rules (:)
    [(_ id : x ...) (define id (allegro-func id : x ...))]))

(defallegrop al-get-allegro-primitives-version : -> _uint32)
(defallegrop al-init-primitives-addon : -> _bool)
(defallegrop al-shutdown-primitives-addon : -> _void)
(defallegrop al-draw-line :
  _float _float _float _float _Allegro-Color _float -> _void)
(defallegrop al-draw-triangle :
  _float _float _float _float _float _float _Allegro-Color _float -> _void)
(defallegrop al-draw-filled-triangle :
  _float _float _float _float _float _float _Allegro-Color -> _void)
(defallegrop al-draw-rectangle :
  _float _float _float _float _Allegro-Color _float -> _void)
(defallegrop al-draw-filled-rectangle : 
  _float _float _float _float _Allegro-Color -> _void)
(defallegrop al-draw-rounded-rectangle :
  _float _float _float _float _float _float _Allegro-Color _float -> _void)
(defallegrop al-draw-filled-rounded-rectangle :
  _float _float _float _float _float _float _Allegro-Color -> _void)
(defallegrop al-calculate-arc :
  (_ptr i _float) _int _float _float
  _float _float _float _float _float _int -> _void)
(defallegrop al-draw-ellipse :
  _float _float _float _float _Allegro-Color _float -> _void)
(defallegrop al-draw-filled-ellipse :
  _float _float _float _float _Allegro-Color -> _void)
(defallegrop al-draw-circle :
  _float _float _float _Allegro-Color _float -> _void)
(defallegrop al-draw-filled-circle :
  _float _float _float _Allegro-Color -> _void)
(defallegrop al-draw-arc :
  _float _float _float _float _float _Allegro-Color _float -> _void)
(defallegrop al-calculate-spline : 
  (_ptr i _float) _int (_list i _float) _float _int -> _void)
(defallegrop al-draw-spline :
  (_list i _float) _Allegro-Color _float -> _void)
(defallegrop al-calculate-ribbon :
  (_ptr i _float) _int (_ptr i _float) _int _float _int -> _void)
(defallegrop al-draw-ribbon :
  (_ptr i _float) _int _Allegro-Color _float _int -> _void)