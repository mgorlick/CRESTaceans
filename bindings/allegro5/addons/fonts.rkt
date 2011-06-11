#lang racket/base

(require ffi/unsafe
         "../base/lib.rkt")
(provide (except-out (all-defined-out)
                     liballegro-fonts
                     allegro-func
                     df
                     liballegro-ttf
                     allegro-func2
                     df2))

(define liballegro-fonts (ffi-lib "liballegro_font"))

(define-syntax allegro-func
  (syntax-rules (:)
    [(_ id : x ...)
     (get-ffi-obj (regexp-replaces 'id '((#rx"-" "_"))) liballegro-fonts (_fun x ...)) ]))

(define-syntax df
  (syntax-rules (:)
    [(_ id : x ...) (define id (allegro-func id : x ...))]))

(define-cpointer-type _Allegro-Font-pointer)

(define _int-ptr (_ptr i _int))

(define Allegro-Align-Left 0)
(define Allegro-Align-Centre 1)
(define Allegro-Align-Right 2)

(df al-init-font-addon : -> _void)
(df al-shutdown-font-addon : -> _void)
(df al-load-font : _string _int (_int = 0) -> _Allegro-Font-pointer)
(df al-destroy-font : _Allegro-Font-pointer -> _void)
(df al-get-font-line-height : _Allegro-Font-pointer -> _int)
(df al-get-font-ascent : _Allegro-Font-pointer -> _int)
(df al-get-font-descent : _Allegro-Font-pointer -> _int)
(df al-get-text-width : _Allegro-Font-pointer -> _int)
(df al-get-ustr-width : _Allegro-Font-pointer _Allegro-Ustr-pointer -> _int)
(df al-draw-text : 
    _Allegro-Font-pointer _Allegro-Color _float _float _int _string -> _void)
(df al-draw-ustr :
    _Allegro-Font-pointer _Allegro-Color _float _float _int _Allegro-Ustr-pointer -> _void)
(df al-draw-justified-text :
    _Allegro-Font-pointer _Allegro-Color
    _float _float _float _float _int _string -> _void)
(df al-draw-justified-ustr :
    _Allegro-Font-pointer _Allegro-Color
    _float _float _float _float _int _Allegro-Ustr-pointer -> _void)
(df al-get-text-dimensions :
    _Allegro-Font-pointer _string _int-ptr _int-ptr _int-ptr _int-ptr -> _void)
(df al-get-ustr-dimensions :
    _Allegro-Font-pointer _Allegro-Ustr-pointer _int-ptr _int-ptr _int-ptr _int-ptr -> _void)
(df al-get-allegro-font-version : -> _uint32)

(define liballegro-ttf (ffi-lib "liballegro_ttf"))

(define-syntax allegro-func2
  (syntax-rules (:)
    [(_ id : x ...)
     (get-ffi-obj (regexp-replaces 'id '((#rx"-" "_"))) liballegro-ttf (_fun x ...)) ]))

(define-syntax df2
  (syntax-rules (:)
    [(_ id : x ...) (define id (allegro-func2 id : x ...))]))

(df2 al-init-ttf-addon : -> _void)
; (df2 al-shutdown-ttf-addon : -> _void) ; ???
(df2 al-load-ttf-font : _string _int (_int = 0) -> _Allegro-Font-pointer)
(df2 al-load-ttf-font-f : _Allegro-File-pointer _string _int _int -> _Allegro-Font-pointer)
(df2 al-get-allegro-ttf-version : -> _uint32)