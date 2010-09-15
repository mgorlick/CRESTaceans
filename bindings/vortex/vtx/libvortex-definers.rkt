#lang racket

(require ffi/unsafe)
(provide (all-defined-out))

(define libvortex (ffi-lib "libvortex-1.1"))
(define libvortex-alive (ffi-lib "/usr/local/lib/libvortex-alive-1.1")) ; XXX find a way to get rid of path
(define libvortex-pull (ffi-lib "libvortex-pull-1.1"))
(define libvortex-sasl (ffi-lib "libvortex-sasl-1.1"))
(define libvortex-tls (ffi-lib "libvortex-tls-1.1"))
(define libvortex-tunnel (ffi-lib "libvortex-tunnel-1.1"))

; Produce two macros:
; id1: obj typ -> define one object in lib with name obj and type signature typ
; id2: typ obj ... -> define many objects in lib that share type signature typ
(define-syntax-rule (define-vtx-definer lib id1 id2)
  (begin
    (define-syntax-rule (id1 obj typ)
        (define obj (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) lib typ)))
    (define-syntax-rule (id2 typ obj (... ...))
      (begin (id1 obj typ)
             (... ...)))))

(define-vtx-definer libvortex defvtx defvtx*)
(define-vtx-definer libvortex-alive deftvxa defvtxa*)
(define-vtx-definer libvortex-pull defvtxp defvtxp*)
(define-vtx-definer libvortex-sasl defvtxs defvtxs*)
(define-vtx-definer libvortex-tls defvtxtl defvtxtl*)
(define-vtx-definer libvortex-tunnel defvtxtu defvtxtu*)