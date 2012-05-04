#lang racket/base

(require (for-syntax racket/base))

;; compile-time condition checking. analogous to #ifdef.
(define-syntax (when/compile stx)
  (syntax-case stx ()
    [(_ condition yes ...)
     (let ([result (eval #'condition (make-base-namespace))])
       (if result
           #'(begin yes ...)
           #'(void)))]))

;; video available on all platforms.

(require racket/provide
         (except-in racket/contract ->)
         (rename-in racket/contract [-> ->/c])
         "message-types.rkt"
         "bindings/vp8/vp8.rkt")
(provide (matching-identifiers-out #rx"^vp8dec.*" (all-from-out "bindings/vp8/vp8.rkt"))
         (matching-identifiers-out #rx"^color-converter.*" (all-from-out "bindings/vp8/vp8.rkt"))
         vp8enc-delete
         vp8enc-quartersize-new
         yuv420p-to-rgb32
         dispose-FrameBuffer
         FrameBuffer->Frame
         (except-out (all-defined-out) vp8enc-new* vp8enc-encode* vp8enc-encode-quarter*)
         (rename-out (vp8enc-new* vp8enc-new)
                     (vp8enc-encode* vp8enc-encode)
                     (vp8enc-encode-quarter* vp8enc-encode-quarter)))

(define/contract (vertical-flip yuv420p-content w h)
  (bytes? (and/c exact-nonnegative-integer? even?) (and/c exact-nonnegative-integer? even?) . ->/c . bytes?)
  (define l (bytes-length yuv420p-content))
  (define expected (/ (* 3 w h) 2))
  (unless (= l expected)
    (error 'vertical-flip "content length doesn't match declared dimensions (was ~a, should be ~a)"
           l expected))
  (define flipped (make-bytes l 128))
  (define yl (* 2 (/ l 3)))
  (define ul (/ l 6))
  (define vl (/ l 6))
  (for ([num-rows       (list h  (/ h 2)   (/ h 2))]
        [sopr           (list w  (/ w 2)   (/ w 2))]
        [src-offset     (list yl (+ yl ul) (+ yl ul vl))]
        [dest-offset    (list 0  yl        (+ yl ul))])
    (for ([i (in-range num-rows)])
      (define row-at (- src-offset (* (add1 i) sopr)))
      (bytes-copy! flipped (+ dest-offset (* i sopr)) yuv420p-content row-at (+ row-at sopr))))
  flipped)

(define (vp8dec-decode dec content width height)
  (define raw-yuv420p-frame (vp8dec-decode-copy dec content width height))
  raw-yuv420p-frame)

(define (vp8enc-new* params)
  (vp8enc-new (:VideoParams/width params)
              (:VideoParams/height params)
              (:VideoParams/fpsNum params)
              (:VideoParams/fpsDen params)))

(define (vp8enc-quartersize-new params)
  (vp8enc-new (/ (:VideoParams/width params) 2)
              (/ (:VideoParams/height params) 2)
              (:VideoParams/fpsNum params)
              (:VideoParams/fpsDen params)))

(define (vp8enc-encode* e frame outbuff)
  (define written (vp8enc-encode e (:FrameBuffer/size frame)
                                 (:FrameBuffer/data frame)
                                 (bytes-length outbuff)
                                 outbuff))
  (FrameBuffer/new (subbytes outbuff 0 written) written #f (:FrameBuffer/ts frame)))

(define (vp8enc-encode-quarter* e frame outbuff row col)
  (define written (vp8enc-encode-quarter e row col
                                         (:FrameBuffer/size frame)
                                         (:FrameBuffer/data frame)
                                         (bytes-length outbuff)
                                         outbuff))
  (FrameBuffer/new (subbytes outbuff 0 written) written #f (:FrameBuffer/ts frame)))

(define (dispose-FrameBuffer f)
  ((:FrameBuffer/disposal f)))

(define (FrameBuffer->Frame v)
  (Frame/new (if (equal? (bytes-length (:FrameBuffer/data v)) (:FrameBuffer/size v))
                 (:FrameBuffer/data v)
                 (subbytes (:FrameBuffer/data v) 0 (:FrameBuffer/size v)))
             (:FrameBuffer/ts v)))

;;; Linux-specific for now: uses Video4Linux2
(require "bindings/ctypes.rkt"
         ffi/unsafe)
(when/compile (equal? 'unix (system-type 'os))
              (define v4l2lib (ffi-lib "libracket-v4l2-wrapper"))
              (define-syntax-rule (defv4l2+ binding obj typ)
                (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) v4l2lib typ)))
              (define-syntax-rule (defv4l2 obj typ)
                (defv4l2+ obj obj typ))
              (define-syntax-rule (defv4l2* typ obj ...)
                (begin (defv4l2 obj typ) ...))
              
              (define-cpointer-type _v4l2-reader-pointer)
              
              (defv4l2 v4l2-reader-setup (_fun _string _uint _uint -> _v4l2-reader-pointer))
              
              (defv4l2 v4l2-reader-delete (_fun _v4l2-reader-pointer -> _void))
              
              (defv4l2 v4l2-reader-get-params
                (_fun _v4l2-reader-pointer
                      (frame-width : (_ptr o _uint))
                      (frame-height : (_ptr o _uint))
                      (fps-num : (_ptr o _uint))
                      (fps-denom : (_ptr o _uint))
                      (buffer-ct : (_ptr o _uint))
                      -> _void
                      -> (values frame-width frame-height fps-num fps-denom buffer-ct)))
              
              (defv4l2 v4l2-reader-is-ready
                (_fun _v4l2-reader-pointer -> _bool))
              
              ;; get a valid pointer to the memory mapped bytestring with its size
              ;; and index number tracked. mmapped buffer is not requeued until
              ;; v4l2-reader-enqueue-buffer is called with the index number returned 
              ;; by v4l2-reader-get-frame-data
              (defv4l2 v4l2-reader-get-frame 
                (_fun _v4l2-reader-pointer
                      (size : (_ptr o _int))
                      (framenum : (_ptr o _int))
                      (index : (_ptr o _int))
                      -> (r : _pointer)
                      -> (values (cast r _pointer (_bytes o size)) framenum size index)))
              
              ;; requeue the buffer into the memory mapping queue once the downstream
              ;; consumers are done with its data
              (defv4l2 v4l2-reader-enqueue-buffer
                (_fun _v4l2-reader-pointer _int -> _bool))
              
              (define (video-reader-get-params v)
                (define-values (w h num den buffct) (v4l2-reader-get-params v))
                (VideoParams/new w h num den))
              
              (define (video-reader-get-frame v ts)
                (define-values (data framenum size index) (v4l2-reader-get-frame v))
                (FrameBuffer/new data size (λ () (v4l2-reader-enqueue-buffer v index)) ts))
              
              (provide video-reader-get-frame
                       video-reader-get-params
                       (rename-out (v4l2-reader-setup video-reader-setup)
                                   (v4l2-reader-is-ready video-reader-is-ready?)
                                   (v4l2-reader-delete video-reader-delete))))