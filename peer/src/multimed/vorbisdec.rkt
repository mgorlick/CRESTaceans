#lang racket

(require "util.rkt"
         "udp-source.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         (planet bzlib/thread:1:0)
         )

(define (go)
  (threads ([t1 -> (vorbis-decode)]
            [t2 -> (udp-source 44000 t1)])
           (vector t1 t2)))

;; Vorbis decoder component

(define (bytes->ogg-header-packet buffer len)
  (let ([b-o-s (if (equal? #x1 (bytes-ref buffer 0)) 1 0)])
    (make-ogg-packet buffer len b-o-s 0 -1 0)))

(define (bytes->ogg-data-packet buffer len)
  (make-ogg-packet buffer len 0 0 -1 0))

(struct vorbisdec
  ([initialized? #:mutable]
   [comment #:mutable]
   [info #:mutable]
   [dsp-state #:mutable]
   [block #:mutable]))

(define (new-vorbis-decoder)
  (let* ([vinfo (vorbis-info-new)]
         [vdsp (vorbis-dsp-state-new vinfo)]
         [vblock (vorbis-block-new vdsp)])
    (vorbisdec #t (vorbis-comment-new) vinfo vdsp vblock)))

(define vorbis-decode
  (case-lambda
    [() (vorbis-decode (new-vorbis-decoder) 0)]
    [(vdec ct)
     (receive/match
      [(list (? thread? thd) (? bytes? buffer))
       (let ([res (handle-vorbis-buffer! buffer vdec ct)])
         (match res
           ['ok (vorbis-decode vdec (add1 ct))]
           ['fatal #f]
           ['done #t]))])]))

(define & bitwise-and)

(define/contract (handle-vorbis-buffer! buffer vdec ct)
  (bytes? vorbisdec? integer? . -> . symbol?)
  (let* ([len (bytes-length buffer)]
         [typ (or (zero? len) (& 1 (bytes-ref buffer 0)))]
         [init? (vorbisdec-initialized? vdec)])
    (match (list len typ init?)
      [(list 0 _ #f) ; empty header: FATAL
       'fatal]
      [(list 0 _ #t) ; empty data: not fatal, just skip
       'ok]
      [(list _ 1 #t) ; non-empty header
       (printf "header packet, c = ~a, l = ~a~n" ct len)
       'ok]
      [(list _ 0 #t) ; non-empty data
       (printf "data packet, c = ~a, l = ~a~n" ct len)
       (bytes->ogg-data-packet buffer len)
       'ok])))

(define (handle-vorbis-header-packet! pkt vdec)
  (printf "~a~n" (vorbis-synthesis-headerin (vorbisdec-info vdec)
                                            (vorbisdec-comment vdec) pkt))
  (match (bytes-ref (ogg-packet-packet pkt) 0)
    [#x01 (printf "identification packet~n")]
    [#x03 (printf "comment packet~n")]
    [#x05 (printf "type packet~n")]
    [_ (printf "unknown packet~n")]))

(define pipeline (go))