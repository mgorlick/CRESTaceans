#lang racket

(require "util.rkt"
         "aoout.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         (planet bzlib/thread:1:0))
(provide (all-defined-out))

;; Vorbis decoder component

(define/contract (vorbis-decode parent [vdec #f] [sinks #f])
  ([thread?] [(or/c #f vorbisdec-pointer?) (or/c #f (listof thread?))] . ->* .  void)
  
  (when (not vdec)
    (vorbis-decode parent (vorbisdec-new)))
  
  (receive/match
   [(list (? thread? thd) 'clone-state-and-die)
    (to-all parent <- 'state-report (handle-state-report vdec))]
   
   [(list (? thread? thd) (? bytes? buffer) (? integer? len))
    (match (handle-vorbis-buffer! vdec buffer len)
      ['ok (vorbis-decode parent vdec)]
      ['fatal #f])]
   ))

(define (packet-type buffer len)
  (cond [(zero? len) 'empty]
        [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
        [else 'data]))

(define (handle-vorbis-buffer! vdec buffer len)
  (match* ((packet-type buffer len) (vorbisdec-is-init vdec))
    [('empty #f) (printf "fatal error: empty header~n") 'fatal] ; empty header is fatal
    [('header #f) (header-packet! buffer len vdec)] ; non-empty header
    [('data #f) 'ok] ; data packet received before initialization finished. skip
    [('empty #t) 'ok] ; empty data packet, but headers ok. just skip
    [('header #t) 'ok] ; looks like a header but we've initialized. skip
    [('data #t) (data-packet! buffer len vdec)] ; non-empty data
    ))

(define (bytestring->uchar** buffer)
  (box (bytes->list buffer)))

(define (header-packet! buffer len vdec)
  (match (header-packet-in vdec (bytestring->uchar** buffer) len)
    [(or 1 3 5) 'ok]
    [any (printf "fatal error in decoding header: ~a~n" any) 'fatal]))

(define (data-packet! buffer len vdec)
  (let* ([ct (data-packet-blockin vdec (bytestring->uchar** buffer) len)])
    (when (> ct 0)
      (let* ([samples (box (make-list ct 0))]
             [sample-ct (data-packet-pcmout vdec samples ct)])
        (audio-out (unbox samples) ct)
        )))
  'ok)

(define (handle-state-report vdec)
  (let ([vi (vorbisdec-get-info vdec)]
        [vc (vorbisdec-get-comment vdec)]
        [vd (vorbisdec-get-dsp-state vdec)]
        [vb (vorbisdec-get-block vdec)]
        [init? (vorbisdec-is-init vdec)])
    ;(printf "vi->rate = ~a~n" (vorbis-info-channels vi))
    ;(printf "vc->comments = ~a~n" (vorbis-comment-comments vc))
    ;(printf "vd->W = ~a~n" (vorbis-dsp-state-W vd))
    ;(printf "vb->totaluse = ~a~n" (vorbis-block-totaluse vb))
  vdec))