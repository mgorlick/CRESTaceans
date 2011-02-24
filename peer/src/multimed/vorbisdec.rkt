#! /usr/bin/env racket
#lang racket

(require "util.rkt"
         "udp-source.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         (planet bzlib/thread:1:0)
         ffi/unsafe
         )

(define (go)
  (define pid (current-thread))
  (launch-threads ([t2 (udp-source 44000 pid pid)])
                  (vorbis-decode #f)))

;; Vorbis decoder component

(define (header-packet buffer len)
  (make-ogg-packet buffer len (if (equal? #x1 (bytes-ref buffer 0)) 1 0) 0 -1 0))

(define (data-packet buffer len)
  (define p (make-ogg-packet (make-bytes len 255) len 0 0 -1 0))
  (copy-buffer-to-packet p (box (bytes->list buffer)) len)
  ;(print-buffer p)
  ;(printf "~a~n" buffer)
  p)

(struct vorbisdec
  ([initialized? #:mutable]
   [comment #:mutable]
   [info #:mutable]
   [dsp-state #:mutable]
   [block #:mutable]))

(define (new-vorbis-decoder)
  (vorbisdec #f (vorbis-comment-new) (vorbis-info-new) #f #f))

(define vorbis-decode
  (case-lambda
    [(parent) (vorbis-decode (new-vorbis-decoder) parent)]
    [(vdec parent)
     (receive/match
      [(list (? thread? thd) (? bytes? buffer) (? integer? len))
       (match (handle-vorbis-buffer! buffer vdec len)
         ['ok (vorbis-decode vdec parent)]
         ['fatal (printf "fatal error~n") #f]
         ['done #t])])]))

(define (handle-vorbis-buffer! buffer vdec len)
  (let* ([typ (if (zero? len) 'empty (if (= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header 'data))])
    (match (cons typ (vorbisdec-initialized? vdec))
      [(cons 'empty #f) 'fatal] ; empty header is fatal
      [(cons 'header #f) (header-packet! (bytes-ref buffer 0) (header-packet buffer len) vdec)] ; non-empty header
      [(cons 'data #f) 'ok] ; data packet received before header initialization finished. skip
      [(cons 'empty #t) 'ok] ; empty da ta packet, but headers ok (since already initialized). just skip
      [(cons 'header #t) 'ok] ; looks like a header but we've initialized already? whatever. throw it away
      [(cons 'data #t) (data-packet! (data-packet buffer len) vdec)] ; non-empty data
      )))

(define (header-packet! firstbyte pkt vdec)
  (match firstbyte
    [#x01 (printf "identification packet~n")
          (if (= (vorbis-synthesis-idheader pkt) 1) 
              (begin (headerin pkt vdec)
                     'ok)
              'fatal)]
    [#x03 (printf "comment packet~n")
          (headerin pkt vdec)
          'ok]
    [#x05 (printf "type packet~n")
          (headerin pkt vdec)
          (type-packet! pkt vdec)]
    [_ (printf "unknown packet~n") 'ok]))

(define (headerin pkt vdec)
  (vorbis-synthesis-headerin (vorbisdec-info vdec) (vorbisdec-comment vdec) pkt))

(define (type-packet! pkt vdec)
  (let* ([dsp-state (vorbis-dsp-state-new (vorbisdec-info vdec))]
         [block (vorbis-block-new dsp-state)])
    (cond
      [(and (not (false? dsp-state)) (not (false? block)))
       (set-vorbisdec-dsp-state! vdec dsp-state)
       (set-vorbisdec-block! vdec block)
       (set-vorbisdec-initialized?! vdec #t)
       (printf "got all necessary header packets~n")
       'ok]
      [else 'fatal])))

(define (data-packet! pkt vdec)
  (define block (vorbisdec-block vdec))
  (define dsp-state (vorbisdec-dsp-state vdec))
  (define channels (vorbis-info-channels (vorbisdec-info vdec)))
  (vorbis-synthesis block pkt)
  (vorbis-synthesis-blockin dsp-state block)
  (let ([count (vorbis-synthesis-pcmout-countonly dsp-state)])
    (when (> count 0)
      (define storage (box (make-vector count 0.0)))
      (let-values ([(count*) (pcmout-wrapper dsp-state storage count channels)])
        ;(pcmout-wrapper dsp-state storage count channels)])
        (vorbis-synthesis-read dsp-state count*)
        )))
  
  'ok)

(define pipeline (go))