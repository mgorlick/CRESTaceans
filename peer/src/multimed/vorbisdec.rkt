#! /usr/bin/env racket
#lang racket

(require "util.rkt"
         "udp-source.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         (planet bzlib/thread:1:0))

(define (go)
  (define pid (current-thread))
  (launch-threads ([t2 (udp-source 44000 pid pid)])
                  (vorbis-decode #f)))

;; Vorbis decoder component

(define vorbis-decode
  (case-lambda
    [(parent) (vorbis-decode (vorbisdec-new) parent)]
    [(vdec parent)
     (receive/match
      [(list (? thread? thd) (? bytes? buffer) (? integer? len))
       (match (handle-vorbis-buffer! buffer vdec len)
         ['ok (vorbis-decode vdec parent)]
         ['fatal (printf "fatal error~n") #f]
         ['done #t])])]))

(define (packet-type buffer len)
  (cond [(zero? len) 'empty]
        [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
        [else 'data]))

(define (handle-vorbis-buffer! buffer vdec len)
  (match (cons (packet-type buffer len) (vorbisdec-is-init vdec))
    [(cons 'empty #f) 'fatal] ; empty header is fatal
    [(cons 'header #f) (header-packet! buffer len vdec)] ; non-empty header
    [(cons 'data #f) 'ok] ; data packet received before initialization finished. skip
    [(cons 'empty #t) 'ok] ; empty data packet, but headers ok. just skip
    [(cons 'header #t) 'ok] ; looks like a header but we've initialized. skip
    [(cons 'data #t) (data-packet! buffer len vdec)] ; non-empty data
    ))

(define (bytestring->uchar** buffer)
  (box (bytes->list buffer)))

(define (header-packet! buffer len vdec)
  (match (header-packet-in vdec (bytestring->uchar** buffer) len)
    [(or 1 3 5) 'ok]
    [_ 'fatal]))

(define (data-packet! buffer len vdec)
  (let* ([ct (data-packet-blockin vdec (bytestring->uchar** buffer) len)])
    (printf "ct = ~a~n" ct)
    (when (> ct 0)
      (let* ([storage (box (make-list ct 0.0))]
             [read-count (data-packet-pcmout vdec storage ct)])
        (when (= read-count 128) (printf "~a~n" storage))
        )))
    'ok)

(define pipeline (go))