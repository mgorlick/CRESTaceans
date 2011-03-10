#lang racket

(require "util.rkt"
         "vorbisdec-private.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         (planet bzlib/thread:1:0))
(provide vorbis-decode)

;; Vorbis decoder component

(define/contract (vorbis-decode parent [localstate (make-vdec-state)] [sinks #f])
  ([thread?] [vdec-state? (or/c #f (listof thread?))] . ->* . void)
  
  (define vdec (vorbisdec-new))
  
  (when (reinitialize? localstate)
    (reinitialize! localstate (curry header-packet! vdec)))
  
  (let loop ([c (sub1 (packetcount localstate))])
    
    ; when all header packets are processed, buffer packets in the mailbox 
    ; before starting data packet processing so that production outpaces consumption
    (when (= c 3) (sleep 1))
    
    (receive/match
     [(list thd buffer len)
      (match (handle-vorbis-buffer! vdec localstate buffer len)
        ['ok (loop (add1 c))]
        ['fatal #f])]
     
     [(list (? thread? thd) 'clone-state-and-die)
      (vorbisdec-delete vdec)
      (cleanup! localstate)
      (to-all parent <- 'state-report localstate)])))

(define/contract (packet-type buffer len)
  (bytes? integer? . -> . symbol?)
  (cond [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
        [(not (zero? len)) 'data]
        [else 'empty]))

(define buffer-process/c (vorbisdec-pointer? vdec-state? bytes? integer? . -> . symbol?))

(define/contract (handle-vorbis-buffer! vdec localstate buffer len)
  buffer-process/c
  (match* ((packet-type buffer len) (vorbisdec-is-init vdec))
    
    ;; "normal" states
    [('data #t) (data-packet! vdec localstate buffer len)] ; non-empty data
    [('header #f) (header-packet! vdec localstate buffer len)] ; non-empty header
    
    ;; states that are not intended, but with varying degrees of fatality
    [('empty #f) (printf "fatal error: empty header~n") 'fatal]
    [('data #f) 'ok] ; data packet received before initialization finished. skip
    [('empty #t) 'ok] ; empty data packet, but headers ok. just skip
    [('header #t) 'ok] ; looks like a header but we've initialized. skip
    ))

(define/contract (header-packet! vdec localstate buffer len)
  buffer-process/c
  (match (header-packet-in vdec (bytestring->uchar** buffer) len)
    [(? (λ (i) (and (>= i 0) (< i 3))) typenum)
     (handle-headerpkt! localstate buffer len typenum (stream-rate vdec) (stream-channels vdec))
     'ok]
    [any (printf "fatal error in decoding header: ~a~n" any) 'fatal]))

(define/contract (data-packet! vdec localstate buffer len)
  buffer-process/c
  (let ([ct (data-packet-blockin vdec (bytestring->uchar** buffer) len)])
    (when (> ct 0)
      (let* ([total-samples (* ct (stream-channels vdec))]
             [sample-ct (data-packet-pcmout vdec (storage localstate) total-samples)])
        (audio-out! localstate total-samples))))
  'ok)