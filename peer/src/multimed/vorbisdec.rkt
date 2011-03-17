#lang racket

(require "util.rkt"
         "udp.rkt"
         "vorbisdec-private.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         (planet bzlib/thread:1:0))
(provide vorbis-decode)

;; Vorbis decoder component

(define BUFFER-AHEAD 20)

(define/contract (vorbis-decode parent port [localstate (make-vdec-state)])
  ([thread? integer?] [vdec-state?] . ->* . void)
  
  (define vdec (vorbisdec-new))
  (when (reinitialize? localstate) (reinitialize! localstate (curry header-packet! vdec)))
  
  (define decoder-thread (thread (make-decoder vdec localstate)))
  (define receiver-thread (thread (make-udp-reader #f port decoder-thread)))
  
  (receive/match
   [(list (? thread? thd) 'clone-state-and-die)
    (kill-thread receiver-thread)
    (kill-thread decoder-thread)
    ; in the future: salvage the packets sitting in the channel and in the udp buffer,
    ; send them along with the localstate
    (vorbisdec-delete vdec)
    (cleanup! localstate)
    (to-all parent <- 'state-report localstate)]))

(define (make-decoder vdec localstate)
  (λ ()
    ;; do prebuffering
    (let ([buffered-packets (for/list ([i (in-range BUFFER-AHEAD)])
                              (thread-receive))])
      (map (curry handle-vorbis-buffer! vdec localstate) buffered-packets)
      ;; prebuffering done
      (let loop ()
        (let ([buffer (thread-receive)])
          (handle-vorbis-buffer! vdec localstate buffer))
        (loop)))))

(define (handle-vorbis-buffer! vdec localstate buffer)
  (define len (bytes-length buffer))
  (match* ((packet-type buffer len) (vorbisdec-is-init vdec))
    ;; "normal" states: non-empty packet, dec initialized with headers before processing data
    [('data #t) (data-packet! vdec localstate buffer len)]
    [('header #f) (header-packet! vdec localstate buffer len)]
    ;; the nasty fatal state: can't recover from missing header
    [('empty #f) (raise (make-exn:fail "fatal: found an empty packet where a header packet was expected"))]
    ;; we can skip the state transition for anything else
    [(_ _) (void)]))

(define (packet-type buffer len)
  (cond [(zero? len) 'empty]
        [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
        [else 'data]))

(define (header-packet! vdec localstate buffer len)
  (match (header-packet-in vdec (bytestring->uchar** buffer) len)
    [(? (λ (i) (and (>= i 0) (< i 3))) typenum)
     (handle-headerpkt! localstate buffer len typenum (stream-rate vdec) (stream-channels vdec))]
    [_ (raise (make-exn:fail "fatal: packet looked like a header, but couldn't process it"))]))

(define (data-packet! vdec localstate buffer len)
  (let ([ct (data-packet-blockin vdec (bytestring->uchar** buffer) len)])
    (cond [(positive? ct)
           (let* ([total-samples (* ct (stream-channels vdec))]
                  [output-buffer (storage localstate)]
                  [sample-ct (data-packet-pcmout vdec output-buffer total-samples)])
             (audio-out! localstate total-samples))]
          [else (void)])))