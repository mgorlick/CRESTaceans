#lang racket/base

(require "util.rkt"
         "structs.rkt"
         "vorbisdec-private.rkt"
         "../bindings/vorbis/libvorbis.rkt"
         racket/contract
         racket/function
         racket/match)
(provide make-vorbis-decoder)

;; Vorbis decoder component
(define/contract (make-vorbis-decoder signaller [localstate (make-vdec-state)])
  ([thread?] [vdec-state?] . ->* . (-> void))
  
  (define vdec (vorbisdec-new))
  (define is-signaller? (make-thread-id-verifier signaller))
  
  (when (reinitialize? localstate) (reinitialize! localstate (curry header-packet! vdec)))
  (λ ()
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? bytes? packet) (handle-vorbis-buffer! vdec localstate packet (bytes-length packet))
                           (loop)]
        [(FrameBuffer packet size λdisposal)
         (handle-vorbis-buffer! vdec localstate packet size)
         (λdisposal)
         (loop)]
        [(? die? sig) (vorbisdec-delete vdec)
                      ;; Don't need to worry about packets in mailbox:
                      ;; Assume downstream producer has not done any more useful
                      ;; work after forwarding die signal
                      (reply/state-report signaller localstate)]))))

(define/contract (handle-vorbis-buffer! vdec localstate buffer len)
  (vorbisdec-pointer? vdec-state? bytes? exact-nonnegative-integer? . -> . void)
  (match* ((packet-type buffer len) (vorbisdec-is-init vdec))
    ;; "normal" states: non-empty packet, dec initialized with headers before processing data
    [('data #t) (data-packet! vdec localstate buffer len)]
    [('header #f) (header-packet! vdec localstate buffer len)]
    ;; the nasty fatal state: can't recover from missing header
    [('empty #f) (fail "fatal: found an empty packet where a header packet was expected")]
    ;; we can skip the state transition for anything else
    [(_ _) (void)]))

(define/contract (packet-type buffer len)
  (bytes? exact-nonnegative-integer? . -> . symbol?)
  (cond [(zero? len) 'empty]
        [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
        [else 'data]))

(define/contract (header-packet! vdec localstate buffer len)
  (vorbisdec-pointer? vdec-state? bytes? exact-nonnegative-integer? . -> . void)
  (let ([r (header-packet-in vdec buffer len)])
    (cond
      [(exact-nonnegative-integer? r)
       (handle-headerpkt! localstate buffer len r (stream-rate vdec) (stream-channels vdec))]
      [else (fail "fatal: expected a header, but couldn't process it")])))

(define/contract (data-packet! vdec localstate buffer len)
  (vorbisdec-pointer? vdec-state? bytes? exact-nonnegative-integer? . -> . void)
  (data-packet-blockin vdec buffer len))

(define (fail str)
  (raise (make-exn:fail str (current-continuation-marks))))
