#lang racket

(require "../util.rkt"
         "../prebuffer.rkt"
         "vorbisdec-private.rkt"
         "../../../../bindings/vorbis/libvorbis.rkt")
(provide make-vorbis-decoder)

(define *BUFFER-AHEAD* 20)

;; Vorbis decoder component
(define/contract (make-vorbis-decoder signaller [localstate (make-vdec-state)])
  ([thread?] [vdec-state?] . ->* . (-> void))
  (let* ([vdec (vorbisdec-new)]
         [is-signaller? (make-thread-id-verifier signaller)]
         [proc! (curry handle-vorbis-buffer! vdec localstate)])
    (when (reinitialize? localstate) (reinitialize! localstate (curry header-packet! vdec)))
    (λ ()
      (let loop ([p (make-prebuffer *BUFFER-AHEAD*)])
        (match (receive-killswitch/whatever is-signaller?)
          [(? bytes? packet) (cond [(prebuffer-more? p) (loop (prebuffer-do p packet proc!))]
                                   [else (proc! packet)
                                         (loop p)])]
          [(? die? sig) (vorbisdec-delete vdec)
                        (cleanup! localstate)
                        ;; need to also salvage packets sitting in mailbox here
                        (reply/state-report signaller localstate)])))))

(define/contract (handle-vorbis-buffer! vdec localstate buffer)
  (vorbisdec-pointer? vdec-state? bytes? . -> . void)
  (define len (bytes-length buffer))
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
  (let ([ct (data-packet-blockin vdec buffer len)])
    (cond [(positive? ct)
           (let* ([total-samples (* ct (stream-channels vdec))]
                  [sample-ct (data-packet-pcmout vdec (storage localstate) total-samples)])
             (audio-out! localstate total-samples))]
          [else (void)])))

(define (fail str)
  (raise (make-exn:fail str (current-continuation-marks))))
