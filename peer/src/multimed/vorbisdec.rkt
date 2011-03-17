#lang racket

(require "util.rkt"
         "vorbisdec-private.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         data/queue
         (planet bzlib/thread:1:0))
(provide make-vorbis-decoder)

(define *BUFFER-AHEAD* 20)

;; Vorbis decoder component
(define/contract (make-vorbis-decoder signaller [localstate (make-vdec-state)])
  ([thread?] [vdec-state?] . ->* . (-> void))
  (let ([vdec (vorbisdec-new)]
        [q (make-queue)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (when (reinitialize? localstate)
        (reinitialize! localstate (curry header-packet! vdec)))
      (let loop ()
        (cond [(receive/killswitch is-signaller?) (vorbisdec-delete vdec)
                                                  (cleanup! localstate)
                                                  ;; need to also salvage packets sitting in mailbox here
                                                  (reply/state-report signaller localstate)]
              [(prebuffer-add?! q) (loop)]
              [(prebuffer-decode?! q vdec localstate) (loop)]
              [(not (prebuffering? q)) (let ([pkt/? (receive/packet)])
                                         (when pkt/? (handle-vorbis-buffer! vdec localstate pkt/?)))
                                       (loop)]
              [else (loop)])))))

(define/contract (receive/packet)
  (-> bytes?)
  (receive/match [(? bytes? packet) packet]))

(define (prebuffering? q)
  (<= (queue-length q) *BUFFER-AHEAD*))

(define (empty-prebuffer? q)
  (= (queue-length q) *BUFFER-AHEAD*))

(define (more-prebuffer? q)
  (< (queue-length q) *BUFFER-AHEAD*))

(define (prebuffer-add?! q)
  (cond [(more-prebuffer? q)
         (let ([pkt/? (receive/packet)])
           (if pkt/?
               (enqueue! q pkt/?)
               #t))]
        [else #f]))

(define (prebuffer-decode?! q vdec localstate)
  (cond [(empty-prebuffer? q)
         (map (curry handle-vorbis-buffer! vdec localstate) (queue->list q))
         (enqueue! q #"ENDPREBUFFER")
         #t]
        [else #f]))

(define/contract (handle-vorbis-buffer! vdec localstate buffer)
  (vorbisdec-pointer? vdec-state? bytes? . -> . void)
  (define len (bytes-length buffer))
  (match* ((packet-type buffer len) (vorbisdec-is-init vdec))
    ;; "normal" states: non-empty packet, dec initialized with headers before processing data
    [('data #t) (data-packet! vdec localstate buffer len)]
    [('header #f) (header-packet! vdec localstate buffer len)]
    ;; the nasty fatal state: can't recover from missing header
    [('empty #f) (fail "fatal: found an empty packet where a header packet was expected" (current-continuation-marks))]
    ;; we can skip the state transition for anything else
    [(_ _) (void)]))

(define/contract (packet-type buffer len)
  (bytes? exact-nonnegative-integer? . -> . symbol?)
  (cond [(zero? len) 'empty]
        [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
        [else 'data]))

(define/contract (header-packet! vdec localstate buffer len)
  (vorbisdec-pointer? vdec-state? bytes? exact-nonnegative-integer? . -> . void)
  (match (header-packet-in vdec (bytestring->uchar** buffer) len)
    [(? (λ (i) (and (>= i 0) (< i 3))) typenum)
     (handle-headerpkt! localstate buffer len typenum (stream-rate vdec) (stream-channels vdec))]
    [_ (raise (fail "fatal: expected a header, but couldn't process it"))]))

(define (data-packet! vdec localstate buffer len)
  (vorbisdec-pointer? vdec-state? bytes? exact-nonnegative-integer? . -> . void)
  (let ([ct (data-packet-blockin vdec (bytestring->uchar** buffer) len)])
    (cond [(positive? ct)
           (let* ([total-samples (* ct (stream-channels vdec))]
                  [output-buffer (storage localstate)]
                  [sample-ct (data-packet-pcmout vdec output-buffer total-samples)])
             (audio-out! localstate total-samples))]
          [else (void)])))

(define (fail str)
  (raise (make-exn:fail str (current-continuation-marks))))