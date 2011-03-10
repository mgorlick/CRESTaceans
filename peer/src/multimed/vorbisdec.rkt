#lang racket

(require "util.rkt"
         "aoout.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         (planet bzlib/thread:1:0))
(provide vorbis-decode)

;; Vorbis decoder component

(struct vdec-state (complete? headers storage device) #:mutable)

(define/contract (vorbis-decode parent [localstate (make-vdec-state)] [vdec (vorbisdec-new)] [sinks #f])
  ([thread?] [vdec-state? vorbisdec-pointer? (or/c #f (listof thread?))] . ->* .  void)
  
  (when (reinitialize? vdec localstate) (reinitialize! vdec localstate))
  (let loop ()
    (receive/match
     [(list thd buffer len)
      (match (handle-vorbis-buffer! vdec localstate buffer len)
        ['ok (loop)]
        ['fatal #f])]
     
     [(list (? thread? thd) 'clone-state-and-die)
      (vorbisdec-delete vdec)
      (cleanup! localstate)
      (to-all parent <- 'state-report localstate)])))

(define (packet-type buffer len)
  (cond [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
        [(not (zero? len)) 'data]
        [else 'empty]))

(define (handle-vorbis-buffer! vdec localstate buffer len)
  (match* ((packet-type buffer len) (vorbisdec-is-init vdec))
    
    ;; "normal" states
    [('data #t) (data-packet! buffer len vdec localstate)] ; non-empty data
    [('header #f) (header-packet! buffer len vdec localstate)] ; non-empty header
    
    ;; states that are not intended, but with varying degrees of fatality
    [('empty #f) (printf "fatal error: empty header~n") 'fatal]
    [('data #f) 'ok] ; data packet received before initialization finished. skip
    [('empty #t) 'ok] ; empty data packet, but headers ok. just skip
    [('header #t) 'ok] ; looks like a header but we've initialized. skip
    ))

(define (header-packet! buffer len vdec localstate)
  (match (header-packet-in vdec (bytestring->uchar** buffer) len)
    [(? (λ (i) (and (>= i 0) (< i 3))) n)
     (unless (complete-header-packets? localstate)
       (store-packet! localstate n buffer len))
     (when (= n 2)
       (set-vdec-state-device! localstate (make-device (stream-rate vdec) 
                                                       (stream-channels vdec) 
                                                       'native)))
     'ok]
    [any (printf "fatal error in decoding header: ~a~n" any) 'fatal]))

(define (data-packet! buffer len vdec localstate)
  (let ([ct (data-packet-blockin vdec (bytestring->uchar** buffer) len)])
    (when (> ct 0)
      (let* ([total-samples (* ct (stream-channels vdec))]
             [sample-ct (data-packet-pcmout vdec (vdec-state-storage localstate) total-samples)])
        (audio-out (vdec-state-device localstate) 
                   (unbox (vdec-state-storage localstate)) 
                   total-samples))))
  'ok)

;; implementation details of header tracking

(define (make-vdec-state)
  (vdec-state #f (make-vector 3) (box (make-list 10000 0)) #f))

(struct headerpkt (buffer len))

(define (vdec-info-packet localstate)
  (vector-ref (vdec-state-headers localstate) 0))
(define (vdec-comment-packet localstate)
  (vector-ref (vdec-state-headers localstate) 1))
(define (vdec-codebook-packet localstate)
  (vector-ref (vdec-state-headers localstate) 2))

(define (store-packet! localstate typenum buffer len)
  (vector-set! (vdec-state-headers localstate) typenum (headerpkt buffer len))
  (when (stream-andmap headerpkt? (vdec-state-headers localstate))
    (set-vdec-state-complete?! localstate #t)))

(define (complete-header-packets? localstate)
  (vdec-state-complete? localstate))

(define (reinitialize? vdec localstate)
  (and (not (vorbisdec-is-init vdec)) (complete-header-packets? localstate)))

(define (reinitialize! vdec localstate)
  (let ([info (vdec-info-packet localstate)]
        [comment (vdec-comment-packet localstate)]
        [codebook (vdec-codebook-packet localstate)])
    (header-packet! (headerpkt-buffer info) (headerpkt-len info) vdec localstate)
    (header-packet! (headerpkt-buffer comment) (headerpkt-len comment) vdec localstate)
    (header-packet! (headerpkt-buffer codebook) (headerpkt-len codebook) vdec localstate)))

(define (cleanup! localstate)
  (close-device (vdec-state-device localstate)))