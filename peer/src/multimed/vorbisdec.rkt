#lang racket

(require "util.rkt"
         "aoout.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         (planet bzlib/thread:1:0))
(provide vorbis-decode)

;; Vorbis decoder component

(struct vdec-state (complete? headers) #:mutable)

(define/contract (vorbis-decode parent [localstate (make-vdec-state)] [vdec (vorbisdec-new)] [sinks #f])
  ([thread?] [vdec-state? vorbisdec-pointer? (or/c #f (listof thread?))] . ->* .  void)
  
  (when (reinitialize? vdec localstate) (reinitialize! vdec localstate))
  
  (let loop ()
    (receive/match
     [(list (? thread? thd) 'clone-state-and-die)
      (to-all parent <- 'state-report localstate)]
     
     [(list (? thread? thd) (? bytes? buffer) (? integer? len))
      (match (handle-vorbis-buffer! vdec localstate buffer len)
        ['ok (loop)]
        ['fatal #f])])))

(define (packet-type buffer len)
  (cond [(zero? len) 'empty]
        [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
        [else 'data]))

(define (handle-vorbis-buffer! vdec localstate buffer len)
  (match* ((packet-type buffer len) (vorbisdec-is-init vdec))
    [('empty #f) (printf "fatal error: empty header~n") 'fatal] ; empty header is fatal
    [('header #f) (header-packet! buffer len vdec localstate)] ; non-empty header
    [('data #f) 'ok] ; data packet received before initialization finished. skip
    [('empty #t) 'ok] ; empty data packet, but headers ok. just skip
    [('header #t) 'ok] ; looks like a header but we've initialized. skip
    [('data #t) (data-packet! buffer len vdec)] ; non-empty data
    ))

(define (header-packet! buffer len vdec localstate)
  (match (header-packet-in vdec (bytestring->uchar** buffer) len)
    [(? (λ (i) (and (>= i 0) (< i 3))) n)
     (printf "valid header packet: type = ~a~n" n)
     (unless (complete-header-packets? localstate) 
       (printf "storing packet~n")
       (store-packet! localstate n buffer len))
     'ok]
    [any (printf "fatal error in decoding header: ~a~n" any) 'fatal]))

(define (data-packet! buffer len vdec)
  (let* ([ct (data-packet-blockin vdec (bytestring->uchar** buffer) len)])
    (when (> ct 0)
      (let* ([samples (box (make-list ct 0))]
             [sample-ct (data-packet-pcmout vdec samples ct)])
        (audio-out (unbox samples) ct))))
  'ok)

;; implementation details of header tracking

(define (make-vdec-state)
  (vdec-state #f (make-vector 3)))

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