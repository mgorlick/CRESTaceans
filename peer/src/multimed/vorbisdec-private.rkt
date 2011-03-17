#lang racket

(require "aoout.rkt")

(provide make-vdec-state
         vdec-state?
         storage
         packetcount
         handle-headerpkt!
         audio-out!
         reinitialize?
         reinitialize!
         cleanup!)

;; implementation details of header tracking, sample buffer and audio output device storage/use
(struct vdec-state (headers storage device) #:mutable)

(define (make-vdec-state)
  (vdec-state (make-vector 3) (box (make-list 10000 0)) #f))

(define storage vdec-state-storage)

(struct headerpkt (buffer len))

(define (ref localstate n) (vector-ref (vdec-state-headers localstate) n))
(define (vdec-info-packet localstate) (ref localstate 0))
(define (vdec-comment-packet localstate) (ref localstate 1))
(define (vdec-codebook-packet localstate) (ref localstate 2))

(define (store-packet! localstate typenum buffer len)
  (vector-set! (vdec-state-headers localstate) typenum (headerpkt buffer len)))

(define (complete? localstate)
  (= 3 (packetcount localstate)))

(define (packetcount localstate)
  (stream-length (stream-filter headerpkt? (vdec-state-headers localstate))))

(define (store-device! localstate rate channels endianness)
  (set-vdec-state-device! localstate (make-device rate channels endianness)))

(define (handle-headerpkt! localstate buffer len typenum rate channels)
  (unless (complete? localstate) 
    (store-packet! localstate typenum buffer len))
  (when (= typenum 2) 
    (store-device! localstate rate channels 'native)))

(define (audio-out! localstate total-samples)
  (audio-out (vdec-state-device localstate) 
             (unbox (vdec-state-storage localstate)) 
             total-samples))

; reinitialize the local state for the decoding thread if any header packets
; are present. VERY IMPORTANT ASSUMPTION: no header packets get dropped 
; between pausing the decoder and restarting it elsewhere.
; Assumption can only be violated when total packet production count < 3
; at the point of pausing the packet producer
(define (reinitialize? localstate)
  (> (packetcount localstate) 0))

; ASSUMPTION: header packets weren't stored out of order
; (since lower layer should signal if this property were violated)
(define/contract (reinitialize! localstate header-packet!)
  (vdec-state? (vdec-state? bytes? integer? . -> . void) . -> . void)
  (set-vdec-state-storage! localstate (box (make-list 10000 0)))
  (let ([ct (packetcount localstate)]
        [info (vdec-info-packet localstate)]
        [comment (vdec-comment-packet localstate)]
        [codebook (vdec-codebook-packet localstate)])
    (when (> ct 0)
      (header-packet! localstate (headerpkt-buffer info) (headerpkt-len info))
      (when (> ct 1)
        (header-packet! localstate (headerpkt-buffer comment) (headerpkt-len comment))
        (when (> ct 2)
          (header-packet! localstate (headerpkt-buffer codebook) (headerpkt-len codebook))))))
  (void))

; cleanup! should only be called when the state is about to migrate
(define (cleanup! localstate)
  (when (vdec-state-device localstate) (close-device (vdec-state-device localstate)))
  (set-vdec-state-storage! localstate #f)
  (set-vdec-state-device! localstate #f))