#lang racket

(provide make-vdec-state
         vdec-state?
         packetcount
         handle-headerpkt!
         reinitialize?
         reinitialize!)

;; implementation details of header tracking storage/use
(struct vdec-state (headers) #:mutable)

(define (make-vdec-state)
  (vdec-state (make-vector 3)))

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
  (vector-length (vector-filter headerpkt? (vdec-state-headers localstate))))

(define (handle-headerpkt! localstate buffer len typenum rate channels)
  (unless (complete? localstate) 
    (store-packet! localstate typenum buffer len)))

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