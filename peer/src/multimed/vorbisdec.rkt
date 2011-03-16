#lang racket

(require "util.rkt"
         "vorbisdec-private.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         racket/async-channel
         (planet bzlib/thread:1:0))
(provide vorbis-decode)

;; Vorbis decoder component

(define BUFFER-AHEAD 0)

(define/contract (vorbis-decode parent port [localstate (make-vdec-state)])
  ([thread? integer?] [vdec-state?] . ->* . void)
  
  (define vdec (vorbisdec-new))
  (when (reinitialize? localstate) (reinitialize! localstate (curry header-packet! vdec)))
  
  ;; the problem we're trying to solve here is that, if you let the decoder start decoding too
  ;; soon after you start to receive packets, you'll probably get a situation where the
  ;; decoder processes packets right after they are stuffed into the channel.
  
  ;; this is *bad*, because it means that there'll be audible time delays between decoding 
  ;; packets, since decoding a packet is faster than the I/O of receiving it through the port,
  ;; and playback happens asynchronously in another pthread, so the decoder will happily march along
  ;; queueing decoded packets into that pthread.
  
  ;; instead, we just prebuffer N packets before the start of decoding.
  ;; complication: N may change from machine to machine, or connection to connection. you don't
  ;; *appear* to need to prebuffer again once the decoding starts, but this may change.
  
  ;; this implementation uses counted semaphores to signal when the prebuffering is complete (as opposed to checking
  ;; a mutable global counter) because waking a thread up and putting it back to sleep is faster
  ;; than checking a counter and looping (i.e., starves the producer thread less during initial prebuffering).
  
  (define qct (make-semaphore 0))
  
  (define receive-packets
    (let ([sock (let ([s (udp-open-socket)]) (udp-bind! s #f port) s)]
          [udp-buffer (make-bytes 10000)]
          [c 0])
      (λ (decoder)
        (let-values ([(len addr port) (udp-receive! sock udp-buffer)])
          ;(printf "a packet came in (len ~a)~n" len)
          (thread-send decoder (subbytes udp-buffer 0 len))
          (signal/count qct (c : (< c BUFFER-AHEAD)))
          (receive-packets decoder)))))
  
  (define (decode-packets)
    (let ([buffer (thread-receive)])
      (cond [(bytes? buffer)
             (match (handle-vorbis-buffer! vdec localstate buffer (bytes-length buffer))
               ['ok (decode-packets)]
               ['fatal #f])])))
  
  (define decoder-thread (thread (λ () (signal-wait/count qct BUFFER-AHEAD) (decode-packets))))
  (define receiver-thread (thread (λ () (receive-packets decoder-thread))))
  
  (receive/match
   [(list (? thread? thd) 'clone-state-and-die)
    (kill-thread receiver-thread)
    (kill-thread decoder-thread)
    ; in the future: salvage the packets sitting in the channel and in the udp buffer,
    ; send them along with the localstate
    (vorbisdec-delete vdec)
    (cleanup! localstate)
    (to-all parent <- 'state-report localstate)]))

(define buffer-process/c (vorbisdec-pointer? vdec-state? bytes? integer? . -> . symbol?))

(define/contract (packet-type buffer len)
  (bytes? integer? . -> . symbol?)
  (cond [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
        [(not (zero? len)) 'data]
        [else 'empty]))

(define/contract (handle-vorbis-buffer! vdec localstate buffer len) buffer-process/c
  (match* ((packet-type buffer len) (vorbisdec-is-init vdec))
    ;; "normal" states: non-empty packet, dec initialized with headers before processing data
    [('data #t) (data-packet! vdec localstate buffer len)]
    [('header #f) (header-packet! vdec localstate buffer len)]
    ;; the nasty fatal state: can't recover from missing header
    [('empty #f) (printf "empty/fatal~n") 'fatal]
    ;; we can skip the state transition associated with a packet that causes one of these. 
    [('data #f) (printf "data/skip~n") 'ok]
    [('header #t) (printf "header/skip~n") 'ok]
    [('empty #t) (printf "empty/skip~n") 'ok]))

(define/contract (header-packet! vdec localstate buffer len) buffer-process/c
  (match (header-packet-in vdec (bytestring->uchar** buffer) len)
    [(? (λ (i) (and (>= i 0) (< i 3))) typenum)
     (handle-headerpkt! localstate buffer len typenum (stream-rate vdec) (stream-channels vdec))
     'ok]
    [any 'fatal]))

(define/contract (data-packet! vdec localstate buffer len) buffer-process/c
  (let ([ct (data-packet-blockin vdec (bytestring->uchar** buffer) len)])
    (cond [(positive? ct)
           (let* ([total-samples (* ct (stream-channels vdec))]
                  [output-buffer (storage localstate)]
                  [sample-ct (data-packet-pcmout vdec output-buffer total-samples)])
             (printf "decoded ~a samples (planned ~a, buffer size ~a)~n" sample-ct total-samples  len)
             (audio-out! localstate total-samples))
           'ok]
          [(zero? ct) 'ok]
          [(negative? ct) 'fatal])))