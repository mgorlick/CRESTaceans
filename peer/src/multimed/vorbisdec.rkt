#lang racket

(require "util.rkt"
         "vorbisdec-private.rkt"
         "../../../bindings/vorbis/libvorbis.rkt"
         racket/async-channel
         (planet bzlib/thread:1:0))
(provide vorbis-decode)

;; Vorbis decoder component

(define BUFFER-AHEAD 20)

(define/contract (vorbis-decode parent port [localstate (make-vdec-state)] [sinks #f])
  ([thread? integer?] [vdec-state? (or/c #f (listof thread?))] . ->* . void)
  
  (define vdec (vorbisdec-new))
  (when (reinitialize? localstate) (reinitialize! localstate (curry header-packet! vdec)))
  
  ;; warning: hack follows: not totally gross, but getting there.
  ;; the problem we're trying to solve here is that, if you let the decoder start decoding too
  ;; soon after you start to receive packets, you'll probably get a situation where the
  ;; decoder processes packets right after they are stuffed into the channel.
  
  ;; this is *bad*, because it means that there'll be audible time delays between decoding 
  ;; packets, since decoding a packet is faster than the I/O of receiving it through the port,
  ;; and playback happens asynchronously in another pthread, so the decoder will happily march along
  ;; queuing decoded packets into that pthread.
  
  ;; all of the ways to solve this without delaying the start of playback are (AFAIK) unsound,
  ;; and amount to starving the receiving thread in terms of scheduled time: exactly what you *don't* want to do
  ;; (e.g., by looping and checking [unprocessed-packet-count > some-constant] before consuming a packet).
  ;; (exercise for the reader: convince yourself that this is true.)
  
  ;; ideally, we would be able to suspend the consuming thread and resume it whenever the 
  ;; aforementioned condition is true, but (AFAICT) Racket doesn't support making syncable events
  ;; out of arbitrary predicate expressions, and using a condition variable would be suboptimal because
  ;; it means that the packet producer needs to follow the consumption progress and signal
  ;; every time the associated condition is true, which implies sychronization and, therefore,
  ;; further starvation of the actual packet-producing process.
  
  ;; having tried the following:
  ;; solution #1 - async channels with no prebuffering before playback starts
  ;; solution #2 - #1, but with prebuffering of size N
  ;; solution #3 - imperative queues protected by semaphore
  ;; solution #4 - #3, but with an extra sema whose internal counter is equal to the unprocessed packet count
  
  ;; the only one which seems to result in acceptable playback quality is #2. (in general, adding
  ;; the packets to a non-thread-safe imperative queue was a bad idea because it was implicitly synchronous. duh.)
  ;; complication: N may change from machine to machine, or connection to connection. you don't
  ;; *appear* to need to prebuffer again once the decoding starts, but this may change.
  
  ;; this implementation uses counted semaphores to signal when the prebuffering is complete (as opposed to checking
  ;; a mutable global counter) because waking a thread up and putting it back to sleep is faster
  ;; than checking a counter and looping (i.e., starves the producer thread less during initial prebuffering).
  
  (define ch (make-async-channel))
  (define qct (make-semaphore 0))
  
  (define-values (sock receive-packets)
    (let ([sock (let ([s (udp-open-socket)]) (udp-bind! s #f port) s)]
          [udp-buffer (make-bytes 10000)])
      (values sock 
              (let ([c 0])
                (λ ()
                  (let-values ([(len addr port) (udp-receive! sock udp-buffer)])
                    (async-channel-put ch (subbytes udp-buffer 0 len))
                    (signal/count qct (c : (< c BUFFER-AHEAD)))
                    (receive-packets)))))))
  
  (define (decode-packets)
    (let ([buffer (async-channel-get ch)])
      (match (handle-vorbis-buffer! vdec localstate buffer (bytes-length buffer))
        ['ok (decode-packets)]
        ['fatal #f])))
  
  (define receiver-thread (thread receive-packets))
  (define decoder-thread (thread (λ () 
                                   (signal-wait/count qct BUFFER-AHEAD)
                                   (decode-packets))))
  
  (receive/match
   [(list (? thread? thd) 'clone-state-and-die)
    (kill-thread receiver-thread)
    (kill-thread decoder-thread)
    ; in the future: salvage the packets sitting in the channel and in the udp buffer,
    ; send them along with the localstate
    (udp-close sock)
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
    
    ;; "normal" states: non-empty, initialized with headers before processing data
    [('data #t) (data-packet! vdec localstate buffer len)]
    [('header #f) (header-packet! vdec localstate buffer len)]
    
    ;; the nasty fatal state: can't recover from missing header
    [('empty #f) (printf "fatal error: empty header~n") 'fatal]
    
    ;; we can skip the state transition associated with a packet that causes one of these. 
    [('data #f) 'ok]
    [('header #t) 'ok]
    [('empty #t) 'ok]
    ))

(define/contract (header-packet! vdec localstate buffer len) buffer-process/c
  (match (header-packet-in vdec (bytestring->uchar** buffer) len)
    [(? (λ (i) (and (>= i 0) (< i 3))) typenum)
     (printf "header packet successful~n")
     (handle-headerpkt! localstate buffer len typenum (stream-rate vdec) (stream-channels vdec))
     'ok]
    [any 'fatal]))

(define/contract (data-packet! vdec localstate buffer len) buffer-process/c
  (let ([ct (data-packet-blockin vdec (bytestring->uchar** buffer) len)])
    (cond [(positive? ct)
           (let* ([total-samples (* ct (stream-channels vdec))]
                  [output-buffer (storage localstate)]
                  [sample-ct (data-packet-pcmout vdec output-buffer total-samples)])
             (audio-out! localstate total-samples))
           'ok]
          [(zero? ct) 'ok]
          [(negative? ct) 'fatal])))