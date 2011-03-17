#lang racket

(require "../../../bindings/vorbis/libvorbis.rkt"
         "util.rkt"
         (planet bzlib/thread:1:0))

(provide (all-defined-out)) 

(struct encoder-settings (channels rate quality fl))

;; component setup
(define/contract (make-vorbis-encoder signaller setup receiver)
  (thread? encoder-settings? thread? . -> . (-> void))
  (let ([enc (vorbisenc-new (encoder-settings-channels setup)
                            (encoder-settings-rate setup)
                            (encoder-settings-quality setup))]
        [output-packet (make-packet-out-callback receiver)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (vorbisenc-init enc output-packet)
      (let loop ()
        (cond [(receive/killswitch is-signaller?) (vorbisenc-delete enc)
                                                  (reply/state-report signaller setup)
                                                  (command/killswitch signaller receiver)]
              [else
               (let ([pkt/? (receive/packet)])
                 (when pkt/?
                   (vorbisenc-encode-pcm-samples enc pkt/? (encoder-settings-fl setup) output-packet)))
               (loop)])))))

(define (receive/packet)
  (-> bytes?)
  (receive/match [(? bytes? packet) packet]))

;; encoder stuff
(define (make-packet-out-callback receiver)
  (thread? . -> . (ogg-packet-pointer? symbol? . -> . boolean?))
  (λ (packet type)
    (thread-send receiver (ogg-packet-data packet))
    #t))