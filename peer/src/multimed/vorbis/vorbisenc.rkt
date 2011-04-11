#lang racket

(require "../../../../bindings/vorbis/libvorbis.rkt"
         "../util.rkt")

(provide (all-defined-out))

(struct encoder-settings (channels rate quality fl))

;; component setup
(define/contract (make-vorbis-encoder signaller setup receiver)
  (thread? encoder-settings? thread? . -> . (-> void))
  (let* ([output-packet (make-packet-out-callback receiver)]
         [enc (vorbisenc-init (encoder-settings-channels setup) (encoder-settings-rate setup) (encoder-settings-quality setup) output-packet)]
         [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (let loop ()
        (match (receive-killswitch/whatever is-signaller?)
          [(? die? sig) (vorbisenc-delete enc)
                        (reply/state-report signaller setup)
                        (command/killswitch signaller receiver)]
          [(? bytes? buffer) (vorbisenc-encode-pcm-samples enc buffer (encoder-settings-fl setup) output-packet)
                             (loop)])))))

;; encoder stuff
(define/contract (make-packet-out-callback receiver)
  (thread? . -> . (ogg-packet-pointer? symbol? . -> . boolean?))
  (λ (packet type)
    (thread-send receiver (bytes-copy (ogg-packet-data packet)))
    #t))