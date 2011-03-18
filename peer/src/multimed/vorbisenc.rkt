#lang racket

(require "../../../bindings/vorbis/libvorbis.rkt"
         "util.rkt")

(provide (all-defined-out)) 

(struct encoder-settings (channels rate quality fl))

;; component setup
(define/contract (make-vorbis-encoder signaller setup receiver)
  (thread? encoder-settings? thread? . -> . (-> void))
  (let ([enc (vorbisenc-new (encoder-settings-channels setup) (encoder-settings-rate setup) (encoder-settings-quality setup))]
        [output-packet (make-packet-out-callback receiver)]
        [is-signaller? (make-thread-id-verifier signaller)])
    (λ ()
      (vorbisenc-init enc output-packet)
      (let loop ()
        (match (receive-killswitch/whatever is-signaller?)
          [(? die? sig) (vorbisenc-delete enc)
                        (reply/state-report signaller setup)
                        (command/killswitch signaller receiver)]
          [(? bytes? buffer) (vorbisenc-encode-pcm-samples enc buffer (encoder-settings-fl setup) output-packet)
                             (loop)])))))

;; encoder stuff
(define (make-packet-out-callback receiver)
  (thread? . -> . (ogg-packet-pointer? symbol? . -> . boolean?))
  (λ (packet type)
    (thread-send receiver (ogg-packet-data packet))
    #t))