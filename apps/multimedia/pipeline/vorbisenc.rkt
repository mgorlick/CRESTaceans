#lang racket/base

(require "../bindings/vorbis/libvorbis.rkt"
         "util.rkt"
         "structs.rkt"
         racket/contract
         racket/match)

(provide (all-defined-out))

(struct encoder-settings (channels rate quality fl))

;; component setup
(define/contract (make-vorbis-encoder signaller setup receiver)
  (thread? encoder-settings? thread? . -> . (-> void))
  
  (define is-signaller? (make-thread-id-verifier signaller))
  (define output-packet (make-packet-out-callback receiver))
  (define enc (vorbisenc-init (encoder-settings-channels setup) 
                              (encoder-settings-rate setup)
                              (encoder-settings-quality setup)
                              output-packet))
  (λ ()
    (let loop ()
      (match (receive-killswitch/whatever is-signaller?)
        [(? die? sig) (vorbisenc-delete enc)
                      (reply/state-report signaller setup)
                      (command/killswitch signaller receiver)]
        [(? bytes? buffer)
         (vorbisenc-encode-pcm-samples enc buffer (bytes-length buffer)
                                       (encoder-settings-fl setup) output-packet)
         (loop)]
        [(FrameBuffer buffer size λdisposal)
         (vorbisenc-encode-pcm-samples enc buffer size (encoder-settings-fl setup) output-packet)
         (λdisposal)
         (loop)]))))

;; encoder stuff
(define/contract (make-packet-out-callback receiver)
  (thread? . -> . (ogg-packet-pointer? symbol? . -> . boolean?))
  (λ (packet type)
    (thread-send receiver (bytes-copy (ogg-packet-data packet)))
    #t))