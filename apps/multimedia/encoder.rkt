#! /usr/bin/env racket
#lang racket/base

(require "pipeline/v4l2-reader.rkt"
         "pipeline/vp8enc.rkt"
         "pipeline/vorbisenc.rkt"
         "pipeline/pulsesrc.rkt"
         "pipeline/structs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/net/structs.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match)

(define me (current-thread))

(define *RKEY*
  (with-handlers ([exn:fail? (λ (e) (printf "NO KEY SPECIFIED!~n") #f)])
    (string->bytes/utf-8 (vector-ref (current-command-line-arguments) 0))))

(define port
  (with-handlers ([exn:fail? (λ (e) 5000)])
    (string->number (vector-ref (current-command-line-arguments) 1))))

(define processor
  (thread
   (λ ()
     (let loop ()
       (printf "new CURL: ~s~n" (start-program (:message/ask/body (thread-receive))))
       (loop)))))

(define *LISTENING-ON* *LOCALHOST*)
(define *RHOST* *LOCALHOST*);"128.195.58.146")
(define *RPORT* 1235)

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LISTENING-ON* port #:key k))
(define request-thread (run-tcp-peer *LISTENING-ON* port this-scurl processor))

(define the-key-in-this-scurl
  (path/param-path (list-ref (url-path (string->url (scurl->string this-scurl))) 1)))

(define (relayer targeturl)
  (let loop ()
    (match (thread-receive)
      [(FrameBuffer buffer len disp ts)
       (ask/send "POST" request-thread *RHOST* *RPORT* *RKEY*
                 `(FrameBuffer ,(subbytes buffer 0 len) ,len #f ,ts)
                 #:url targeturl)
       (disp)
       (loop)]
      [(? bytes? buffer)
       (ask/send "POST" request-thread *RHOST* *RPORT* *RKEY*
                 `(FrameBuffer ,buffer ,(bytes-length buffer) void 0)
                 #:url targeturl)
       (loop)])))

(define video-decoder
  '(let ([src/decoder
          (lambda ()
            (let ([d (vp8dec-new)])
              (let loop ([v (thread-receive)])
                (printf "packet is ~a ms old~n" (FrameBuffer-age v))
                (vp8dec-decode d (FrameBuffer-size v) (FrameBuffer-data v))
                (loop (thread-receive)))))])
     (src/decoder)))

(define audio-decoder
  '(let ([src/decoder
          (lambda ()
            (let* ([dec (vorbisdec-new)]
                   [packet-type (lambda (buffer len)
                                  (cond [(zero? len) 'empty]
                                        [(= 1 (bitwise-and 1 (bytes-ref buffer 0))) 'header]
                                        [else 'data]))]
                   [handle-buffer (lambda (buffer len)
                                    (cond [(not (vorbisdec-is-init dec))
                                           (cond [(equal? (packet-type buffer len) 'header)
                                                  (header-packet-in dec buffer len)]
                                                 [else
                                                  (printf "error: non-header received when decoder uninitialized~n")
                                                  #f])]
                                          [else
                                           (and (equal? (packet-type buffer) 'data)
                                                (data-packet-blockin dec buffer len))]))])
              (let loop ([v (thread-receive)])
                (printf "packet is ~a ms old~n" (FrameBuffer-age v))
                (if (handle-buffer (FrameBuffer-data v) (FrameBuffer-size v))
                    (loop (thread-receive))
                    #f))))])
     (src/decoder)))

(cond [(equal? port 5000)
       (ask/send "SPAWN" request-thread *RHOST* *RPORT* *RKEY* video-decoder
                 #:url "/" #:metadata '(("accepts" . "video/webm")))
       (define videorelay0 (thread (λ () (relayer "/video0"))))
       (define vp80 (thread (make-vp8-encoder me videorelay0)))
       (define video0 (thread (make-v4l2-reader me vp80)))
       (no-return)]
      
      [else
       (ask/send "SPAWN" request-thread *RHOST* *RPORT* *RKEY* audio-decoder
                 #:url "/" #:metadata '(("accepts" . "audio/webm")))
       (define audiorelay0 (thread (λ () (relayer "/audio0"))))
       (define vorbis0 (thread (make-vorbis-encoder me (encoder-settings 2 44100 1.0 'naive) audiorelay0)))
       (define pulse0 (thread (make-pulsesrc me (pulse-settings 2 44100 1024) vorbis0)))
       (no-return)])

(no-return)
