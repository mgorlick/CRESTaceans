#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(define SERVER-HOST "0.0.0.0")
(define SERVER-PORT "44018")

(define FILE-TO-TRANSFER (string-append (path->string (current-directory)) "sample.ogg"))
(define FILE-SIZE (file-size (string->path FILE-TO-TRANSFER)))

(define (frame-received channel connection frame user-data)
  (let ([in (open-input-file FILE-TO-TRANSFER)])
    (let loop ([total 0])
      (let ([buffer (read-bytes 4096 in)])
        (if (eof-object? buffer)
            (printf "Server sent ~s bytes (should have read ~s bytes)~n" total FILE-SIZE)
            (begin
              (let* ([bytes-read (bytes-length buffer)]
                     [total* (+ bytes-read total)])
                (printf "Server sending ~s bytes in a new frame~n" bytes-read)
                (vortex-channel-send-ans-rpy channel buffer (vortex-frame-get-msgno frame))
                (loop total*)))))
      (close-input-port in)))
  (vortex-channel-finalize-ans-rpy channel (vortex-frame-get-msgno frame))
  )

(define (start-channel num connection user-data) axl-true)
(define (close-channel num connection user-data) axl-true)

(context
 [#f]
 (vortex-profiles-register context Plain-Profile-URI start-channel #f close-channel #f frame-received #f)
 (vortex-listener-new context SERVER-HOST SERVER-PORT #f #f)
 (vortex-listener-wait context)
 )
