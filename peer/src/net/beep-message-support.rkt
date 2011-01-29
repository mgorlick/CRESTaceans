#lang racket

(require "beep-message-typed.rkt")
(require "../../../bindings/vortex/vortex.rkt")

(define (frame->message frame validator)
  (let* ([message (payload->beep-message (vortex-frame-get-payload-bytes frame))]
         [understand? (message-validate/decode!? validator message)])
    (values message understand?)))

(provide (all-from-out "beep-message-typed.rkt")
         (all-defined-out))