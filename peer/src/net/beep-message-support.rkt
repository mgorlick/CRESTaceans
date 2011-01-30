#lang typed/racket

(require "beep-message-typed.rkt")
(require/typed "../../../bindings/vortex/vortex.rkt"
               [opaque VortexFrame VortexFrame*?]
               [vortex-frame-get-payload-bytes (VortexFrame -> Bytes)]
               [vortex-frame-get-payload-string (VortexFrame -> String)])

(: frame->message (VortexFrame Mac-Validator -> (values beep-message Boolean)))
(define (frame->message frame validator)
  (let* ([message (payload->beep-message (vortex-frame-get-payload-bytes frame))]
         [understand? (message-validate/decode!? validator message)])
    (values message understand?)))

(provide (all-from-out "beep-message-typed.rkt")
         (except-out (all-defined-out)
                     VortexFrame*?
                     vortex-frame-get-payload-bytes
                     vortex-frame-get-payload-string))