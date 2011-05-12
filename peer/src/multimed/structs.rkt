#lang typed/racket

(provide (all-defined-out))

;; a disposal is a thunk which 'disposes' of the frame data
;; contract: a downstream consumer must execute λdisposal thunk
;; when it is the last consumer in the chain to use the buffer
;; and the FrameBuffer manufacturer guarantees that executing λdisposal
;; will eventually requeue the buffer for later reuse
(struct: FrameBuffer
         ; we'd like to be able to say (a) FrameBuffer
         ; to let the framenum be whatever, but then we couldn't
         ; match against it in untyped code
         ([data : Bytes]
          [size : Natural]
          [λdisposal : (-> Void)]))

; hack. untyped modules get confused when trying to refer to
; the data constructor aliased to the type name
(define make-FrameBuffer FrameBuffer)

(define voidthunk (λ () (void)))