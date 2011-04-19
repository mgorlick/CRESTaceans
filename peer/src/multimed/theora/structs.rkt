#lang typed/racket

(provide (all-defined-out))

;; a disposal is a thunk which 'disposes' of the frame data
;; contract: a downstream consumer must execute λdisposal thunk
;; when it is the last consumer in the chain to use the buffer
;; and the VideoFrameBuffer manufacturer guarantees that executing λdisposal
;; will eventually requeue the buffer for later reuse
(struct: VideoFrameBuffer ([data : Bytes]
                      [framenum : Exact-Nonnegative-Integer]
                      [λdisposal : (-> Void)]))

; hack. untyped modules get confused when trying to refer to
; the data constructor aliased to the type name
(define make-VideoFrameBuffer VideoFrameBuffer)