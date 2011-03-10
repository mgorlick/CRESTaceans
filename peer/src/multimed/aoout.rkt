#lang racket

(require (planet synx/ao)
         (prefix-in sf: (planet synx/ao/sample-format-struct))
         ffi/unsafe)

(provide make-device
         close-device
         audio-out)

(define lib (ffi-lib "libao" "4"))
(define play* (get-ffi-obj "ao_play" lib (_fun (device samples len) ::
                                               (device : _pointer)
                                               (samples : (_list i _int16))
                                               (len : _uint32) -> _bool)))

(define LR #"L,R")

(define (make-device rate channels endian)
  (let ([fmt (sf:make 16 rate channels 
                      (match endian ['native native-endian] ['little little-endian] ['big big-endian])
                      (match channels [1 monaural] [2 LR]))])
    (printf "opened device~n")
    (open #:format fmt)))

(define (audio-out device samples ct)
  (play* device samples ; each sample = 16-bit unsigned int
         (* 2 ct) ; 16 bit audio = 2 bytes/sample
         ))

(define (close-device d)
  (printf "closed device~n")
  (close d))