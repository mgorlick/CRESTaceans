#lang racket

(require (planet synx/ao)
         (prefix-in sf: (planet synx/ao/sample-format-struct))
         ffi/unsafe)

(provide audio-out)

(define lib (ffi-lib "libao" "4"))
(define play* (get-ffi-obj "ao_play" lib (_fun (device samples len) ::
                                               (device : _pointer)
                                               (samples : (_list i _int16))
                                               (len : _uint32) -> _bool)))

(define vorbis-format (sf:make 16 44100 1 native-endian monaural))

(define device (open #:format vorbis-format))

(define (audio-out samples ct)
  (play* device samples ; each sample = 16-bit unsigned int
                        (* 2 ct) ; 16 bit audio = 2 bytes/sample
                        ))