#lang racket/base

(require ffi/unsafe)
(provide (rename-out (fastlz-compress compress-bytes)
                     (fastlz-decompress uncompress-bytes)))
          
(define lib (ffi-lib "fastlz"))
(define-syntax-rule (deflz+ binding obj typ)
  (define binding (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) lib typ)))
(define-syntax-rule (deflz obj typ)
  (deflz+ obj obj typ))

(deflz fastlz-compress (_fun (ub) ::
                             (ub : _bytes)
                             (_int = (bytes-length ub))
                             (cb : (_bytes o (* 2 (bytes-length ub))))
                             -> (cl : _int)
                             -> (subbytes cb 0 cl)))

(deflz fastlz-decompress (_fun (cb ul) ::
                               (cb : _bytes)
                               (_int = (bytes-length cb))
                               (ub : (_bytes o ul))
                               (ul : _int)
                               -> (actual : _int)
                               -> (and (positive? actual) ub)))