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
                             (_bytes = (if (>= (bytes-length ub) 16) 
                                           ub 
                                           (error "uncompressed buffer too small")))
                             (_int = (bytes-length ub))
                             ; The output buffer must be at least 5% larger than the input buffer
                             ; and can not be smaller than 66 bytes.
                             (cb : (_bytes o (max 66 
                                                  (inexact->exact
                                                   (round (+ 0.5 (* 1.1 (bytes-length ub))))))))
                             -> (actual : _int)
                             -> (cond [(positive? actual) 
                                       (make-sized-byte-string cb actual)]
                                      [else (error "fastlz couldn't compress")])))

(deflz fastlz-decompress (_fun (cb ul) ::
                               (cb : _bytes)
                               (_int = (bytes-length cb))
                               (ub : (_bytes o ul))
                               (ul : _int)
                               -> (actual : _int)
                               -> (cond [(positive? actual)
                                         (make-sized-byte-string ub actual)]
                                        [else (error "fastlz couldn't decompress")])))