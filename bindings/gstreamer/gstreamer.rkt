#lang racket

(require "gst/gstreamer.rkt")
(provide (all-defined-out)
         (all-from-out "gst/gstreamer.rkt"))

(define-syntax-rule (with-gst-init argv body ...)
  (cond
    [(eq? argv #f)
     (begin 
       (gst_init #f #f)
       body 
       ...)]
    [else
     (begin
       (let ([argc* (malloc _int 'raw)]
             [argv** (malloc (_list i _string) 'raw)])
         (ptr-set! argc* _int (length argv))
         (ptr-set! argv** (_list i _string) argv)
         (gst_init argc* argv**)
         body
         ...
         (free argc*)
         (free argv**)))]))