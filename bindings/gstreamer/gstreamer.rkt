#lang racket

(require "gst/gstreamer.rkt"
         "common-wrap/wrap.rkt")
(provide (all-defined-out)
         (all-from-out "gst/gstreamer.rkt"
                       "common-wrap/wrap.rkt"))

; with-gst-init: (or #f listof-string) any ... -> nothing
; initialize gstreamer in the current thread and perform
; any subsequent actions before deinitializing gstreamer.
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



; with-gst-init-check: (or #f listof-string) any ... -> boolean
; initialize gstreamer in the current thread and perform
; any subsequent actions before deinitializing gstreamer.
;This function will return FALSE if GStreamer could not be initialized for some reason.s


(define-syntax-rule (with-gst-init-check body ...)
  (let ([err** (cast (malloc _GError-pointer 'raw) _pointer (_ptr io _GError-pointer))])
    (let-values ([(result error) (gst_init_check #f #f err**)])
      body
      ...
      (cond
        [(not (pointer_is_null err**)) (free err**)])
      result)))


(define-syntax-rule (with-gst-init-check-args argv body ...)
  (let ([err** (cast (malloc _GError-pointer 'raw) _pointer (_ptr io _GError-pointer))]
        [argc* (malloc _int 'raw)]
        [argv** (malloc (_list i _string) 'raw)])
    
    (ptr-set! argc* _int (length argv))
    (ptr-set! argv** (_list i _string) argv)
    
    (let-values ([(result error) (gst_init_check argc* argv** err**)])
      body
      ...
      (free argc*)
      (free argv**)
      (cond
        [(not (pointer_is_null err**)) (free err**)])
      result)))


; print-error-message: _GstMessage-pointer -> (values _GError-pointer string)
; extract the error from the GstMessage,
; print the error and the debug info to stdout
; using gst's default error printing message,
; and return both the error and the debug string
(define-syntax-rule (extract-and-print-error message)
  (let ([debug-str (malloc _string 'raw)])
    (let-values ([(error) (gst_message_parse_error message debug-str)])
      (gst_object_default_error (cast message _GstMessage-pointer _GstObject-pointer)
                                error (ptr-ref debug-str _string))
      (let ([debug-val (ptr-ref debug-str _string)])
        (free debug-str)
        (g_error_free error)
        (values error debug-val)))))