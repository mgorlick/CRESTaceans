#lang racket

(require "../gstreamer.rkt")

(define (std-event-loop pipe)
  (define bus (gst_element_get_bus pipe))
  (let loop ()
    (let* ([message (gst_bus_poll bus 'any -1)]
           [type (gst_message_type message)])
      (match type
        ['eos  (printf "eos~n")
               (gst_message_unref_w message)]
        [(or 'warning 'error)  (printf "error~n")
                               (extract-and-print-error message)
                               (gst_message_unref_w message)]
        [_  (printf "else: ~s~n" type)
            (gst_message_unref_w message)
            (loop)]))))

(provide (all-defined-out))