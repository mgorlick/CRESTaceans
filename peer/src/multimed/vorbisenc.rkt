#! /usr/bin/env racket
#lang racket

(require "../../../bindings/gstreamer/gstreamer.rkt"
         "util.rkt"
         (planet bzlib/thread:1:0))
(provide (all-defined-out))

(define (std-event-loop bin)
  
  (define bus (gst_element_get_bus bin))
  
  (define/contract (handle-a-message)
    (-> boolean?)
    (let ([message (gst_bus_timed_pop bus 0)])
      (if message
          (let ([type (gst_message_type message)])
            (match type
              ['eos  (printf "eos~n")
                     (gst_message_unref_w message)
                     #f]
              [(or 'warning 'error)  (printf "error~n")
                                     (extract-and-print-error message)
                                     (gst_message_unref_w message)
                                     #f]
              [_  (printf "else: ~s~n" type)
                  (gst_message_unref_w message)
                  #t]))
          #t)))
  
  (let loop ([paused? #f])
    (receive/match
     [(list (? thread? t) 'pause/switch-port (? integer? port))
      (printf "Pausing...~n")
      (gst_element_set_state bin GST_STATE_PAUSED)
      (let ([udpsink (gst_bin_get_by_name* bin "udpsink")])
        (g_object_set udpsink "port" port))
      (loop #t)]
     [(list (? thread? t) 'restart)
      (printf "Restarting...~n")
      (gst_element_set_state bin GST_STATE_PLAYING)
      (loop #f)]
     [after 0
            (if paused?
                (loop paused?)
                (when (handle-a-message)
                  (loop paused?)))])))

(define (start port)
  (thread (λ () 
            (with-gst-init
             #f
             (let-values ([(bin error)
                           (gst_parse_launch
                            (string-append "audiotestsrc ! vorbisenc ! "
                                           "udpsink name=udpsink host=127.0.0.1 port=" (number->string port)))])
               (gst_element_set_state bin GST_STATE_PLAYING)
               (std-event-loop bin)
               (gst_element_set_state bin GST_STATE_NULL)
               (gst_object_unref bin))))))

(define (pause/switch-port thd port)
  (to-all thd <- 'pause/switch-port port))

(define (restart thd)
  (to-all thd <- 'restart))

(define driver (start 5000))
(sleep 2)
(pause/switch-port driver 5001)
;(sleep 2)
;(restart driver)