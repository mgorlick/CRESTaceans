#lang racket

(require "util.rkt"
         "../../../bindings/gstreamer/gstreamer.rkt"
         (planet bzlib/thread:1:0))
(provide (all-defined-out))

(define (restartable-evt-loop bin)
  
  (define bus (gst_element_get_bus bin))
  
  (define (unref m #:is-error? [is-error? #f])
    (when is-error? (printf "error/warning:~n") (extract-and-print-error m))
    (gst_message_unref_w m))
  
  (define/contract (handle-a-message)
    (-> boolean?)
    (let ([message (gst_bus_timed_pop bus 0)])
      (cond
        [message (let ([type (gst_message_type message)])
                   (match type
                     ['eos (printf "eos~n") (unref message) #f]
                     [(or 'warning 'error) (unref message #:is-error? #t) #f]
                     [_  (printf "else: ~a~n" type) (unref message) #t]))]
        [else #t])))
  
  (define (pause-bin/switch port)
    (printf "Pausing...~n")
    (gst_element_set_state bin GST_STATE_PAUSED)
    (let ([udpsink (gst_bin_get_by_name* bin "udpsink")])
      (g_object_set udpsink "port" port)))
  
  (define (restart-bin)
    (printf "Restarting...~n")
    (gst_element_set_state bin GST_STATE_PLAYING))
  
  (let loop ([paused? #f])
    (receive/match
     [(list (? thread? t) 'pause/switch-port (? integer? port))
      (pause-bin/switch port)
      (loop #t)]
     
     [(list (? thread? t) 'restart)
      (restart-bin)
      (loop #f)]
     
     [after 0
            (if paused?
                (loop paused?)
                (when (handle-a-message)
                  (loop paused?)))]
     )))

(define (start port)
  (thread (λ () 
            (with-gst-init
             #f
             (let-values ([(bin error)
                           (gst_parse_launch
                            (string-append "audiotestsrc wave=3 ! "
                                           "audioconvert ! audio/x-raw-float,channels=1,rate=44100,width=32 ! "
                                           "udpsink name=udpsink host=127.0.0.1 port=" (number->string port)))])
               (gst_element_set_state bin GST_STATE_PLAYING)
               (restartable-evt-loop bin)
               (gst_element_set_state bin GST_STATE_NULL)
               (gst_object_unref bin))))))

(define (p/s thd port)
  (thread-send thd (list (current-thread) 'pause/switch-port port)))

(define (restart thd)
  (thread-send thd (list (current-thread) 'restart)))

(define pcm/pipeline (start 4999))
;(sleep 3)
;(p/s pcm/pipeline 4998)