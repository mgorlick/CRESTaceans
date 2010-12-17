#! /usr/bin/env racket
#lang racket

(require "../gstreamer.rkt")
(provide (all-defined-out))

; elementary pausing and resuming via command line interaction
; $ racket
; Welcome to Racket v5.0.1.2.
; > (require "test2.rkt")
; #<thread:.../tests/test2.rkt:62:8>
; > (pause)
; > Pausing...
; (play)
; > Playing...
; (pause)  
; > Pausing...
; (play)
; > Playing...
; End of stream

(define path-to-file (string-append "file://" (path->string (current-directory))))
(define file-name "video.mpe")

(define (buscall bus* msg* data*)
  (let ([loop (cast data* _gpointer _GMainLoop-pointer)]
        [type (gst_message_type msg*)])
    (cond
      [(eq? type GST_MESSAGE_EOS) 
       (printf "End of stream~n") 
       (g_main_loop_quit loop)]
      [(eq? type GST_MESSAGE_ERROR)
       (printf "Unknown error~n")
       (g_main_loop_quit loop)]
      [else #f])
    1))

(define (print-position data*)
  (let ([fmt* (malloc _int 'raw)]
        [pipeline* (cast data* _gpointer _GstElement-pointer)])
    (ptr-set! fmt* _int GST_FORMAT_TIME)
    (let-values ([(pos-suc pos) (gst_element_query_position pipeline* fmt*)]
                 [(len-suc len) (gst_element_query_duration pipeline* fmt*)])
      (print_gst_time_format pos len)
      (free fmt*)
      ))
  1)

(define pause-the-stream? #f)

(define (pause)
  (set! pause-the-stream? #t))

(define (play)
  (set! pause-the-stream? #f))

(define (play-or-pause data*)
  (let ([pipeline* (cast data* _gpointer _GstElement-pointer)])
    (let-values ([(state pending) (gst_element_get_state pipeline* 0)])
      (if pause-the-stream?
          (if (equal? state GST_STATE_PLAYING)
              (begin 
                (gst_element_set_state pipeline* GST_STATE_PAUSED)
                (printf "Pausing...~n"))
              #f)
          (if (equal? state GST_STATE_PAUSED)
              (begin
                (gst_element_set_state pipeline* GST_STATE_PLAYING)
                (printf "Playing...~n"))
              #f))
      ))
  1)


(with-gst-init 
 #f
 (thread 
  (lambda ()
    (let* ([pipeline (gst_element_factory_make "playbin2" "player")]
           [loop (g_main_loop_new #f 0)]
           [bus (gst_pipeline_get_bus (cast pipeline 
                                            _GstElement-pointer 
                                            _GstPipeline-pointer))])
      
      
      (g_object_set pipeline "uri" (string-append path-to-file file-name))
      (gst_bus_add_watch bus buscall loop)
      (gst_object_unref bus)
      (gst_element_set_state pipeline GST_STATE_PLAYING)
      (g_timeout_add 10000 print-position pipeline)
      (g_timeout_add 100 play-or-pause pipeline)
      (g_main_loop_run loop)
      (gst_element_set_state pipeline GST_STATE_NULL)
      (g_free loop)
      (gst_object_unref pipeline)
      )
    )
  )
 )