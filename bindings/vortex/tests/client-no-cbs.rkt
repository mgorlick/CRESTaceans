#! /usr/bin/env racket
#lang racket

(require (except-in ffi/unsafe ->)
         "../vortex.rkt"
         "no-cbs/no-cbs.rkt")

(define Profile-URI "http://vortex.aspl.es/profiles/example")

(define/contract (cast-to-aqueue queue)
  (cpointer? . -> . VortexAsyncQueue*?)
  (cast queue _pointer _VortexAsyncQueue-pointer))

(define/contract (handle-close-request close-queue)
  (VortexAsyncQueue*? . -> . void?)
  (printf "in handle-close-request~n")
  (let* ([value (cast (malloc _string) _pointer _string)]
         [read-close-request (read (get_close 0) value 1)])
    (printf "value in handle-close-request: ~s~n" value)
    (if (< read-close-request 0)
        (printf "Failed to read close notification~n")
        (begin
          (printf "Handling close channel, accepting to close it~n")
          (let ([channel (cast (vortex-async-queue-pop close-queue) _pointer _VortexChannel-pointer)]
                [msg-no (cast (vortex-async-queue-pop close-queue) _pointer _int)])
            (vortex-channel-notify-close channel axl-true msg-no)
            (void))))))

(define/contract (handle-frame-received queue)
  (VortexAsyncQueue*? . -> . void?)
  (printf "in handle-frame-received~n")
  (let ([value (cast (malloc (_bytes o 2)) _pointer (_bytes o 2))])
    (let ([read-frame (rkt_read (get_frame 0) value 1)]) ; read the frame
    ;(printf "value in handle-frame-received: ~s~n" value)
    (if (< read-frame 0)
        (printf "Failed to read from the pipe~n")
        (begin
          (printf "Read the following frame: ~s~n" read-frame)
          (let ([frame (cast (vortex-async-queue-pop queue) _pointer _VortexFrame-pointer)])
            (printf "Frame received: (size: ~s):~n     ~s~n"
                    (vortex-frame-get-payload-size frame)
                    (cast (vortex-frame-get-payload frame) _pointer _string))
            (vortex-frame-free frame)
            (void)))))))

(define (main)
  (with-vtx-ctx
   ctx
   (let* ([queue (vortex-async-queue-new)]
          [close-queue (vortex-async-queue-new)])
     (make_frame_pipe)
     (make_close_pipe)
     (printf "Queue and close-queue: ~s, ~s~n" queue close-queue)
     (let ([max_fds (max (get_frame 0)
                         (get_frame 1)
                         (get_close 0)
                         (get_close 1))])
       (printf "max_fds: ~s~n" max_fds)
       ;(vortex-profiles-register ctx Profile-URI
       ;                          #f #f ; no start handler, accept all channels
       ;                          #f #f ; no close channel, accept all channels to be closed
       ;                          vortex-channel-queue-reply queue)
       (client_register_nocbs_profile ctx queue)
       (with-vtx-conn
        connection ctx "localhost" "44000" #f #f ; a blocking connection
        (with-vtx-channel
         channel connection 0 Profile-URI ; next channel available
         #f #f ; no close notification
         ;queue-reply queue ; use queue-reply as frame receive handler
         #f #f
         #f #f ; no channel creation notification
         ;(vortex-channel-set-close-notify-handler channel close-request-received close-queue) ; configure close notification
         (set_frame_received_handler channel queue)
         (set_close_notify channel close-queue)
         (let loop ([iterator 0])
           (printf "in (loop ~s)~n" iterator)
           (cond 
             [(< iterator 10) 
              (printf "Sending message: ~s...~n" iterator)
              (let ([send-result 
                     (vortex-channel-send-msgv channel #f (format "This is a test: ~s" iterator))])
                (if (vtx-false? send-result)
                    (printf "Unable to send message~n")
                    (begin
                      (printf "sent message~n")
                      (set! iterator (+ iterator 1))
                      (let ([rdfs (rkt_fd_make)]
                            [tv (rkt_timeval_make 5 0)])
                        (printf "rdfs: ~s~n" rdfs)
                        (rkt_fd_zero rdfs)
                        (printf "zero'd rdfs~n")
                        (rkt_fd_set (get_frame 0) rdfs)
                        (rkt_fd_set (get_close 0) rdfs)
                        (printf "frame-pipe after FD_SET: ~s, ~s~n" (get_frame 0) (get_frame 1))
                        (printf "close-pipe after FD_SET: ~s, ~s~n" (get_close 0) (get_close 1))
                        (let ([retval (rkt_select (+ max_fds 1) rdfs #f #f tv)])
                          (printf "select return value: ~s~n" retval)
                          (if (eq? retval -1) 
                              (begin 
                                (printf "Signal trap...retrying~n") 
                                (loop iterator))
                              (if (eq? retval 0) 
                                  (begin 
                                    (printf "Timeout found...retrying~n") 
                                    (loop iterator))
                                  (if (rkt_fd_isset (get_frame 0) rdfs)
                                      (begin 
                                        (printf "frame-pipe is set, handling frame received~n")
                                        (handle-frame-received queue)
                                        (loop iterator))
                                      (if (rkt_fd_isset (get_close 0) rdfs)
                                          (begin 
                                            (printf "close-pipe is set, handling close request~n")
                                            (handle-close-request close-queue))
                                          (loop iterator)
                                          )))))))))]
             [else (printf "iterator >= 10~n")]))))))))

(main)

;(define/contract (queue-reply channel connection frame user-data)
;  (VortexChannel*? VortexConnection*? VortexFrame*? cpointer? . -> . void?)
;  (printf "in queue-reply~n")
;  (let ([queue (cast-to-aqueue user-data)]
;        [frame-copy (vortex-frame-copy frame)])
;    (vortex-async-queue-push queue frame-copy)
;    (let ([result (rkt_write (vector-ref frame-pipe 1) "f" 1)])
;      (void)
;      )))
;
;(define/contract (close-request-received channel msg-no user-data)
;  (VortexChannel*? integer? cpointer? . -> . void?)
;  (printf "in close-request-received~n")
;  (let ([queue (cast-to-aqueue user-data)])
;    (vortex-async-queue-push queue channel)
;    (vortex-async-queue-push queue msg-no)
;    (rkt_write (vector-ref close-pipe 1) "c" 1)
;    (void)
;    ))