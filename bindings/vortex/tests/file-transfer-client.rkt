#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt"
         ffi/unsafe)

(define SERVER-HOST "localhost")
(define SERVER-PORT "44017")

(define FILE-TRANSFER-URI "http://www.aspl.es/vortex/profiles/file-transfer")
(define FILE-TRANSFER-URI-BIGMSG "http://www.aspl.es/vortex/profiles/file-transfer/bigmessage")
(define FILE-TRANSFER-URI-FEEDER "http://www.aspl.es/vortex/profiles/file-transfer/feeder")

(define FILE-TO-SAVE (string-append (path->string (current-directory)) "copy.ogg"))

(let ([out (open-output-file FILE-TO-SAVE #:exists 'replace)]
      [times 1]
      [size 4096])
  
  (define (re/open!)
    (cond [(port? out) (close out)])
    (set! out (open-output-file FILE-TO-SAVE #:exists 'replace)))
  
  (define (open? port)
    (not (port-closed? port)))
  
  (define (close port)
    (cond [(open? port) (close-output-port port)]))
  
  (define (write s)
    (write-bytes s out))
  
  (define (write* frame)
    (printf "Writing ~s bytes...~n" (vortex-frame-get-payload-size frame))
    (let ([amt (write (vortex-frame-get-payload-bytes frame))])
      (printf "Wrote ~s bytes~n" amt)))
  
  (define (frame-received channel connection frame user-data)
    (printf "frame received, ~s bytes~n" (vortex-frame-get-payload-size frame))
    (cond [(eq? 'nul (vortex-frame-get-type frame))
           (cond [(not (eq? 0 (vortex-frame-get-payload-size frame)))
                  (printf "Expected to find NUL terminator message with empty content, but found ~s frame size" 
                          (vortex-frame-get-payload-size frame))]
                 [else
                  (printf "LAST frame received; operation completed, closing the file~n")
                  (vortex-async-queue-push-intsignal (cast user-data _pointer _VortexAsyncQueue-pointer) 1)
                  ])]
          [else (write* frame)])
    )
  
  (define (frame-received-with-msg channel connection frame user-data)
    (write* frame)
    (cond [(vtx-false? (vortex-frame-get-more-flag frame))
           (printf "Last message received~n")
           (vortex-async-queue-push-intsignal (cast user-data _pointer _VortexAsyncQueue-pointer) 1)])
    )
  
  (context
   [#f #f #f]
   (connection 
    [context SERVER-HOST SERVER-PORT #f #f]
    
    (let ([q (vortex-async-queue-new)]
          [uri (if (> (vector-length (current-command-line-arguments)) 0)
                   (match (vector-ref (current-command-line-arguments) 0)
                     ["bigmsg" FILE-TRANSFER-URI-BIGMSG]
                     ["feeder" FILE-TRANSFER-URI-FEEDER]
                     [else FILE-TRANSFER-URI])
                   FILE-TRANSFER-URI)]
          [f (if (> (vector-length (current-command-line-arguments)) 0)
                 (match (vector-ref (current-command-line-arguments) 0)
                   [(or "bigmsg" "feeder") frame-received-with-msg]
                   [else frame-received])
                 frame-received)])
      
      (channel
       [connection 0 uri #f #f f q #f #f]
       
       ; feeder profile requires deactivation of message reassembly. why? who knows
       (cond [(eq? uri FILE-TRANSFER-URI-FEEDER)
              (vortex-channel-set-complete-flag channel axl-false)])
       (printf "Made channel~n")
       
       ; allow user to adjust # of times to download, and window size
       (cond [(> (vector-length (current-command-line-arguments)) 1)
              (set! times (string->number (vector-ref (current-command-line-arguments) 1)))])
       (cond [(> (vector-length (current-command-line-arguments)) 2)
              (set! size (string->number (vector-ref (current-command-line-arguments) 2)))])
       (printf "mode = ~a~n" uri)
       (printf "Requested to download ~s ~s times~n" FILE-TO-SAVE times)
       (printf "Window size = ~s bytes~n" size)
       (vortex-channel-set-window-size channel size)
       (printf "Window size after change: ~s bytes~n" (vortex-channel-get-window-size channel))
       (vortex-channel-set-serialize channel axl-true)
       (let loop ([c times])
         (printf "requesting file~n")
         (vortex-channel-send-msg* channel "send the message, please" #f)
         (vortex-async-queue-pop q)
         (printf "Transfer #~s done, pending: ~s more times~n" (- times c) (sub1 c))
         (cond
           [(> c 1)
            (re/open!)
            (loop 
             (sub1 c))]))
       
       (vortex-async-queue-unref q)
       (close out)
       (printf "Exiting client~n")
       )))))