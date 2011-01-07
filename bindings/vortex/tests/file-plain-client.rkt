#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt"
         ffi/unsafe)

(define SERVER-HOST "localhost")
(define SERVER-PORT "44018")

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
  
  (context
   [#f]
   (connection 
    [context SERVER-HOST SERVER-PORT #f #f]
    (let ([q (vortex-async-queue-new)])
      (channel
       [connection 0 Plain-Profile-URI #f #f frame-received q #f #f]
       ; allow user to adjust # of times to download, and window size
       (printf "requesting file~n")
       (vortex-channel-send-msg* channel "send the message, please" #f)
       (vortex-async-queue-pop q)
       (vortex-async-queue-unref q)
       (close out)
       (printf "Exiting client~n")
       )))))
