#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(define SERVER-HOST "localhost")
(define SERVER-PORT "44017")

(define FILE-TRANSFER-URI "http://www.aspl.es/vortex/profiles/file-transfer")
(define FILE-TRANSFER-URI-BIGMSG "http://www.aspl.es/vortex/profiles/file-transfer/bigmessage")
(define FILE-TRANSFER-URI-FEEDER "http://www.aspl.es/vortex/profiles/file-transfer/feeder")

(define FILE-TO-TRANSFER "copy.ogg")

(let ([outport (open-output-file FILE-TO-TRANSFER #:exists 'replace)]
      [times 1]
      [size 4096])
  (define (frame-received channel connection frame user-data)
    (cond [(eq? 'nul (vortex-frame-get-type frame))
           (cond [(not (eq? 0 (vortex-frame-get-payload-size frame)))
                  (printf "Expected to find NUL terminator message with empty content, but found ~s frame size" 
                          (vortex-frame-get-payload-size frame))]
                 [else
                  (printf "LAST frame received; operation completed, closing the file~n")
                  (vortex-async-queue-push-intsignal user-data 1)
                  ])])
    )
  
  (define (frame-received-with-msg channel connection frame user-data)
    ;
    (cond [(vtx-false? (vortex-frame-get-more-flag frame))
           (printf "Last message received~n")
           (vortex-async-queue-push-intsignal user-data 1)])
    )
  
  (context
   (connection 
    [context SERVER-HOST SERVER-PORT #f #f]
    
    (let ([q (vortex-async-queue-new)]
          [uri (match (vector-ref (current-command-line-arguments) 0)
                 ["bigmsg" FILE-TRANSFER-URI-BIGMSG]
                 ["feeder" FILE-TRANSFER-URI-FEEDER]
                 [else FILE-TRANSFER-URI])]
          [f (match (vector-ref (current-command-line-arguments) 0)
               ["bigmsg" frame-received-with-msg]
               ["feeder" frame-received-with-msg]
               [else frame-received])])
      (channel
       [connection 0 uri #f #f f q #f #f]
       
       (cond [(eq? uri FILE-TRANSFER-URI-FEEDER)
              (vortex-channel-set-complete-flag channel axl-false)])
       (printf "Made channel~n")
       
       (cond [((vector-length (current-command-line-arguments)) . > . 1)
              (set! times  (string->number (vector-ref (current-command-line-arguments) 1)))])
       (cond [((vector-length (current-command-line-arguments)) . > . 2)
              (set! size (string->number (vector-ref (current-command-line-arguments) 2)))
              (vortex-channel-set-window-size channel size)])
       
       (printf "Requested to download ~s ~s times~n" FILE-TO-TRANSFER times)
       (printf "Window size = ~s bytes~n" size)
       
       (vortex-channel-set-serialize channel axl-true)
       
       )))))