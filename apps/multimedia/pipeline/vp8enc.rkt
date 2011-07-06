#lang racket/base

(require "../bindings/vp8/vp8.rkt"
         "util.rkt"
         "structs.rkt"
         "bufferpool.rkt"
         racket/contract
         racket/match)

(provide make-vp8-encoder)

(define BUFSIZE (* 1024 1024))

(define/contract (make-vp8-encoder signaller receiver)
  (thread? thread? . -> . (-> void))
  
  (define is-signaller? (make-thread-id-verifier signaller))
  
  (λ ()
    (let-values ([(width height fpsnum fpsden)
                  (match (receive-killswitch/whatever is-signaller?)
                    [(VideoParams w h n d) (values w h n d)]
                    [else (error "Precending component failed to obey parameter startup protocol~n")])])
      
      (define e (vp8enc-new width height fpsnum fpsden))
      (define-values (handler λrequest) (make-bufferpool-handler 20 BUFSIZE))
      
      (let loop ()
        (match (receive-killswitch/whatever is-signaller?)
          [(? die? _) (vp8enc-delete e)
                      (command/killswitch signaller receiver)
                      (reply/state-report signaller #f)]
          [(FrameBuffer data size λdisposal)
           (let-values ([(outbuff λreturn) (λrequest)])
             (let ([written (vp8enc-encode e size data BUFSIZE outbuff)])
               (thread-send receiver (make-FrameBuffer outbuff written λreturn))))
           (λdisposal)
           (loop)])))))