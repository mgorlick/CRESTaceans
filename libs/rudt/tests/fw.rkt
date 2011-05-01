#lang racket

(require rackunit
         rackunit/text-ui
         "../packets/util.rkt"
         "../packets/packets.rkt"
         "../packets/recv.rkt"
         "../packets/control.rkt"
         "../packets/data.rkt")

(provide (all-defined-out))

(define bigp (make-SinglePacket 0 0 top31 top29 #f (make-bytes (- 1400 16))))

(define shost "172.16.121.168")
(define sport 5000)

(define (echoloop s buffer behavior)
  (define start-time (current-inexact-milliseconds))
  (let loop ([c (sub1 766958)])
    (let-values ([(written rhost rport) (udp-receive! s buffer)])
      (define pb (subbytes buffer 0 written))
      (behavior s rhost rport pb)
      (if (= c 0)
          (printf "elapsed time: ~a ms (= ~a secs)~n"
                  (- (current-inexact-milliseconds) start-time)
                  (/ (- (current-inexact-milliseconds) start-time) (* 1000)))
          (loop (sub1 c))))))

(define (run-client [behavior echo])
  (define buffer (make-bytes (* 64 1024)))
  (define s (udp-open-socket))
  (udp-connect! s shost sport)
  (udp-send s (dpacket->bytes bigp))
  (echoloop s buffer behavior)
  (udp-close s))

(define (run-server [behavior echo])
  (define (open-server)
    (define s (udp-open-socket))
    (udp-bind! s shost sport)
    s)
  (define buffer (make-bytes (* 64 1024)))
  (define s (open-server))
  (echoloop s buffer behavior)
  (udp-close s))


;; behaviors
(define (echo s rhost rport pb)
  (if (udp-connected? s)
      (udp-send s pb)
      (udp-send-to s rhost rport pb)))

(define (deserialize/echo s rhost rport pb)
  (define p (bytes->packet pb))
  (if (udp-connected? s)
      (udp-send s pb)
      (udp-send-to s rhost rport pb)))

(define (deserialize/reserialize/echo s rhost rport pb)
  (define p (bytes->packet pb))
  (define pb2 (packet->bytes p))
  (if (udp-connected? s)
      (udp-send s pb)
      (udp-send-to s rhost rport pb2)))