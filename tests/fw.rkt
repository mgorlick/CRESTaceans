#lang racket

(require "../packets/module.rkt")

(provide (all-defined-out))

(define bigp (make-SinglePacket 0 0 top31 top29 #f (make-bytes (- 1460 16))))

(define shost "172.16.121.169")
(define sport 5000)
(define buffer (make-bytes (* 64 1024)))

(define time current-process-milliseconds)

(define (echoloop s behavior)
  (define start-time (time))
  (let loop ([c 766958])
    (let-values ([(written rhost rport) (udp-receive! s buffer)])
      (behavior buffer written s rhost rport)
      (if (= c 0)
          (let ([t (- (time) start-time)])
            (printf "elapsed time: ~a secs (effective ~a MB/sec)~n"
                    (exact->inexact (/ t 1000))
                    (exact->inexact (/ 1024 (/ t 1000)))))
          (loop (sub1 c))))))

(define (client [behavior e])
  (define s (udp-open-socket))
  (define written (dpacket->bytes bigp buffer))
  (udp-connect! s shost sport)
  (udp-send s buffer 0 written)
  (echoloop s behavior)
  (udp-close s))

(define (server [behavior e])
  (define s (udp-open-socket))
  (udp-bind! s shost sport)
  (echoloop s behavior)
  (udp-close s))

;;; TEST BEHAVIORS

;; only echo
(define (e pb l s rhost rport)
  (if (udp-connected? s)
      (udp-send s pb 0 l)
      (udp-send-to s rhost rport pb 0 l)))

;; deserialize, echo
(define (de pb l s rhost rport)
  (bytes->dpacket pb l)
  (if (udp-connected? s)
      (udp-send s pb 0 l)
      (udp-send-to s rhost rport pb 0 l)))

;; deserialize, reserialize, echo
(define (dre pb l s rhost rport)
  (define p (bytes->packet pb l))
  (define len (packet->bytes p buffer))
  (if (udp-connected? s)
      (udp-send s pb 0 l)
      (udp-send-to s rhost rport buffer 0 len)))