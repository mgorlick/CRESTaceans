#lang racket

(require (planet bzlib/thread:1:0))

(define (udp-source port sinks)
  (let ([sock (udp-open-socket)]
        [buffer (make-bytes 40)])
    (udp-bind! sock #f port)
    (let loop ()
      (let-values ([(nr addr port) (udp-receive! sock buffer)])
        (for ([sink (in-list sinks)])
          (thread-send sink (list (current-thread) (bytes->immutable-bytes buffer) nr)))
        (loop)))))

(let* ([pid (current-thread)]
       [t (thread (λ () (udp-source 44000 (list pid))))])
  (let loop ()
    (receive/match
     [(list (? thread? thd) (? bytes? buffer) (? number? nr))
      (printf "~a~n" buffer) (loop)])))