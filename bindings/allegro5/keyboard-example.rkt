#! /usr/bin/racket
#lang racket/base
(require ffi/unsafe
         ffi/unsafe/cvector
         "allegro5.rkt")

(easy-init 800 600)
(call/cc (lambda (k) 
           (for ([i (in-range 1000)])
             (printf "loop start~n")
             (let ((state (al-get-keyboard-state)))
               (cond
                 [(al-key-down state Allegro-Key-F)
                  (printf "#t~n")]
                 [else (printf "#f~n")])
               (cond
                 [(al-key-down state Allegro-Key-Escape)
                  (k 42)])
               (al-delete-keyboard-state state)
               (sleep 1)))))
(easy-exit)