#! /usr/bin/env racket
#lang racket

(require "lander.rkt")

(define (go)
  (make-game 1200.0 900.0 24))

(go)