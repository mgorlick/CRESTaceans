#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(context
 [#f #f #f]
 (vortex-profiles-register context Plain-Profile-URI #f #f
                           #f #f #f #f)
 (vortex-listener-new context "0.0.0.0" "44015" #f #f)
 (vortex-listener-wait context))
