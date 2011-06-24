
(load "modules.scm")

(import* serf/log log/new)
(import* serf/fiber this-log)
; Define a default logger for fibers (and anything else that wants to use it) using a bogus authority, serf:0
(this-log (log/new "serf:0" "serf:0"))

 ; Justin and Yongjie: Change this path to the location of your SISC sandbox.
(define :sisc-sandbox-path: "/Users/mgorlick/Projects/sisc")
; Do NOT change this path.
(define :serf-sandbox-path: (string-append :sisc-sandbox-path: "/serf/"))

(import serf/peer)
(define (localhost) (peer/test/02 "localhost" 8080))


