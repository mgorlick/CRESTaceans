(import generic-procedures)
(import oo)
(import threading)
(import type-system)

(require-library 'sisc/libs/srfi/srfi-13)
(import srfi-13) ; String library.

(import pattern-matching) ; For the (match ...) macro.

(load "time.scm")
(load "uri.scm")
(load "uuid.scm")
(load "queue.scm")
(load "mailbox.scm")
(load "fiber.scm")
(load "serialize.scm")
(load "logger.scm")
(load "nio/nio_http_client.scm")
(load "nio/http.scm")
(load "nio/session.scm")
(load "nio/client.scm")
(load "nio/nio_tests.scm")

; Define a default logger for fibers (and anything else that wants to use it) using a bogus authority, serf:0
(this-logger (make-logger-for-authority (make-authority "serf" 0)))




