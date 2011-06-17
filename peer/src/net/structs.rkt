#lang racket/base

(provide (all-defined-out))

(struct request (host port data))
(struct response (data))