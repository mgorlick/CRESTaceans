#lang racket/base

(provide (all-defined-out))

(struct request (host port key message))