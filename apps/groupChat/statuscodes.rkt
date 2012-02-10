#lang racket/base

(require "../../peer/src/api/tuple-type.rkt")
(provide (all-defined-out))

(define-tuple-type (200-OK) [reason in-reference-to])
(define-tuple-type (201-Created) [reason in-reference-to])
(define-tuple-type (202-Accepted) [reason in-reference-to])

(define-tuple-type (403-Forbidden) [reason in-reference-to])
(define-tuple-type (404-Not-Found) [reason in-reference-to])
(define-tuple-type (405-Method-Not-Allowed) [reason in-reference-to])
(define-tuple-type (406-Not-Acceptable) [reason in-reference-to])
(define-tuple-type (409-Conflict) [reason in-reference-to])
(define-tuple-type (410-Gone) [reason in-reference-to])
(define-tuple-type (415-Unsupported-Media-Type) [reason in-reference-to])

(define-tuple-type (500-Internal-Server-Error) [reason in-reference-to])



