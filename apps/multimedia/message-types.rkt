#lang racket/base

(require "../../peer/src/api/tuple-type.rkt")
(provide (all-defined-out))

(define-tuple-type (spawn) [body metadata] [(reply = #f)])
(define-tuple-type (remote) [body metadata] [(reply = #f)])
(define-tuple-type (error) [reason in-reference-to])

(define-tuple-type (AddCURL) [curl])
(define-tuple-type (RemoveCURL) [curl])
(define-tuple-type (Quit) [])
(define-tuple-type (Quit/MV) [host port])
(define-tuple-type (GetParent) [])
(define-tuple-type (CopyActor) [host port])
(define-tuple-type (CopyChild) [curl host port])
(define-tuple-type (Frame) [data timestamp])
(define-tuple-type (FrameBuffer) [data size disposal ts])
(define-tuple-type (VideoParams) [width height fpsNum fpsDen])
(define-tuple-type (PIPOn) [major minor])
(define-tuple-type (InitiateBehavior) [type ref])
(define-tuple-type (AddBehaviors) [new-behaviors])
(define-tuple-type (FwdBackward) [msg ref])