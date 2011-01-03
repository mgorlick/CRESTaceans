#! /usr/bin/env racket
#lang racket

(require "../vortex.rkt")

(define SERVER-HOST "localhost")
(define SERVER-PORT "44017")

(define FILE-TRANSFER-URI "http://www.aspl.es/vortex/profiles/file-transfer")
(define FILE-TRANSFER-URI-BIGMSG "http://www.aspl.es/vortex/profiles/file-transfer/bigmessage")
(define FILE-TRANSFER-URI-FEEDER "http://www.aspl.es/vortex/profiles/file-transfer/feeder")

(context
 (vortex-profiles-register context FILE-TRANSFER-URI #f #f #f #f #f #f)
 (vortex-profiles-register context FILE-TRANSFER-URI-BIGMSG #f #f #f #f #f #f)
 (vortex-profiles-register context FILE-TRANSFER-URI-FEEDER #f #f #f #f #f #f)
 (vortex-listener-new context SERVER-HOST SERVER-PORT #f #f)
 (vortex-listener-wait context)
 )