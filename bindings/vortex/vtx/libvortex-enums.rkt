#lang racket

(require ffi/unsafe)
(provide (all-defined-out))

(define << arithmetic-shift)

(define _VortexFileTest
  (_enum '(file-exists = 1
                       file-is-link = 2
                       file-is-dir = 4
                       file-is-regular = 8)))

; enum VortexConfItem
;(define Vortex-Soft-Sock-Limit 1)
;(define Vortex-Hard-Sock-Limit 2)
;(define Vortex-Listener-Backlog 3)
;(define Vortex-Enforce-Profiles-Supported 4)
;(define Vortex-Automatic-Mime-Handling 5)

(define _VortexConfItem
  (_enum '(soft-sock-limit = 1
                           hard-sock-limit = 2
                           listener-backlog = 3
                           enforce-profiles-supported = 4
                           automatic-mime-handling = 5)))

; enum VortexConnectionOptItem
;(define Vortex-Opts-End 0)
;(define Vortex-Servername-Feature 1)
;(define Vortex-Servername-Acquire 2)
;(define Vortex-Opts-Release 3)

(define _VortexConnectionOptItem
  (_enum '(opts-end = 0
                    servername-feature = 1
                    servername-acquire = 2
                    opts-release = 3)))

; enum VortexConnectionStage
;(define Connection-Stage-Post-Created 1)
;(define Connection-Stage-Process-Greetings-Features 2)

(define _VortexConnectionStage
  (_enum '(post-created = 1
                        process-greetings-features = 2)))

; enum VortexDebugLevel

(define _VortexDebugLevel
  (_enum '(debug = 0
                 warning = 1
                 critical = 2)))

; enum VortexEncoding
;(define EncodingUnknown 1)
;(define EncodingNone 2)
;(define EncodingBase64 3)

(define _VortexEncoding
  (_enum '(unknown = 1
                   none = 2
                   base64 = 3)))

; enum VortexFrameType
;(define Vortex-Frame-Type-Unknown 0)
;(define Vortex-Frame-Type-MSG 1)
;(define Vortex-Frame-Type-RPY 2)
;(define Vortex-Frame-Type-ANS 3)
;(define Vortex-Frame-Type-ERR 4)
;(define Vortex-Frame-Type-NUL 5)
;(define Vortex-Frame-Type-SEQ 6)

(define _VortexFrameType
  (_enum '(unknown = 0
                   msg = 1
                   rpy = 2
                   ans = 3
                   err = 4
                   nul = 5
                   seq = 6)))

; enum VortexIoWaitingFor
;(define Read-Operations (1 . << . 0))
;(define Write-Operations (1 . << . 1))

(define _VortexIoWaitingFor
  (_enum '(read-operations = 1
                           write-operations = 2)))
                           

; enum VortexIoWaitingType
;(define Vortex-Io-Wait-Select 1)
;(define Vortex-Io-Wait-Poll 2)
;(define Vortex-Io-Wait-Epoll 3)

(define _VortexIoWaitingType
  (_enum '(select = 1
                  poll = 2
                  epoll = 3)))

; enum VortexPeerRole
;(define VortexRoleUnknown 0)
;(define VortexRoleInitiator 1)
;(define VortexRoleListener 2)
;(define VortexRoleMasterListener 3)

(define _VortexPeerRole
  (_enum '(unknown = 0
                   initiator = 1
                   listener = 2
                   master-listener = 3)))

; enum VortexStatus
;(define VortexError 1)
;(define VortexOk 2)
;(define VortexBindError 3)
;(define VortexWrongReference 4)
;(define VortexNameResolvFailure 5)
;(define VortexSocketCreationError 6)
;(define VortexSocketSanityError 7)
;(define VortexConnectionError 8)
;(define VortexConnectionTimeoutError 9)
;(define VortexGreetingsFailure 10)
;(define VortexXmlValidationError 11)
;(define VortexConnectionCloseCalled 12)
;(define VortexConnectionForcedClose 13)
;(define VortexProtocolError 14)
;(define VortexConnectionFiltered 15)
;(define VortexMemoryFail 16)

(define _VortexStatus
  (_enum '(error = 1
                 ok = 2
                 bind-error = 3
                 wrong-reference = 4
                 name-resolve-failure = 5
                 socket-creation-error = 6
                 socket-sanity-error = 7
                 connection-error = 8
                 connection-timeout-error = 9
                 greetings-failure = 10
                 xml-validation-error = 11
                 connection-close-called = 12
                 connection-forced-close = 13
                 protocol-error = 14
                 connection-filtered = 15
                 memory-fail = 16)))

; enum VortexThreadConf
;(define Vortex-Thread-Conf-End 0)
;(define Vortex-Thread-Conf-Joinable 1)

(define _VortexThreadConf
  (_enum '(end = 0
               joinable = 1)))

; enum WhatUpdate
;(define Update-SEQ-No (1 . << . 0))
;(define Update-MSG-No (1 . << . 1))
;(define Update-RPY-No (1 . << . 2))
;(define Update-ANS-No (1 . << . 3))
;(define Update-RPY-No-Written (1 . << . 4))
;(define Decrease-MSG-No (1 . << . 5))
;(define Decrease-RPY-No (1 . << . 6))

(define _WhatUpdate
  (_enum '(update-seq-no = 1
                  update-msg-no = 2
                  update-rpy-no = 4
                  update-ans-no = 8
                  update-rpy-no-written = 16
                  decrease-msg-no = 32
                  decrease-rpy-no = 64)))

; enum VortexTunnelItem

(define _VortexTunnelItem
  (_enum '(end-conf = 1
                    fqdn = 2
                    port = 3
                    ip4 = 4
                    ip6 = 5
                    srv = 6
                    uri = 7
                    endpoint = 8)))

; enum VortexDigestMethod
(define _VortexDigestMethod
  (_enum '(sha1 = 1
                md5 = 2)))