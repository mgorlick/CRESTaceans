#lang racket

(require ffi/unsafe
         "libvortex-enums.rkt"
         "libvortex-definers.rkt"
         "libaxl.rkt")
(provide (all-defined-out)
         (all-from-out "libvortex-enums.rkt"
                       "libvortex-definers.rkt"
                       "libaxl.rkt"))

; misc
(define-cstruct _in-addr
  ([s_addr _ulong]))

(define-cstruct _sockaddr-in
  ([sin_family _short]
   [sin_port _ushort]
   [sin_addr _in-addr]
   [sin_zero (_bytes o 8)]))

(define-cstruct _timeval
  ([tv_sec _long]
   [tv_usec _long]))

; Macro definitions in vortex.h
(define _Vortex-Socket _int)
(define Vortex-Socket-Error -1)
(define Vortex-Invalid-Socket -1)

; constants
(define Max-Buffer-Size 65536)
(define Max-Channel-No 2147483647)
(define Max-Channels-No 2147483647)
(define Max-Message-No 2147483647)
(define Max-Msg-No 2147483647)
(define Max-Seq-Mod 4294967296)
(define Max-Seq-No 4294967295)
(define Max-Sequence-No 4294967295)
(define Mime-Content-Description "Content-Description")
(define Mime-Content-ID "Content-ID")
(define Mime-Content-Transfer-Encoding "Content-Transfer-Encoding")
(define Mime-Content-Type "Content-Type")
(define Mime-Version "MIME-Version")

(define Vortex-TlS-Profile-URI "http://iana.org/beep/TLS")
(define Plain-Profile-URI "http://fact.aspl.es/profiles/plain_profile")

; cpointers to opaque structs
(define-cpointer-type _VortexAsyncQueue-pointer)
(define-cpointer-type _VortexChannel-pointer)
(define-cpointer-type _VortexChannelPool-pointer)
(define-cpointer-type _VortexCond-pointer)
(define-cpointer-type _VortexConnection-pointer)
(define-cpointer-type _VortexConnectionOpts-pointer)
(define-cpointer-type _VortexFrame-pointer)
(define-cpointer-type _VortexHash-pointer)
(define-cpointer-type _VortexMimeHeader-pointer)
(define-cpointer-type _VortexMutex-pointer)
(define-cpointer-type _VortexQueue-pointer)
(define-cpointer-type _VortexThread-pointer)
(define-cpointer-type _WaitReplyData-pointer)
(define-cpointer-type _VortexTunnelSettings-pointer)
(define-cpointer-type _SSL-pointer)
(define-cpointer-type _SSL-CTX-pointer)

; associated contracts for use in define/contract, provide/contract, etc
; example:
; (define/contract (foo con) 
;  (VortexConnection*? . -> . any) 
;  #t)
(define VortexAsyncQueue*? (flat-named-contract 'VortexAsyncQueue*? VortexAsyncQueue-pointer?))
(define VortexChannel*? (flat-named-contract 'VortexChannel*? VortexChannel-pointer?))
(define VortexChannelPool*? (flat-named-contract 'VortexChannelPool*? VortexChannelPool-pointer?))
(define VortexCond*? (flat-named-contract 'VortexCond*? VortexCond-pointer?))
(define VortexConnection*? (flat-named-contract 'VortexConnection*? VortexConnection-pointer?))
(define VortexConnectionOpts*? (flat-named-contract 'VortexConnectionOpts*? VortexConnectionOpts-pointer?))
(define VortexFrame*? (flat-named-contract 'VortexFrame*? VortexFrame-pointer?))
(define VortexHash*? (flat-named-contract 'VortexHash*? VortexHash-pointer?))
(define VortexMimeHeader*? (flat-named-contract 'VortexMimeHeader*? VortexMimeHeader-pointer?))
(define VortexMutex*? (flat-named-contract 'VortexMutex*? VortexMutex-pointer?))
(define VortexQueue*? (flat-named-contract 'VortexQueue*? VortexQueue-pointer?))
(define VortexThread*? (flat-named-contract 'VortexThread*? VortexThread-pointer?))
(define WaitReplyData*? (flat-named-contract 'WaitReplyData*? WaitReplyData-pointer?))
(define VortexTunnelSettings*? (flat-named-contract 'VortexTunnelSettings*? VortexTunnelSettings-pointer?))
(define SSL*? (flat-named-contract 'SSL*? SSL-pointer?))
(define SSL-CTX*? (flat-named-contract 'SSL-CTX*? SSL-CTX-pointer?))

(define-cstruct _VortexThreadPool
  ([queue _VortexAsyncQueue-pointer]
   [mutex _VortexMutex-pointer]
   [threads _axlList-pointer]
   [stopped _axlList-pointer]
   [stopped-mutex _VortexMutex-pointer]
   [events _axlList-pointer]
   [events-cursor _axlListCursor-pointer]
   [ctx _pointer]))

; full definition of ctx. flub the function pointer typedefs
; since we don't have something analogous to forward declarations
; in Racket, and we can't do the fpointer typedefs (see below)
; without having a ctx definition.

(define _mutex _VortexMutex-pointer)

(define-cstruct _VortexCtx
  ([ref-mutex _mutex]
   [ref-count _int]
   
   [data _VortexHash-pointer]
   [vortex-exit _axl-bool]
   [vortex-initialized _axl-bool]
   
   ; global mutexes
   [frame-id-mutex _mutex]
   [connection-id-mutex _mutex]
   [search-path-mutex _mutex]
   [exit-mutex _mutex]
   [listener-mustex _mutex]
   [listener-unlock _mutex]
   [inet-ntoa-mutex _mutex]
   [log-mutex _mutex]
   [use-log-mutex _axl-bool]
   [prepare-log-string _axl-bool]
   
   [serverName-acquire _axl-bool]
   [cleanups _axlList-pointer]
   
   ; default configurations
   [backlog _int]
   [enforce-profiles-supported _axl-bool]
   [automatic-mime _int]
   
   ; log variables
   [debug-checked _axl-bool]
   [debug _axl-bool]
   [debug2-checked _axl-bool]
   [debug2 _axl-bool]
   [debug-color-checked _axl-bool]
   [debug-color _axl-bool]
   [debug-handler (_fun _string _int _VortexDebugLevel _string  _pointer -> _void)] ; VortexLogHandler
   [debug-filter _int]
   [debug-filter-checked _axl-bool]
   [debug-filter-is-enabled _axl-bool]
   
   [finish-handler (_fun _pointer _pointer -> _void)] ; VortexOnFinishHandler
   [finish-handler-data _pointer]
   
   ; global handlers
   [global-frame-received (_fun _VortexChannel-pointer
                                _VortexConnection-pointer
                                _VortexFrame-pointer
                                _pointer -> _void)] ; VortexOnFrameReceived
   [global-frame-received-data _pointer]
   
   [global-notify-close (_fun _int
                              _VortexConnection-pointer
                              _pointer -> _axl-bool)] ; VortexOnNotifyCloseChannel
   [global-notify-close-data _pointer]
   
   [global-channel-added (_fun _VortexChannel-pointer
                               _pointer -> _void)] ; VortexConnectionOnChannelUpdate
   [global-channel-added-data _pointer]
   
   [global-channel-removed (_fun _VortexChannel-pointer
                                 _pointer -> _void)] ; VortexConnectionOnChannelUpdate
   [global-channel-removed-data _pointer]
   
   [global-channel-start-extended (_fun _string _int _VortexConnection-pointer _string
                                        _string (_ptr io _string) _VortexEncoding
                                        _pointer -> _axl-bool)] ; VortexOnStartChannelExtended
   [global-channel-start-extended-data _pointer]
   
   [global-idle-handler (_fun _pointer _VortexConnection-pointer
                              _pointer _pointer -> _void)] ; VortexIdleHandler
   [max-idle-period _long]
   [global-idle-handler-data _pointer]
   [global-idle-handler-data2 _pointer]
   
   [connection-id _long]
   [connection-enable-sanity-check _axl-bool]
   
   ; xml caching functions
   [connection-xml-cache _axlHash-pointer]
   [connection-hostname _axlHash-pointer]
   [connection-xml-cache-mutex _mutex]
   [connection-hostname-mutex _mutex]
   
   ; connection creation status reporting
   [connection-actions-mutex _mutex]
   [connection-actions _axlList-pointer]
   
   ; default timeout mechanisms
   [connection-std-timeout _long]
   [connection-timeout-checked _axl-bool]
   [connection-timeout-str _string]
   [connection-connect-std-timeout _long]
   [connection-connect-timeout-checked _axl-bool]
   [connection-connect-timeout-str _string]
   
   ; channel module state
   [channel-start-reply-cache-mutex _mutex]
   [channel-start-reply-cache _axlHash-pointer]
   
   ; frame factory module state
   [frame-id _long]
   
   ; profiles module state
   [registered-profiles _VortexHash-pointer]
   [profiles-list _axlList-pointer]
   [profiles-list-mutex _VortexMutex-pointer]
   
   ; io waiting module state
   [waiting-create (_fun _pointer _VortexIoWaitingFor -> _pointer)] ; VortexIoCreateFdGroup
   [waiting-destroy (_fun _pointer -> _void)] ; VortexIoDestroyFdGroup
   [waiting-clear (_fun _pointer -> _void)] ; VortexIoClearFdGroup
   [waiting-wait-on (_fun _pointer _int _VortexIoWaitingFor -> _int)] ; VortexIoWaitOnFdGroup
   [waiting-add-to (_fun _int _VortexConnection-pointer _pointer -> _axl-bool)] ; VortexIoAddToFdGroup
   [waiting-is-set (_fun _int _pointer _pointer -> _axl-bool)] ; VortexIoIsSetFdGroup
   [waiting-have-dispatch (_fun _pointer -> _axl-bool)] ; VortexIoHaveDispatch
   [waiting-dispatch (_fun _pointer
                           (_fun _int _VortexIoWaitingFor
                                 _VortexConnection-pointer _pointer -> _void) ; VortexIoDispatchFunc
                           _int _pointer -> _void)] ; VortexIoDispatch
   [waiting-type _VortexIoWaitingType] ; VortexIoWaitingType
   
   ; dtd module state
   [channel-dtd _axlDtd-pointer]
   [xml-rpc-boot-dtd _axlDtd-pointer]
   
   ; reader module state
   [reader-queue _VortexAsyncQueue-pointer]
   [reader-stopped _VortexAsyncQueue-pointer]
   [on-reading _pointer]
   [con-list _axlList-pointer]
   [srv-list _axlList-pointer]
   [con-cursor _axlListCursor-pointer]
   [srv-cursor _axlListCursor-pointer]
   [reader-cleanup _axl-bool]
   [reader-seq-frame (_bytes o 50)]
   
   ; pull module state
   [pull-pending-events _VortexAsyncQueue-pointer]
   
   ; support state
   [support-search-path _axlList-pointer]
   
   ; sequencer state
   [sequencer-queue _VortexAsyncQueue-pointer]
   [sequencer-stopped _VortexAsyncQueue-pointer]
   [sequencer-send-buffer _string]
   [sequencer-send-buffer-size _int]
   [sequencer-feeder-buffer _string]
   [sequencer-feeder-buffer-size _int]
   
   ; thread pool state
   [thread-pool-exclusive _axl-bool]
   [thread-pool _VortexThreadPool-pointer]
   [thread-pool-being-stopped _axl-bool]
   
   ; greetings state
   [greetings-features _string]
   [greetings-localize _string]
   
   ; listener state
   [listener-wait-lock _VortexAsyncQueue-pointer]
   [listener-on-accept-handlers _axlList-pointer]
   [listener-default-realm _string]
   [next-frame-size (_fun _VortexChannel-pointer
                          _int _int _int _pointer -> _int)] ; VortexChannelFrameSize
   [next-frame-size-data _pointer]
   ))
  
(define VortexCtx*? (flat-named-contract 'VortexCtx*? VortexCtx?))

;; function pointer handlers
(define _VortexAsyncQueueForeach
  (_fun _VortexAsyncQueue-pointer _axlPointer _int _axlPointer -> _void))

(define _VortexChannelFrameSize
  (_fun _VortexChannel-pointer _int _int _int _axlPointer -> _int))

(define _VortexOnCloseChannel
  (_fun _int _VortexConnection-pointer _axlPointer -> _axl-bool))

(define _VortexOnFrameReceived
  (_fun _VortexChannel-pointer _VortexConnection-pointer 
        _VortexFrame-pointer _axlPointer -> _void))

(define _VortexChannelPoolCreate
  (_fun _VortexConnection-pointer _int _string
        _VortexOnCloseChannel _axlPointer
        _VortexOnFrameReceived _axlPointer
        _axlPointer _axlPointer -> _VortexChannel-pointer))

(define _VortexChannelSelector
  (_fun _VortexChannel-pointer _axlPointer -> _axl-bool))

(define _VortexConnectionAction
  (_fun _VortexCtx-pointer _VortexConnection-pointer
        (_ptr io _VortexConnection-pointer)
        _VortexConnectionStage
        _axlPointer -> _int))

(define _VortexConnectionNew
  (_fun (_or-null _VortexConnection-pointer) (_or-null _axlPointer) -> _void))

(define _VortexConnectionOnChannelUpdate
  (_fun _VortexChannel-pointer _axlPointer -> _void))

(define _VortexConnectionOnClose 
  (_fun _VortexConnection-pointer -> _void))

(define _VortexConnectionOnCloseFull
  (_fun _VortexConnection-pointer _axlPointer -> _void))

(define _VortexConnectionOnPreRead
  (_fun _VortexConnection-pointer -> _void))

(define _VortexIdleHandler
  (_fun _VortexCtx-pointer _VortexConnection-pointer _axlPointer _axlPointer -> _void))

(define _VortexIoAddToFdGroup
  (_fun _int _VortexConnection-pointer _axlPointer -> _axl-bool))

(define _VortexIoClearFdGroup 
  (_fun _axlPointer -> _void))

(define _VortexIoCreateFdGroup
  (_fun _VortexCtx-pointer _VortexIoWaitingFor
        -> _axlPointer))

(define _VortexIoDestroyFdGroup
  (_fun _axlPointer -> _void))

(define _VortexIoDispatchFunc
  (_fun _int _VortexIoWaitingFor
        _VortexConnection-pointer _axlPointer -> _void))

(define _VortexIoDispatch
  (_fun _axlPointer _VortexIoDispatchFunc _int _axlPointer -> _void))

(define _VortexIoHaveDispatch
  (_fun _axlPointer -> _axl-bool))

(define _VortexIoIsSetFdGroup
  (_fun _int _axlPointer _axlPointer -> _axl-bool))

(define _VortexIoWaitOnFdGroup
  (_fun _axlPointer _int _VortexIoWaitingFor
        -> _int))

(define _VortexListenerReady
  (_fun _string _int _VortexStatus
        _string _axlPointer -> _void))

(define _VortexListenerReadyFull
  (_fun _string _int _VortexStatus
        _string _VortexConnection-pointer _axlPointer
        -> _void))

(define _VortexLogHandler
  (_fun _string _int _VortexDebugLevel
        _string
        _pointer ;(_list i _pointer) ;; XXX va_list
        -> _void))

(define _VortexOnAcceptedConnection
  (_fun _VortexConnection-pointer _axlPointer -> _axl-bool))

(define _VortexOnChannelCreated
  (_fun _int _VortexChannel-pointer 
        _VortexConnection-pointer _axlPointer -> _void))

(define _VortexOnChannelPoolCreated
  (_fun _VortexChannelPool-pointer _axlPointer -> _void))

(define _VortexOnClosedChannel
  (_fun _VortexChannel-pointer _axlPointer -> _void))

(define _VortexOnClosedNotification
  (_fun _int _axl-bool _string _string -> _void))

(define _VortexOnClosedNotificationFull
  (_fun _VortexConnection-pointer _int _axl-bool
        _string _string _axlPointer -> _void))

(define _VortexOnFinishHandler
  (_fun _VortexCtx-pointer _axlPointer -> _void))

(define _VortexOnNotifyCloseChannel
  (_fun _VortexChannel-pointer _int _axlPointer -> _void))

(define _VortexOnStartChannel
  (_fun _int _VortexConnection-pointer _axlPointer -> _axl-bool))

(define _VortexOnStartChannelExtended
  (_fun _string _int _VortexConnection-pointer
        _string _string (_ptr io _string)
        _VortexEncoding
        _axlPointer -> _axl-bool))

(define _VortexProfileMaskFunc
  (_fun _VortexConnection-pointer _int _string _string 
        _VortexEncoding
        _string _VortexFrame-pointer (_ptr io _string)
        _axlPointer -> _axl-bool))

(define _VortexReceiveHandler
  (_fun _VortexConnection-pointer _string _int -> _int))

(define _VortexSendHandler
  (_fun _VortexConnection-pointer _string _int -> _int))

(define _VortexThreadFunc
  (_fun _axlPointer -> _axlPointer))

(define _VortexThreadAsyncEvent
  (_fun _VortexCtx-pointer _axlPointer _axlPointer -> _axl-bool))

(define _VortexThreadCreateFunc
  (_fun (_or-null _VortexThread-pointer) _VortexThreadFunc _axlPointer
        -> _axl-bool))

(define _VortexThreadDestroyFunc
  (_fun _VortexThread-pointer _axl-bool -> _axl-bool))

; Addon: tunnel
(define _VortexTunnelLocationResolver
  (_fun _string _int _axlDoc-pointer _axlPointer -> _VortexTunnelSettings-pointer))

; Addon: TLS support
(define _VortexTlsAcceptQuery
  (_fun _VortexConnection-pointer _string -> _axl-bool))

(define _VortexTlsActivation
  (_fun _VortexConnection-pointer _VortexStatus _string _axlPointer -> _void))

(define _VortexTlsCertificateFileLocator
  (_fun _VortexConnection-pointer _string -> _string))

(define _VortexTlsCtxCreation
  (_fun _VortexConnection-pointer _axlPointer -> _axlPointer))

(define _VortexTlsPostCheck
  (_fun _VortexConnection-pointer _axlPointer _axlPointer _axlPointer -> _axl-bool))

(define _VortexTlsPrivateKeyFileLocator
  (_fun _VortexConnection-pointer _string -> _string))

;;; extra structs and pointer types needed for rewriting parts of vortex in Racket...sigh

(define-cstruct _VortexThreadPoolTask
  ([func _VortexThreadFunc]
   [data _axlPointer]))

(define-cstruct _VortexThreadPoolEvent
  ([func _VortexThreadAsyncEvent]
   [data _axlPointer]
   [data2 _axlPointer]
   [delay _long]
   [next-step _timeval]))

(define-cstruct _VortexThreadPoolStarter
  ([pool _VortexThreadPool-pointer]
   [thread _VortexThread-pointer]))

(define-cpointer-type _VortexPayloadFeeder-pointer)

(define-cstruct _VortexSequencerData
  ([channel _VortexChannel-pointer]
   [conn _VortexConnection-pointer]
   [type _VortexFrameType]
   [channel-num _int]
   [msg-no _int]
   [first-seq-no _uint]
   [message _string]
   [message-size _int]
   [step _uint]
   [resequence _axl-bool]
   [ansno _int]
   [discard _axl-bool]
   [ans-nul-list _axlList-pointer]
   [feeder _VortexPayloadFeeder-pointer]))

(define-cstruct _VortexWriterData
  ([type _VortexFrameType]
   [msg-no _int]
   [the-frame _string]
   [the-size _int]
   [is-complete _int]))

(define _ListenClosure (_fun _VortexConnection-pointer _string _string -> _int))
(define _AcceptClosure (_fun _VortexConnection-pointer _VortexConnection-pointer -> _int))
(define _ConnectClosure (_fun _VortexConnection-pointer _string _string -> _int))
(define _ReadClosure (_fun _VortexConnection-pointer _string _int -> _int))
(define _WriteClosure (_fun _VortexConnection-pointer _string -> _int))
(define _CloseClosure (_fun _VortexConnection-pointer -> _int))
(define _GetSockNameClosure (_fun _VortexConnection-pointer _pointer _pointer _pointer _pointer -> _int))
(define _GetHostUsedClosure (_fun _VortexConnection-pointer (_ptr io _string) (_ptr io _int) -> _int))
(define _WaitReadClosure (_fun _VortexConnection-pointer _int -> _int))
(define _WaitWriteClosure (_fun _VortexConnection-pointer _int -> _int))

(define _ClosureSetter (_fun _VortexConnection-pointer -> _void))

(define _NewTaskFunc (_fun _VortexCtx-pointer _VortexThreadFunc _axlPointer -> _void))
(define _NewEventFunc (_fun _VortexCtx-pointer _long _VortexThreadAsyncEvent
                            _axlPointer _axlPointer -> _int))