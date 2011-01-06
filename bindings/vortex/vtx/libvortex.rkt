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
(define-cpointer-type _SSL-pointer)
(define-cpointer-type _SSL-CTX-pointer)
(define-cpointer-type _VortexCtx-pointer)
(define-cpointer-type _VortexThreadPool-pointer)
(define-cpointer-type _VortexPayloadFeeder-pointer)

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
(define SSL*? (flat-named-contract 'SSL*? SSL-pointer?))
(define SSL-CTX*? (flat-named-contract 'SSL-CTX*? SSL-CTX-pointer?))
(define VortexCtx*? (flat-named-contract 'VortexCtx*? VortexCtx-pointer?))
(define VortexThreadPool*? (flat-named-contract 'VortexThreadPool*? VortexThreadPool-pointer?))
(define VortexPayloadFeeder*? (flat-named-contract 'VortexPayloadFeeder*? VortexPayloadFeeder-pointer?))

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
  (_fun _pointer _VortexThreadFunc _axlPointer
        -> _axl-bool))

(define _VortexThreadDestroyFunc
  (_fun _VortexThread-pointer _axl-bool -> _axl-bool))

; tunneling
(define-cpointer-type _VortexTunnelSettings-pointer)
(define VortexTunnelSettings*? (flat-named-contract 'VortexTunnelSettings*? VortexTunnelSettings-pointer?))
(define _VortexTunnelLocationResolver
  (_fun _string _int _axlDoc-pointer _axlPointer -> _VortexTunnelSettings-pointer))

(define _ListenClosure (_fun _VortexConnection-pointer _string _string -> _int))
(define _AcceptClosure (_fun _VortexConnection-pointer _VortexConnection-pointer -> _int))
(define _ConnectClosure (_fun _VortexConnection-pointer _string _string -> _int))
(define _ReadClosure (_fun _VortexConnection-pointer _pointer _int -> _int))
(define _WriteClosure (_fun _VortexConnection-pointer _pointer _int -> _int))
(define _CloseClosure (_fun _VortexConnection-pointer _string -> _int))
(define _GetSockNameClosure (_fun _VortexConnection-pointer _pointer _pointer _pointer _pointer -> _int))
(define _GetHostUsedClosure (_fun _VortexConnection-pointer (_ptr io _string) (_ptr io _int) -> _int))
(define _WaitReadClosure (_fun _VortexConnection-pointer _int -> _int))
(define _WaitWriteClosure (_fun _VortexConnection-pointer _int -> _int))

(define _ClosureSetter (_fun _VortexConnection-pointer _int _string -> _void))

(define _NewTaskFunc (_fun _VortexCtx-pointer _VortexThreadFunc _axlPointer -> _void))
(define _NewEventFunc (_fun _VortexCtx-pointer _long _VortexThreadAsyncEvent
                            _axlPointer _axlPointer -> _int))