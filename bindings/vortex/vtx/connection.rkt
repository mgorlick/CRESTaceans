#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexConnection-pointer
               _ListenClosure
               _AcceptClosure
               _ReadClosure
               _WriteClosure
               _CloseClosure
               _GetSockNameClosure
               _GetHostUsedClosure
               _WaitReadClosure
               _WaitWriteClosure -> _void)
  vortex-connection-set-listener-mode-closures)

(defvtx* (_fun _VortexConnection-pointer
               _ConnectClosure
               _ReadClosure
               _WriteClosure
               _CloseClosure
               _GetSockNameClosure
               _WaitReadClosure
               _WaitWriteClosure -> _void)
  vortex-connection-set-client-mode-closures)

(defvtx* (_fun _ClosureSetter -> _void)
  vortex-connection-set-client-closures-setter
  vortex-connection-set-listener-closures-setter)

(defvtx* (_fun _VortexConnection-pointer -> _void)
  vortex-connection-set-client-closures
  vortex-connection-set-listener-closures)

(defvtx* (_fun _VortexCtx-pointer -> _void)
  vortex-connection-init)

(defvtx* (_fun _VortexConnection-pointer _VortexChannel-pointer -> _void)
  vortex-connection-add-channel)

(defvtx* (_fun _VortexConnection-pointer _VortexChannel-pointer
               _axl-bool -> _void)
  vortex-connection-add-channel-common)

(defvtx* (_fun _VortexConnection-pointer _axl-bool -> _void)
  vortex-connection-block)

(defvtx* (_fun _VortexConnection-pointer _int -> _axl-bool)
  vortex-connection-channel-exists)

(defvtx* (_fun _VortexConnection-pointer -> _int)
  vortex-connection-channels-count
  vortex-connection-get-id
  vortex-connection-get-mss
  vortex-connection-get-next-channel
  vortex-connection-get-opened-channels
  vortex-connection-get-pending-msgs
  vortex-connection-ref-count)

(defvtx* (_fun _VortexConnection-pointer -> _axl-bool)
  vortex-connection-close
  vortex-connection-is-blocked
  vortex-connection-is-defined-preread-handler
  vortex-connection-is-tlsficated
  vortex-connection-seq-frame-updates-status
  vortex-connection-set-blocking-socket
  vortex-connection-set-nonblocking-socket
  vortex-connection-uncheck-ref)

(defvtx* (_fun _VortexCtx-pointer _long -> _void)
  vortex-connection-connect-timeout
  vortex-connection-timeout)

(defvtx* (_fun _VortexCtx-pointer _VortexConnection-pointer
               _VortexConnectionOpts-pointer _int -> _axl-bool)
  vortex-connection-do-greetings-exchange)

(defvtx* (_fun _VortexConnection-pointer _axlHashForeachFunc
               _axlPointer -> _int)
  vortex-connection-foreach-channel)

(defvtx* (_fun _VortexConnection-pointer -> _void)
  vortex-connection-free
  vortex-connection-invoke-preread-handler
  vortex-connection-shutdown)

(defvtx* (_fun _VortexConnection-pointer _int -> _VortexChannel-pointer)
  vortex-connection-get-channel)

(defvtx* (_fun _VortexConnection-pointer
               _VortexChannelSelector _axlPointer -> _VortexChannel-pointer)
  vortex-connection-get-channel-by-func)

(defvtx* (_fun _VortexConnection-pointer _string -> _VortexChannel-pointer)
  vortex-connection-get-channel-by-uri)

(defvtx* (_fun _VortexConnection-pointer _string -> _int)
  vortex-connection-get-channel-count)

(defvtx* (_fun _VortexConnection-pointer _int -> _VortexChannelPool-pointer)
  vortex-connection-get-channel-pool)

(defvtx* (_fun _VortexCtx-pointer -> _long)
  vortex-connection-get-connect-timeout)

(defvtx* (_fun _VortexConnection-pointer -> _VortexCtx-pointer)
  vortex-connection-get-ctx)

(defvtx* (_fun _VortexConnection-pointer _string -> _axlPointer)
  vortex-connection-get-data)

(defvtx* (_fun (_or-null _VortexConnection-pointer) -> _string)
  vortex-connection-get-features
  vortex-connection-get-host
  vortex-connection-get-local-addr
  vortex-connection-get-local-port
  vortex-connection-get-localize
  vortex-connection-get-message
  vortex-connection-get-port
  vortex-connection-get-server-name)

(defvtx* (_fun _VortexConnection-pointer -> _VortexConnection-pointer)
  vortex-connection-get-listener)

(defvtx* (_fun _VortexConnection-pointer _VortexChannel-pointer
               _int _int _int -> _int)
  vortex-connection-get-next-frame-size)

(defvtx* (_fun _VortexConnection-pointer -> _axlList-pointer)
  vortex-connection-get-remote-profiles)

(defvtx* (_fun _VortexConnection-pointer -> _VortexPeerRole)
  vortex-connection-get-role)

(defvtx* (_fun _VortexConnection-pointer -> _Vortex-Socket)
  vortex-connection-get-socket)

(defvtx* (_fun _VortexConnection-pointer -> _VortexStatus)
  vortex-connection-get-status)

(defvtx* (_fun _VortexCtx-pointer -> _long)
  vortex-connection-get-timeout)

(defvtx* (_fun (_or-null _VortexConnection-pointer) _axl-bool -> _axl-bool)
  vortex-connection-is-ok)

(defvtx* (_fun _VortexConnection-pointer _int _string
               _string _VortexEncoding
               _string _VortexFrame-pointer 
               (errmsg : (_ptr o _string)) -> (result : _axl-bool)
               -> (values result errmsg))
  vortex-connection-is-profile-filtered)

(defvtx* (_fun _VortexConnection-pointer _string -> _axl-bool)
  vortex-connection-is-profile-supported
  vortex-connection-ref)

(defvtx* (_fun _VortexCtx-pointer _string _string
               _VortexConnectionNew _axlPointer -> (_or-null _VortexConnection-pointer))
  vortex-connection-new)

(defvtx* (_fun _VortexCtx-pointer _Vortex-Socket _VortexPeerRole
               -> (_or-null _VortexConnection-pointer))
  vortex-connection-new-empty)

(defvtx* (_fun _VortexCtx-pointer _string _string
               _VortexConnectionOpts-pointer _VortexConnectionNew
               _axlPointer -> (_or-null _VortexConnection-pointer))
  vortex-connection-new-full)

(defvtx* (_fun _VortexConnection-pointer _VortexFrame-pointer -> _axl-bool)
  vortex-connection-parse-greetings-and-enable)

(defvtx* (_fun _VortexConnection-pointer _int (_ptr io _string) -> _axl-bool)
  vortex-connection-pop-channel-error)

(defvtx* (_fun _VortexConnection-pointer _VortexConnectionNew _axlPointer
               -> _axl-bool)
  vortex-connection-reconnect)

(defvtx* (_fun _VortexConnection-pointer _VortexChannel-pointer -> _void)
  vortex-connection-remove-channel)

(defvtx* (_fun _VortexConnection-pointer _VortexChannel-pointer _axl-bool -> _void)
  vortex-connection-remove-channel-common)

;(defvtx* (_fun _VortexConnection-pointer _VortexConnectionHandler
;               _axlPointer -> _void)
;  vortex-connection-remove-handler)

(defvtx* (_fun _VortexConnection-pointer _VortexConnectionOnCloseFull
               _axlPointer -> _axl-bool)
  vortex-connection-remove-on-close-full)

(defvtx* (_fun _VortexCtx-pointer _axl-bool -> _void)
  vortex-connection-sanity-socket-check)

(defvtx* (_fun _VortexConnection-pointer _axl-bool -> _void)
  vortex-connection-seq-frame-updates)

(defvtx* (_fun _VortexConnection-pointer 
               _VortexConnectionOnChannelUpdate _axlPointer -> _axlPointer)
  vortex-connection-set-channel-added-handler
  vortex-connection-set-channel-removed-handler)

(defvtx* (_fun _VortexConnection-pointer _VortexConnectionStage
               _VortexConnectionAction _axlPointer -> _void)
  vortex-connection-set-connection-actions)

(defvtx* (_fun _VortexConnection-pointer _string _axlPointer -> _void)
  vortex-connection-set-data)

(defvtx* (_fun _VortexConnection-pointer _string _axlPointer
               _axlDestroyFunc _axlDestroyFunc -> _void)
  vortex-connection-set-data-full)

(defvtx* (_fun _VortexConnection-pointer -> _void)
  vortex-connection-set-default-io-handler)

(defvtx* (_fun _VortexCtx-pointer _VortexChannelFrameSize _axlPointer
               -> _VortexChannelFrameSize)
  vortex-connection-set-default-next-frame-size-handler)

(defvtx* (_fun _VortexConnection-pointer _VortexChannelFrameSize _axlPointer
               -> _VortexChannelFrameSize)
  vortex-connection-set-next-frame-size-handler)

(defvtx* (_fun _VortexConnection-pointer _VortexConnectionOnClose -> _void)
  vortex-connection-set-on-close)

(defvtx* (_fun _VortexConnection-pointer _VortexConnectionOnCloseFull
               _axlPointer -> _void)
  vortex-connection-set-on-close-full)

(defvtx* (_fun _VortexConnection-pointer _VortexConnectionOnCloseFull
               _axl-bool _axlPointer -> _void)
  vortex-connection-set-on-close-full2)

(defvtx* (_fun _VortexConnection-pointer _VortexConnectionOnPreRead -> _void)
  vortex-connection-set-preread-handler)

(defvtx* (_fun _VortexConnection-pointer _VortexProfileMaskFunc _axlPointer -> _int)
  vortex-connection-set-profile-mask)

(defvtx* (_fun _VortexConnection-pointer _VortexReceiveHandler ->
               _VortexReceiveHandler)
  vortex-connection-set-receive-handler)

(defvtx* (_fun _VortexConnection-pointer _VortexSendHandler -> _VortexSendHandler)
  vortex-connection-set-send-handler)

(defvtx* (_fun _VortexConnection-pointer _Vortex-Socket _axl-bool -> _axl-bool)
  vortex-connection-set-sock-block
  vortex-connection-set-sock-tcp-nodelay)

(defvtx* (_fun _VortexConnection-pointer _Vortex-Socket (_or-null _string) (_or-null _string) -> _axl-bool)
  vortex-connection-set-socket)

(defvtx* (_fun _VortexCtx-pointer _string _string _int 
               (error : (_ptr o _axlError-pointer)) -> (socket : _Vortex-Socket)
               -> (values socket error))
  vortex-connection-sock-connect)

(defvtx* (_fun _VortexConnection-pointer _string -> _void)
  vortex-connection-unref)