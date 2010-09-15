#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexChannelPool-pointer _int -> _void)
  vortex-channel-pool-add
  vortex-channel-pool-remove)

(defvtx* (_fun _VortexChannelPool-pointer _int _axlPointer -> _void)
  vortex-channel-pool-add-full)

(defvtx* (_fun _VortexChannelPool-pointer _VortexChannel-pointer -> _void)
  vortex-channel-pool-attach
  vortex-channel-pool-deattach
  vortex-channel-pool-release-channel)

(defvtx* (_fun _VortexChannelPool-pointer -> _void)
  vortex-channel-pool-close)

(defvtx* (_fun _VortexChannelPool-pointer -> _int)
  vortex-channel-pool-get-available-num
  vortex-channel-pool-get-id
  vortex-channel-pool-get-num)

(defvtx* (_fun _VortexChannelPool-pointer -> _VortexConnection-pointer)
  vortex-channel-pool-get-connection)

(defvtx* (_fun _VortexChannelPool-pointer _axl-bool -> _VortexChannel-pointer)
  vortex-channel-pool-get-next-ready)

(defvtx* (_fun _VortexChannelPool-pointer _axl-bool _axlPointer -> _VortexChannel-pointer)
  vortex-channel-pool-get-next-ready-full)

(defvtx* (_fun _VortexConnection-pointer _string _int _VortexOnCloseChannel _axlPointer
               _VortexOnFrameReceived _axlPointer _VortexOnChannelPoolCreated _axlPointer
               -> _VortexChannelPool-pointer)
  vortex-channel-pool-new)

(defvtx* (_fun _VortexConnection-pointer _string _int _VortexChannelPoolCreate _axlPointer
               _VortexOnCloseChannel _axlPointer
               _VortexOnFrameReceived _axlPointer _VortexOnChannelPoolCreated _axlPointer
               -> _VortexChannelPool-pointer)
  vortex-channel-pool-new-full)