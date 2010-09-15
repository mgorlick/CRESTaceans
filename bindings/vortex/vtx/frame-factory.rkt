#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx* (_fun _VortexFrame-pointer _VortexFrame-pointer -> _axl-bool)
  vortex-frame-are-equal
  vortex-frame-are-joinable
  vortex-frame-join
  vortex-frame-join-extending)

(defvtx* (_fun _VortexFrameType _int _int 
               _axl-bool _uint _int _int _pointer -> _string)
  vortex-frame-build-up-from-params)

(defvtx* (_fun _VortexFrameType _int _int 
               _axl-bool _uint _int _int _string ; more seqno size ansno content_type
               _string _pointer _int -> _string)
  vortex-frame-build-up-from-params-s)

(defvtx* (_fun _VortexFrameType _int _int
               _axl-bool _uint _int _int _string
               _string _pointer _int _string _int -> _string)
  vortex-frame-build-up-from-params-s-buffer)

(defvtx* (_fun _VortexFrame-pointer -> _VortexFrame-pointer)
  vortex-frame-copy)

(defvtx* (_fun _VortexCtx-pointer _VortexFrameType _int _int _axl-bool _uint
               _int _int _pointer -> _VortexFrame-pointer)
  vortex-frame-create)

(defvtx* (_fun _VortexCtx-pointer _VortexFrameType _int _int _axl-bool _uint
               _int _int _string  _string _pointer -> _VortexFrame-pointer)
  vortex-frame-create-full
  vortex-frame-create-full-ref)

(defvtx* (_fun _VortexFrame-pointer -> _void)
  vortex-frame-free
  vortex-frame-unref)

(defvtx* (_fun _VortexFrame-pointer -> _int)
  vortex-frame-get-ansno
  vortex-frame-get-channel
  vortex-frame-get-content-size
  vortex-frame-get-id
  vortex-frame-get-mime-header-size
  vortex-frame-get-more-flag
  vortex-frame-get-msgno
  vortex-frame-get-payload-size
  vortex-frame-ref-count)

(defvtx* (_fun _VortexFrame-pointer -> _VortexChannel-pointer)
  vortex-frame-get-channel-ref)

(defvtx* (_fun _int _string _string _string -> _string)
  vortex-frame-get-close-message)

(defvtx* (_fun _VortexFrame-pointer -> _string)
  vortex-frame-get-content
  vortex-frame-get-raw-frame
  vortex-frame-get-transfer-encoding)

(defvtx* (_fun _VortexFrame-pointer -> _VortexCtx-pointer)
  vortex-frame-get-ctx)

(defvtx* (_fun _string _string _string -> _string)
  vortex-frame-get-error-message)

(defvtx* (_fun _VortexFrame-pointer _string -> _VortexMimeHeader-pointer)
  vortex-frame-get-mime-header)

(defvtx* (_fun -> _string)
  vortex-frame-get-ok-message)

(defvtx* (_fun _VortexFrame-pointer -> _pointer)
  vortex-frame-get-payload)

(defvtx* (_fun _VortexFrame-pointer -> _uint)
  vortex-frame-get-seqno)

(defvtx* (_fun _int _string _string _VortexEncoding _string _int -> _string)
  vortex-frame-get-start-message)

(defvtx* (_fun _string _string -> _string)
  vortex-frame-get-start-rpy-message)

(defvtx* (_fun _VortexFrame-pointer -> _VortexFrameType)
  vortex-frame-get-type)

(defvtx* (_fun _VortexFrame-pointer (_ptr io _string) (_ptr io _string) -> _axl-bool)
  vortex-frame-is-error-message)

(defvtx* (_fun _VortexMimeHeader-pointer -> _string)
  vortex-frame-mime-header-content
  vortex-frame-mime-header-name)

(defvtx* (_fun _VortexMimeHeader-pointer -> _int)
  vortex-frame-mime-header-count)

(defvtx* (_fun _VortexMimeHeader-pointer -> _VortexMimeHeader-pointer)
  vortex-frame-mime-header-next)

(defvtx* (_fun _VortexFrame-pointer -> _axl-bool)
  vortex-frame-mime-process
  vortex-frame-ref)

(defvtx* (_fun _VortexConnection-pointer _string _int -> _int)
  vortex-frame-readline)

(defvtx* (_fun _int _uint _int -> _string)
  vortex-frame-seq-build-up-from-params)

(defvtx* (_fun _int _int _int _string _int (_ptr io _int) -> _string)
  vortex-frame-seq-build-up-from-params-buffer)

(defvtx* (_fun _VortexFrame-pointer _string _string _string -> _void)
  vortex-frame-set-mime-header)