#lang racket

(require ffi/unsafe
         "libvortex.rkt")
(provide (all-defined-out))

(defvtx vortex-channel-are-equal 
  (_fun _VortexChannel-pointer _VortexChannel-pointer -> _axl-bool))

(defvtx* (_fun _VortexChannel-pointer _long -> _axl-bool)
  vortex-channel-block-until-replies-are-sent
  )

(defvtx* (_fun _VortexChannel-pointer _VortexOnClosedNotification -> _axl-bool)
  vortex-channel-close)

(defvtx* (_fun _VortexChannel-pointer _VortexOnClosedNotificationFull _axlPointer -> _axl-bool)
  vortex-channel-close-full)

(defvtx* (_fun -> _WaitReplyData-pointer)
  vortex-channel-create-wait-reply)

(defvtx* (_fun _VortexChannel-pointer -> _void)
  vortex-channel-defer-start
  vortex-channel-free
  vortex-channel-unref)

(defvtx* (_fun _VortexChannel-pointer _axlPointer -> _void)
  vortex-channel-delete-data)

(defvtx* (_fun _VortexChannel-pointer _int -> _axl-bool)
  vortex-channel-finalize-ans-rpy)

(defvtx* (_fun _VortexChannel-pointer _int -> _void)
  vortex-channel-set-window-size)

(defvtx* (_fun _VortexChannel-pointer _axl-bool -> _void)
  vortex-channel-set-complete-flag
  vortex-channel-set-serialize
  vortex-channel-flag-reply-processed)

(defvtx* (_fun _WaitReplyData-pointer -> _void)
  vortex-channel-free-wait-reply)

(defvtx* (_fun _VortexChannel-pointer -> _int)
  vortex-channel-get-automatic-mime
  vortex-channel-get-last-msg-no-received
  vortex-channel-get-next-expected-reply-no
  vortex-channel-get-next-msg-no
  vortex-channel-get-next-reply-no
  vortex-channel-get-number
  vortex-channel-get-window-size
  vortex-channel-ref-count)

(defvtx* (_fun _VortexChannel-pointer -> _VortexConnection-pointer)
  vortex-channel-get-connection)

(defvtx* (_fun _VortexChannel-pointer -> _VortexCtx-pointer)
  vortex-channel-get-ctx)

(defvtx* (_fun _VortexChannel-pointer _axlPointer -> _axlPointer)
  vortex-channel-get-data)

(defvtx* (_fun _VortexChannel-pointer -> _string)
  vortex-channel-get-mime-type
  vortex-channel-get-profile
  vortex-channel-get-transfer-encoding)

(defvtx* (_fun _VortexChannel-pointer -> _uint)
  vortex-channel-get-next-ans-no
  vortex-channel-get-next-expected-ans-no
  vortex-channel-get-next-expected-seq-no
  vortex-channel-get-next-seq-no)

(defvtx* (_fun _VortexChannel-pointer _uint _int _uint -> _int)
  vortex-channel-get-next-frame-size)

(defvtx* (_fun _VortexChannel-pointer (_ptr io _int) -> _int)
  vortex-channel-get-outstanding-messages)

(defvtx* (_fun _VortexChannel-pointer -> _VortexFrame-pointer)
  vortex-channel-get-piggyback)

(defvtx* (_fun _VortexChannel-pointer -> _VortexChannelPool-pointer)
  vortex-channel-get-pool)

(defvtx* (_fun _VortexChannel-pointer _VortexAsyncQueue-pointer -> _VortexFrame-pointer)
  vortex-channel-get-reply)

(defvtx* (_fun _VortexChannel-pointer -> _axl-bool)
  vortex-channel-have-complete-flag
  vortex-channel-have-piggyback
  vortex-channel-is-being-closed
  vortex-channel-is-defined-close-handler
  vortex-channel-is-defined-received-handler
  vortex-channel-is-empty-pending-message
  vortex-channel-is-opened
  vortex-channel-is-ready
  vortex-channel-ref)

(defvtx* (_fun _VortexChannel-pointer _string -> _axl-bool)
  vortex-channel-is-running-profile)

(defvtx* (_fun _VortexConnection-pointer _int _string _VortexOnCloseChannel
               _axlPointer _VortexOnFrameReceived _axlPointer _VortexOnChannelCreated
               _axlPointer -> _VortexChannel-pointer)
  vortex-channel-new)

(defvtx* (_fun _VortexConnection-pointer _int _string _string _VortexEncoding
               _string _int _VortexOnCloseChannel _axlPointer _VortexOnFrameReceived
               _axlPointer _VortexOnChannelCreated _axlPointer -> _VortexChannel-pointer)
  vortex-channel-new-full)

(defvtx* (_fun _VortexChannel-pointer _int _axl-bool -> _void)
  vortex-channel-notify-close
  vortex-channel-set-outstanding-limit)

(defvtx* (_fun _VortexChannel-pointer _string _axl-bool -> _axl-bool)
  vortex-channel-notify-start)

(defvtx* (_fun _VortexChannel-pointer _VortexConnection-pointer _VortexFrame-pointer _axlPointer -> _void)
  vortex-channel-queue-reply)

(defvtx* (_fun (c m i) ::
               (c : _VortexChannel-pointer)
               (m : _bytes)
               (_int = (bytes-length m))
               (i : _int) -> _axl-bool)
  vortex-channel-send-ans-rpy
  vortex-channel-send-err
  vortex-channel-send-rpy)

(defvtx* (_fun (c m) :: 
               (c : _VortexChannel-pointer)
               (m : _bytes)
               (_int = (bytes-length m))
               (p : (_ptr o _int))
               -> (r : _axl-bool)
               -> (values r p))
  vortex-channel-send-msg)

(defvtx* (_fun (c m w) :: 
               (c : _VortexChannel-pointer)
               (m : _bytes)
               (_int = (bytes-length m))
               (p : (_ptr o _int))
               (w : _WaitReplyData-pointer)
               -> (r : _axl-bool)
               -> (values r p))
  vortex-channel-send-msg-and-wait)

; send-[something]*: versions of the send-* functions above that take Racket strings
; and convert them to ASCII-encoded bytes before passing off to
; send-[something]. Needed for conventional string sending.

; (the versions above are for sending non-nul-terminated bytestrings (const void *),
; including binary data encoded in bytestreams)

; never supply a length. all these functions compute the length themselves.
(define (vortex-channel-send-rpy* c m i)
  (vortex-channel-send-rpy c (string->bytes/latin-1 m) i))

(define (vortex-channel-send-ans-rpy* c m i)
  (vortex-channel-send-ans-rpy c (string->bytes/latin-1 m) i))

(define (vortex-channel-send-err* c m i)
  (vortex-channel-send-err c (string->bytes/latin-1 m) i))

(define (vortex-channel-send-msg-and-wait* c m w)
  (vortex-channel-send-msg-and-wait c (string->bytes/latin-1 m) w))

(define (vortex-channel-send-msg* c m)
  (vortex-channel-send-msg c (string->bytes/latin-1 m)))

(defvtx* (_fun _VortexChannel-pointer (_or-null (_ptr io _int)) _string -> _axl-bool)
  vortex-channel-send-msgv)

(defvtx* (_fun _VortexChannel-pointer _int _string -> _axl-bool)
  vortex-channel-send-rpyv)

(defvtx* (_fun _VortexChannel-pointer _VortexOnCloseChannel _axlPointer -> _void)
  vortex-channel-set-close-handler)

(defvtx* (_fun _VortexChannel-pointer _VortexOnNotifyCloseChannel _axlPointer -> _void)
  vortex-channel-set-close-notify-handler)

(defvtx* (_fun _VortexChannel-pointer _VortexOnClosedChannel _axlPointer -> _void)
  vortex-channel-set-closed-handler)

(defvtx* (_fun _VortexChannel-pointer _axlPointer _axlPointer -> _void)
  vortex-channel-set-data)

(defvtx* (_fun _VortexChannel-pointer _axlPointer _axlPointer _axlDestroyFunc _axlDestroyFunc -> _void)
  vortex-channel-set-data-full)

(defvtx* (_fun _VortexChannel-pointer _VortexChannelFrameSize _axlPointer -> _VortexChannelFrameSize)
  vortex-channel-set-next-frame-size-handler)

(defvtx* (_fun _VortexChannel-pointer _VortexOnFrameReceived _axlPointer -> _void)
  vortex-channel-set-received-handler)

(defvtx* (_fun _VortexFrame-pointer (_ptr io _string) (_ptr io _string) -> _axl-bool)
  vortex-channel-validate-err)

(defvtx* (_fun _VortexChannel-pointer _int _WaitReplyData-pointer -> _VortexFrame-pointer)
  vortex-channel-wait-reply)

(defvtx* (_fun _WaitReplyData-pointer -> _void)
  vortex-channel-wait-reply-ref)

(defvtx* (_fun _VortexCtx-pointer -> _void)
  vortex-channel-init)

(defvtx* (_fun _VortexChannel-pointer _VortexPayloadFeeder-pointer _int -> _axl-bool)
  vortex-channel-send-rpy-from-feeder)