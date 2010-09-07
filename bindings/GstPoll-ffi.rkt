#lang racket

(require "gst_base.rkt"
         "GstClock-ffi.rkt")

(provide (all-defined-out))

;;typedef struct _GstPoll GstPoll;
(define-cpointer-type _GstPoll-pointer)

;;typedef struct {
;;  int fd;
;;} GstPollFD;

(define-cstruct _GstPollFD
  ([fd _int]))

;#define             GST_POLL_FD_INIT


;;GstPoll* GstPollFD*  -> gboolean
(define-gstreamer*
  (_fun _GstPoll-pointer _GstPollFD-pointer -> _gboolean)
  gst_poll_add_fd gst_poll_fd_can_read gst_poll_fd_can_write gst_poll_fd_has_closed gst_poll_fd_has_error gst_poll_remove_fd)


;;GstPoll* GstPollFD* _gboolean  -> gboolean
(define-gstreamer*
  (_fun _GstPoll-pointer _GstPollFD-pointer _gboolean -> _gboolean)
  gst_poll_fd_ctl_read gst_poll_fd_ctl_write)


;void                gst_poll_fd_ignored                 (GstPoll *set, GstPollFD *fd);
(define-gstreamer gst_poll_fd_ignored (_fun _GstPoll-pointer _GstPollFD-pointer -> _void))

;void                gst_poll_fd_init                    (GstPollFD *fd);
(define-gstreamer gst_poll_fd_init (_fun _GstPollFD-pointer -> _void))

;;GstPoll*  -> void
(define-gstreamer*
  (_fun _GstPoll-pointer -> _void)
  gst_poll_free gst_poll_restart)

;GstPoll*            gst_poll_new                        (gboolean controllable);
(define-gstreamer gst_poll_new (_fun _gboolean -> _GstPoll-pointer))

;GstPoll*            gst_poll_new_timer                  (void);
(define-gstreamer gst_poll_new_timer (_fun -> _GstPoll-pointer))

;gboolean            gst_poll_set_controllable           (GstPoll *set, gboolean controllable);
(define-gstreamer gst_poll_set_controllable (_fun _GstPoll-pointer _gboolean -> _gboolean))

;void                gst_poll_set_flushing               (GstPoll *set, gboolean flushing);
(define-gstreamer gst_poll_set_flushing (_fun _GstPoll-pointer _gboolean -> _void))

;gint                gst_poll_wait                       (GstPoll *set, GstClockTime timeout);
(define-gstreamer gst_poll_wait (_fun _GstPoll-pointer _GstClockTime -> _gint))

;;GstPoll*  -> gboolean
(define-gstreamer*
  (_fun _GstPoll-pointer -> _gboolean)
  gst_poll_read_control gst_poll_write_control)
