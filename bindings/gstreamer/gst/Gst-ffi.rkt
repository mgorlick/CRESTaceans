#lang racket

(require ffi/unsafe
         "gst_base.rkt")

(provide (all-defined-out))

;;void   gst_init (int *argc, char **argv[]);
(define-gstreamer gst_init (_fun (_or-null (_ptr io _int)) (_or-null (_ptr io (_list i _string))) -> _void))

;gboolean gst_init_check (int *argc, char **argv[], GError ** err);
(define-gstreamer gst_init_check (_fun (_or-null (_ptr io _int))
                                       (_or-null (_ptr io (_list i _string)))
                                       (error : (_ptr i _GError-pointer))
                                       -> (result : _gboolean)
                                       -> (values result error)))

;;GOptionGroup *      gst_init_get_option_group (void);                      OJO * means pointer
(define-gstreamer gst_init_get_option_group (_fun -> _GOptionGroup-pointer))

;gboolean            gst_is_initialized                  (void); USED IN LATER VERSION - uncomment for version 10.30.2
;(define-gstreamer gst_is_initialized (_fun -> _gboolean))

;;void gst_deinit (void);
(define-gstreamer gst_deinit (_fun -> _void))

;;void gst_version (guint *major, guint *minor, guint *micro, guint *nano);
(define-gstreamer gst_version (_fun (major : (_ptr o _guint)) (minor : (_ptr o _guint)) (micro : (_ptr o _guint)) (nano : (_ptr o _guint)) -> _void -> (values major minor micro nano)))

;;gchar *             gst_version_string                  (void);
(define-gstreamer gst_version_string (_fun -> _string))

;;VOID -> GBOOLEAN
(define-gstreamer*
  (_fun -> _gboolean)
  gst_segtrap_is_enabled gst_registry_fork_is_enabled gst_update_registry)

;;GBOOLEAN -> VOID
(define-gstreamer*
  (_fun _gboolean -> _void)
  gst_segtrap_set_enabled gst_registry_fork_set_enabled)