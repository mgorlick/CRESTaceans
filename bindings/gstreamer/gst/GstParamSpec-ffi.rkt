#lang racket

(require "gst_base.rkt")

(provide (all-defined-out))

#|#define             GST_PARAM_CONTROLLABLE
#define             GST_PARAM_USER_SHIFT
#define             GST_PARAM_MUTABLE_PAUSED
#define             GST_PARAM_MUTABLE_PLAYING
#define             GST_PARAM_MUTABLE_READY|#
  
;GParamSpec* gst_param_spec_fraction (const gchar *name, const gchar *nick, const gchar *blurb, gint min_num, gint min_denom, gint max_num, gint max_denom, gint default_num, gint default_denom, GParamFlags flags);
(define-gstreamer gst_param_spec_fraction (_fun _string _string _string _gint _gint _gint _gint _gint _gint _int -> _GParamSpec-pointer))
