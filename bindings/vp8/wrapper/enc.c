#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <vpx/vpx_encoder.h>
#include <vpx/vp8cx.h>

#include "enc_settings.h"

#define ROUND_UP_2(num) (((num)+1)&~1)
#define ROUND_UP_4(num) (((num)+3)&~3)
#define ROUND_UP_8(num) (((num)+7)&~7)
#define GEN_MASK(x) ((1<<(x))-1)
#define ROUND_UP_X(v,x) (((v) + GEN_MASK(x)) & ~GEN_MASK(x))
#define DIV_ROUND_UP_X(v,x) (((v) + GEN_MASK(x)) >> (x))

inline int get_offset (const int component_index, const int pic_width, const int pic_height);
inline int get_row_stride (const int component_index, const int pic_width);
static void yuv422_to_yuv420p (unsigned char *dest, const unsigned char *src,
                               const int width, const int height);

typedef struct VP8Enc {
  vpx_codec_ctx_t codec;
  vpx_image_t *image;
  int width;
  int height;
  int n_frames;
} VP8Enc;

VP8Enc* vp8enc_new (void) {
  int i;
  vpx_codec_err_t status;
  vpx_codec_enc_cfg_t cfg;
  VP8Enc *enc;

  status = vpx_codec_enc_config_default (&vpx_codec_vp8_cx_algo, &cfg, 0);
  if (status != VPX_CODEC_OK) goto no_config;

  enc = malloc (sizeof (VP8Enc));
  if (enc == NULL) goto no_enc_alloc;

  cfg.g_w = enc_frame_width;
  cfg.g_h = enc_frame_height;
  cfg.g_timebase.num = enc_fps_denominator;
  cfg.g_timebase.den = enc_fps_numerator;

  /* keyframes: in theory this sets the keyframe rate minimum at one/sec */
  cfg.kf_mode = VPX_KF_AUTO;
  cfg.kf_min_dist = 0;
  cfg.kf_max_dist = enc_fps_numerator;

  /* quality settings */
  /* the target bitrate here is a little bit arbitrary, but it offers
     a decent balance. probably needs to change if res increases */
  cfg.rc_target_bitrate = cfg.g_w * cfg.g_h / 500;
  cfg.g_threads = 4;
  /* these three settings disable the behavior where the first ~10-20MB
     of data gets produced in 50-75MB chunks, therefore speeding up
     the start of stream and reducing variation in picture quality */
  cfg.g_pass = VPX_RC_ONE_PASS;
  cfg.g_lag_in_frames = 0;
  cfg.g_usage = VPX_CQ;

  status = vpx_codec_enc_init (&enc->codec, &vpx_codec_vp8_cx_algo, &cfg, 0);
  if (status != VPX_CODEC_OK) goto no_init;

  int ctrls[] = { VP8E_SET_CQ_LEVEL, VP8E_SET_TUNING };
  int vals[] = { 63, VP8_TUNE_PSNR };
  for (i = 0; i < sizeof (ctrls) / sizeof (*ctrls); i++) {
    status = vpx_codec_control_ (&enc->codec, ctrls[i], vals[i]);
    if (status != VPX_CODEC_OK) printf ("Couldn't set setting %d, proceeding anyway\n", i);
  }

  /* pixel-format-specific offset and stride settings */
  enc->image = vpx_img_alloc (NULL, VPX_IMG_FMT_VPXI420, cfg.g_w, cfg.g_h, 0);
  for (i = 0; i < 3; i++) {
    enc->image->planes[i] = enc->image->img_data + get_offset (i, cfg.g_w, cfg.g_h);
    enc->image->stride[i] = get_row_stride (i, cfg.g_w);
  }

  /* tracking values for conversion and encoding later */
  enc->width = cfg.g_w;
  enc->height = cfg.g_h;
  enc->n_frames = 0;

  return enc;
    
no_config:
  printf ("Failed to get configuration: %s\n", vpx_codec_err_to_string (status));
  return NULL;

no_enc_alloc:
  printf ("Couldn't allocate encoder (malloc returned NULL)\n");
  return NULL;
  
no_init:
  printf ("Couldn't initialize codec: %s\n", vpx_codec_err_to_string (status));
  free (enc);
  return NULL;
}

inline int get_offset (const int component_index, const int pic_width, const int pic_height) {
  switch (component_index) {
    case 0:
      return 0;
    case 1:
      return ROUND_UP_4 (pic_width) * ROUND_UP_2 (pic_height);
    case 2:
      return ROUND_UP_4 (pic_width) * ROUND_UP_2 (pic_height) +
        ROUND_UP_4 (ROUND_UP_2 (pic_width) / 2) *
          (ROUND_UP_2 (pic_height) / 2);
    default: /* error */
      return 0;
  }
}

inline int get_row_stride (const int component_index, const int pic_width) {
  return (component_index == 0) ?
      ROUND_UP_4 (pic_width) :
      ROUND_UP_4 (ROUND_UP_2 (pic_width) / 2);
}

int vp8enc_encode (VP8Enc *enc, size_t size, const unsigned char *buffer,
                   unsigned char *out, size_t *written) {

  const vpx_codec_cx_pkt_t *pkt;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  int flags = 0;
  
  yuv422_to_yuv420p (enc->image->img_data, buffer, enc->width, enc->height);

  /* XXX fixme set force-keyframe flag only when commanded */
  /* if (enc->n_frames % 7 == 0) flags |= VPX_EFLAG_FORCE_KF; */
  
  status = vpx_codec_encode (&enc->codec, enc->image, enc->n_frames++, 1, flags, VPX_DL_REALTIME);
  if (status != VPX_CODEC_OK) goto no_frame;

  /* XXX fixme this only takes the first packet from the iterator
     and discards the rest corresponding to the input buffer */
  if (NULL != (pkt = vpx_codec_get_cx_data (&enc->codec, &iter))) {
    switch (pkt->kind) {
      case VPX_CODEC_CX_FRAME_PKT:
        memcpy (out, pkt->data.frame.buf, pkt->data.frame.sz);
        *written = pkt->data.frame.sz;
        break;
      default:
        printf ("Found some non-data packet, type %d\n", pkt->kind);
        break;
    }
  }

  return 1;

no_frame:
  printf ("Couldn't encode buffer: %s\n", vpx_codec_err_to_string (status));
  return 0;
}

/* this function pieced together from gstreamer ffmpegcolorspace component */
static void yuv422_to_yuv420p (unsigned char *dest,
                               const unsigned char *src,
                               int width, int height) {
  const unsigned char *p, *p1;
  unsigned char *lum, *cr, *cb, *lum1, *cr1, *cb1;
  int w;
  
  int dest_stride[3] = { get_row_stride (0, width),
                         get_row_stride (1, width),
                         get_row_stride (2, width) };

  /* this stuff specific to I420 and others in family */
  int x_chroma_shift = 1;
  int y_chroma_shift = 1;
  int offset_cb = ROUND_UP_4 (width) * ROUND_UP_X (height, y_chroma_shift);
  int offset_cr = ROUND_UP_4 (DIV_ROUND_UP_X (width, x_chroma_shift))
      * DIV_ROUND_UP_X (height, y_chroma_shift);
  
  /* src_stride calculation specific to YUV422 and others in family */
  int src_stride = ROUND_UP_4 (width * 2);
  
  p1 = src;
  lum1 = dest;
  cb1 = dest + offset_cb;
  cr1 = cb1 + offset_cr;
  
  for (; height >= 1; height -= 2) {
    p = p1;
    lum = lum1;
    cb = cb1;
    cr = cr1;
    for (w = width; w >= 2; w -= 2) {
      lum[0] = p[0];
      cb[0] = p[1];
      lum[1] = p[2];
      cr[0] = p[3];
      p += 4;
      lum += 2;
      cb++;
      cr++;
    }
    if (w) {
      lum[0] = p[0];
      cb[0] = p[1];
      cr[0] = p[3];
    }
    p1 += src_stride;
    lum1 += dest_stride[0];
    if (height > 1) {
      p = p1;
      lum = lum1;
      for (w = width; w >= 2; w -= 2) {
        lum[0] = p[0];
        lum[1] = p[2];
        p += 4;
        lum += 2;
      }
      if (w) {
        lum[0] = p[0];
      }
      p1 += src_stride;
      lum1 += dest_stride[0];
    }
    cb1 += dest_stride[1];
    cr1 += dest_stride[2];
  }
}
