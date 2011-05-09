#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <vpx/vpx_encoder.h>
#include <vpx/vp8cx.h>

#include "enc_settings.h"

#define ROUND_UP_2(num) (((num)+1)&~1)
#define ROUND_UP_4(num) (((num)+3)&~3)
#define ROUND_UP_8(num) (((num)+7)&~7)
#define GEN_MASK(x) ((1<<(x))-1)
#define ROUND_UP_X(v,x) (((v) + GEN_MASK(x)) & ~GEN_MASK(x))
#define DIV_ROUND_UP_X(v,x) (((v) + GEN_MASK(x)) >> (x))

inline int get_offset (int component_index, int pic_width, int pic_height);
inline int get_row_stride (int component_index, int pic_width);
static void yuv422_to_yuv420p (unsigned char *dest, unsigned char *src,
                               int width, int height);

typedef struct VP8Enc {
  vpx_codec_ctx_t codec;
  vpx_image_t image;
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
  cfg.kf_min_dist = 0;
  cfg.kf_max_dist = 0;

  enc->width = cfg.g_w;
  enc->height = cfg.g_h;
  enc->n_frames = 0;

  vpx_img_alloc (&enc->image, VPX_IMG_FMT_I420, cfg.g_w, cfg.g_h, 0);
  enc->image.planes[0] = enc->image.img_data;
  for (i = 1; i < 3; i++) {
    printf ("planes[%d] offset = %d\n", i, get_offset (i, cfg.g_w, cfg.g_h));
    printf ("planes[%d] stride = %d\n", i, get_row_stride (i, cfg.g_w));
    enc->image.planes[i] = enc->image.planes[0] + get_offset (i, cfg.g_w, cfg.g_h);
    enc->image.stride[i] = get_row_stride (i, cfg.g_w);
  }
  
  status = vpx_codec_enc_init (&enc->codec, &vpx_codec_vp8_cx_algo, &cfg, 0);
  if (status != VPX_CODEC_OK) goto no_init;

  return enc;
    
no_config:
  printf ("Failed to get configuration: %s\n", vpx_codec_err_to_string (status));
  return NULL;

no_enc_alloc:
  printf ("Couldn't allocate encoder (malloc returned NULL)\n");
  return NULL;
  
no_init:
  printf ("Couldn't initialize codec: %s\n", vpx_codec_err_to_string (status));
  vpx_img_free (&enc->image);
  free (enc);
  return NULL;
}

/* Assume YUY2 for these calculations! */
void buffer_to_image_YUYV (size_t size, unsigned char *buffer, vpx_image_t *image) {

  int delta_cb, delta_cr;
  image->x_chroma_shift = image->y_chroma_shift = 1;
  
  delta_cb = ROUND_UP_4 (image->w) * ROUND_UP_X (image->h, 1);
  delta_cr = ROUND_UP_4 (DIV_ROUND_UP_X (image->w, 1))
      * DIV_ROUND_UP_X (image->h, 1);

  image->planes[VPX_PLANE_Y] = buffer;
  image->planes[VPX_PLANE_U] = image->planes[VPX_PLANE_Y] + delta_cb;
  image->planes[VPX_PLANE_V] = image->planes[VPX_PLANE_U] + delta_cr;
}

inline int get_offset (int component_index, int pic_width, int pic_height) {
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

inline int get_row_stride (int component_index, int pic_width) {
  return (component_index == 0) ?
      ROUND_UP_4 (pic_width) :
      ROUND_UP_4 (ROUND_UP_2 (pic_width) / 2);
}

int vp8enc_encode (VP8Enc *enc, size_t size, unsigned char *buffer,
                   unsigned char *out, size_t *written) {

  const vpx_codec_cx_pkt_t *pkt;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  int flags = 0;
  
  yuv422_to_yuv420p (enc->image.img_data, buffer, enc->width, enc->height);
  
  /* XXX fixme set force-keyframe flag only when commanded */
  flags |= VPX_EFLAG_FORCE_KF;
  
  status = vpx_codec_encode (&enc->codec, &enc->image, enc->n_frames++, 1, flags, 10000);
  if (status != VPX_CODEC_OK) goto no_frame;
  
  while (NULL != (pkt = vpx_codec_get_cx_data (&enc->codec, &iter))) {
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
                               unsigned char *src,
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
