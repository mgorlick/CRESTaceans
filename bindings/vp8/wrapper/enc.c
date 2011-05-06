#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define VPX_CODEC_DISABLE_COMPAT 1
#include <vpx/vpx_encoder.h>
#include <vpx/vp8cx.h>

#include "enc_settings.h"

#define ROUND_UP_2(num) (((num)+1)&~1)
#define ROUND_UP_4(num) (((num)+3)&~3)
#define ROUND_UP_8(num) (((num)+7)&~7)
#define GEN_MASK(x) ((1<<(x))-1)
#define ROUND_UP_X(v,x) (((v) + GEN_MASK(x)) & ~GEN_MASK(x))
#define DIV_ROUND_UP_X(v,x) (((v) + GEN_MASK(x)) >> (x))

typedef struct VP8Enc {
  vpx_codec_ctx_t codec;
  vpx_image_t image;
  int width;
  int height;
  int n_frames;
} VP8Enc;

typedef void (*vp8enc_foreach_frame) (long size, uint8_t *buffer);

void reprep_YUYV (vpx_image_t *image) {

  int delta_cb, delta_cr, s;

  /* salvage s from the alloc fun to check alloc size */
  s = image->stride[VPX_PLANE_Y];
  printf ("size of prealloced buffer = %d\n",
          (image->fmt & VPX_IMG_FMT_PLANAR) ?
          image->h * image->w * image->bps / 8 :
          image->h * s);

  /* YUYV support in vp8 is broken, it thinks xcs and ycs for YUYV = 0 */
  image->x_chroma_shift = image->y_chroma_shift = 1;
  delta_cb = ROUND_UP_4 (image->w) * ROUND_UP_X (image->h, image->y_chroma_shift);
  delta_cr = ROUND_UP_4 (DIV_ROUND_UP_X (image->w, image->x_chroma_shift))
      * DIV_ROUND_UP_X (image->h, image->y_chroma_shift);
}

VP8Enc* vp8enc_new (void) {
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
  cfg.kf_max_dist = (1<<enc_keyframe_granule_shift);

  enc->width = cfg.g_w;
  enc->height = cfg.g_h;
  enc->n_frames = 1;

  vpx_img_alloc (&enc->image, VPX_IMG_FMT_YUY2, cfg.g_w, cfg.g_h, 0);
  /* reprep_YUYV (&enc->image); */
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
  vpx_img_free (&(enc->image));
  free (enc);
  return NULL;
}

/* Assume YUY2 for these calculations! */
void buffer_to_image_YUYV (long size, unsigned char *buffer, vpx_image_t *image) {

  int delta_cb, delta_cr;

  delta_cb = ROUND_UP_4 (image->w) * ROUND_UP_X (image->h, 1);
  delta_cr = ROUND_UP_4 (DIV_ROUND_UP_X (image->w, 1))
      * DIV_ROUND_UP_X (image->h, 1);

  image->x_chroma_shift = image->y_chroma_shift = 1;

  image->planes[VPX_PLANE_Y] = buffer;
  image->planes[VPX_PLANE_U] = image->planes[VPX_PLANE_Y] + delta_cb;
  image->planes[VPX_PLANE_V] = image->planes[VPX_PLANE_U] + delta_cr;
  
  printf ("wxh: %dx%d / dwxdw: %dx%d\n", image->w, image->h, image->d_w, image->d_h);
  printf ("xcs: %d, ycs: %d\n", image->x_chroma_shift, image->y_chroma_shift);
  printf ("bps: %d\n", image->bps);
  printf ("Cb - Y = %d, Cr - Cb = %d\n",
          image->planes[VPX_PLANE_U] - image->planes[VPX_PLANE_Y],
          image->planes[VPX_PLANE_V] - image->planes[VPX_PLANE_U]);
  printf ("strides: %d, %d, %d\n",
          image->stride[VPX_PLANE_Y], image->stride[VPX_PLANE_U],
          image->stride[VPX_PLANE_V]);
}

int vp8enc_encode (VP8Enc *enc, long size, unsigned char *buffer,
                   vp8enc_foreach_frame f) {

  const vpx_codec_cx_pkt_t *pkt;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  int flags = 0;
  
  buffer_to_image_YUYV (size, buffer, &enc->image);
  printf ("encoding buffer %d, size %ld\n", enc->n_frames, size);
  /* XXX Fixme set force keyframe flag here */
  status = vpx_codec_encode (&enc->codec, &enc->image, enc->n_frames++, 1, flags, 10000);
  if (status != VPX_CODEC_OK) goto no_frame;
  
  while (NULL != (pkt = vpx_codec_get_cx_data (&enc->codec, &iter))) {
    switch (pkt->kind) {
      case VPX_CODEC_CX_FRAME_PKT:
        f (pkt->data.frame.sz, pkt->data.frame.buf);
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
