#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <libswscale/swscale.h>
#include <vpx/vpx_encoder.h>
#include <vpx/vp8cx.h>

int ENCODER_SPEEX = VPX_DL_REALTIME;
int THREADS = 8;

typedef struct VP8Enc {
  vpx_codec_ctx_t codec;
  vpx_image_t *image;
  int width;
  int height;
  vpx_codec_pts_t n_frames;

  struct SwsContext *swsctx;
} VP8Enc;

void vp8enc_delete (VP8Enc *enc) {
  if (enc == NULL) return;
  if (enc->image != NULL) vpx_img_free (enc->image);
  if (enc->swsctx != NULL) sws_freeContext (enc->swsctx);
  free (enc);
}

VP8Enc* vp8enc_new (int enc_frame_width, int enc_frame_height,
                    int enc_fps_numerator, int enc_fps_denominator) {
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
  cfg.g_timebase.num = enc_fps_numerator;
  cfg.g_timebase.den = enc_fps_denominator;

  /* keyframes: in theory this sets the keyframe rate minimum at one/sec */
  cfg.kf_mode = VPX_KF_AUTO;
  cfg.kf_min_dist = 0;
  cfg.kf_max_dist = enc_fps_denominator;

  /* QUALITY SETTINGS */

  /* scale bitrate linearly with width for now */
  cfg.rc_target_bitrate = cfg.g_w;
  cfg.g_threads = THREADS;
  /* these three settings disable the behavior where the first ~10-20MB
     of data gets produced in 50-75KB chunks, therefore speeding up
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
    if (status != VPX_CODEC_OK) 
      printf ("vpx encoder: ouldn't set setting %d, proceeding anyway (status %d)\n",
	      i, status);
  }

  /* pixel-format-specific offset and stride settings */
  enc->image = vpx_img_alloc (NULL, VPX_IMG_FMT_VPXI420, cfg.g_w, cfg.g_h, 0);

  /* tracking values for conversion and encoding later */
  enc->width = cfg.g_w;
  enc->height = cfg.g_h;
  enc->n_frames = 0;

  enc->swsctx = sws_getContext (enc->image->w, enc->image->h, PIX_FMT_YUYV422, 
				enc->image->w, enc->image->h, PIX_FMT_YUV420P,
				1, NULL, NULL, NULL);
  if (enc->swsctx == NULL)
    goto no_sws;
  
  return enc;

 no_sws:
  printf ("Could not initialize libswscale\n");
  return NULL;
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

void convert_yuv422_to_yuv420p (VP8Enc *enc, const unsigned char *buffer) {
  const uint8_t *src_slices[3] = { buffer, buffer + 1, buffer + 3 };
  int stride422 = enc->width * 2;
  const int src_stride[3] = { stride422, stride422, stride422 };

  sws_scale (enc->swsctx, src_slices, src_stride, 0, enc->image->h,
	     enc->image->planes, enc->image->stride );

}

int vp8enc_encode (VP8Enc *enc,
                   const size_t size, const unsigned char *buffer,
                   const size_t outsize, unsigned char *out,
                   size_t *written) {

  const vpx_codec_cx_pkt_t *pkt;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  vpx_fixed_buf_t fbuff = { out, outsize };
  vpx_enc_frame_flags_t flags = 0;
  int need_extra_copy = 0;
  
  convert_yuv422_to_yuv420p (enc, buffer);
 
  /* XXX fixme set force-keyframe flag only when commanded */
  /* if (enc->n_frames % 7 == 0) flags |= VPX_EFLAG_FORCE_KF; */
  
  status = vpx_codec_encode (&enc->codec, enc->image, enc->n_frames++, 
			     1, flags, VPX_DL_REALTIME);
  if (status != VPX_CODEC_OK) goto no_frame;

  status = vpx_codec_set_cx_data_buf (&enc->codec, &fbuff, 0, 0);
  if (status != VPX_CODEC_OK) need_extra_copy = 1;

  /* XXX fixme this only takes the first packet from the iterator
     and discards the rest corresponding to the input buffer */
  if (NULL != (pkt = vpx_codec_get_cx_data (&enc->codec, &iter))) {
    switch (pkt->kind) {
      case VPX_CODEC_CX_FRAME_PKT:
        if (need_extra_copy) memcpy (out, pkt->data.frame.buf, pkt->data.frame.sz);
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
