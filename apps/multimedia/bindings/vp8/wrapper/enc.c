#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <libswscale/swscale.h>
#include <vpx/vpx_encoder.h>
#include <vpx/vp8cx.h>

int ENCODER_SPEED = VPX_DL_REALTIME;

typedef struct VP8Enc {
  vpx_codec_ctx_t *codec;
  vpx_image_t *image;
  int width;
  int height;
  vpx_codec_pts_t n_frames;
  struct SwsContext *swsctx;
} VP8Enc;

void vp8enc_delete (VP8Enc *enc) {
  if (enc == NULL) return;
  if (enc->codec != NULL) free (enc->codec);
  if (enc->image != NULL) vpx_img_free (enc->image);
  if (enc->swsctx != NULL) sws_freeContext (enc->swsctx);
  free (enc);
}

VP8Enc* vp8enc_new (int num_threads, 
		    int enc_frame_width, int enc_frame_height,
                    int enc_fps_numerator, int enc_fps_denominator) {
  unsigned int i;
  vpx_codec_err_t status;
  vpx_codec_enc_cfg_t cfg;
  VP8Enc *enc;

  status = vpx_codec_enc_config_default (&vpx_codec_vp8_cx_algo, &cfg, 0);
  if (status != VPX_CODEC_OK) goto no_config;

  enc = malloc (sizeof (VP8Enc));
  if (enc == NULL) goto no_enc_alloc;

  enc->codec = malloc (sizeof (vpx_codec_ctx_t));

  cfg.g_w = enc_frame_width;
  cfg.g_h = enc_frame_height;
  cfg.g_timebase.num = enc_fps_numerator;
  cfg.g_timebase.den = enc_fps_denominator;

  /* keyframes: in theory this sets the keyframe rate minimum at one/sec */
  cfg.kf_mode = VPX_KF_AUTO;
  cfg.kf_min_dist = 0;
  cfg.kf_max_dist = enc_fps_denominator;

  /* QUALITY SETTINGS */

  cfg.rc_target_bitrate = cfg.g_w;
  cfg.g_threads = num_threads;
  /* these three settings disable the behavior where the first ~10-20MB
     of data gets produced in 50-75KB chunks, therefore speeding up
     the start of stream and reducing variation in picture quality */
  cfg.g_pass = VPX_RC_ONE_PASS;
  cfg.g_lag_in_frames = 0;
  cfg.g_usage = VPX_CQ;

  status = vpx_codec_enc_init (enc->codec, &vpx_codec_vp8_cx_algo, &cfg, 0);
  if (status != VPX_CODEC_OK) goto no_init;

  int ctrls[] = { VP8E_SET_CQ_LEVEL, VP8E_SET_TUNING };
  int vals[] = { 63, VP8_TUNE_SSIM };
  for (i = 0; i < sizeof (ctrls) / sizeof (*ctrls); i++) {
    status = vpx_codec_control_ (enc->codec, ctrls[i], vals[i]);
    if (status != VPX_CODEC_OK) 
      printf ("vpx encoder: couldn't set setting %d, proceeding anyway (status %d)\n", i, status);
  }

  /* pixel-format-specific offset and stride settings */
  enc->image = vpx_img_alloc (NULL, VPX_IMG_FMT_YV12, cfg.g_w, cfg.g_h, 0);

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

int encode_image (VP8Enc *enc,
		  const size_t outsize, unsigned char *out,
		  size_t *written) {

  const vpx_codec_cx_pkt_t *pkt;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  vpx_fixed_buf_t fbuff = { out, outsize };
  vpx_enc_frame_flags_t flags = 0;
  int need_extra_copy = 0;
   
  status = vpx_codec_encode (enc->codec, enc->image, enc->n_frames++, 
			     1, flags, ENCODER_SPEED);
  if (status != VPX_CODEC_OK) goto no_frame;

  status = vpx_codec_set_cx_data_buf (enc->codec, &fbuff, 0, 0);
  if (status != VPX_CODEC_OK) need_extra_copy = 1;

  /* XXX fixme this only takes the first packet from the iterator
     and discards the rest corresponding to the input buffer */
  if (NULL != (pkt = vpx_codec_get_cx_data (enc->codec, &iter))) {
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

int vp8enc_encode_quarter (VP8Enc *enc, const int qtr_row, const int qtr_col,
			   const size_t size, const unsigned char *buffer,
			   const size_t outsize, unsigned char *out,
			   size_t *written) {
  
  if (qtr_row < 0 || qtr_row > 1 || qtr_col < 0 || qtr_col > 1) {
    printf ("Error: quarter coordinates must be in range (0,0) - (1,1)\n");
    return 0;
  }

  if (enc->width * enc->height * 2 != (int) size / 4) {
    printf ("Error: either the encoder was not initialized for quarter-size encoding \
or the input buffer size is wrong.\n");
    return 0;
  }

  /* all the remaining code in this procedure assumes that the VP8Enc was
   * correctly initialized for quarter-frame encoding.
   */

  unsigned char tmp[enc->width * enc->height * 2]; // should be the same as size / 4
  const uint8_t *src_slices[3] = { tmp, tmp + 1, tmp + 3 };
  int stride422 = enc->width * 2;
  const int src_stride[3] = { stride422, stride422, stride422 };

  // copy the quarter of the frame we're interested in.
  int row_modifier = qtr_row == 0 ? 0 : enc->height;
  int col_modifier = qtr_col == 0 ? 0 : enc->width * 2;
  
  int row = 0;
  const unsigned char *input_cursor;
  unsigned char *output_cursor;
  
  for (row = 0; row < enc->height; row++) {
    input_cursor = buffer + stride422*2*(row + row_modifier) + col_modifier;
    output_cursor = tmp + stride422*row;
    memcpy (output_cursor, input_cursor, stride422);
  }
  
  sws_scale (enc->swsctx, src_slices, src_stride, 0, enc->image->h,
	     enc->image->planes, enc->image->stride);

  return encode_image (enc, outsize, out, written);
}

int vp8enc_encode (VP8Enc *enc,
                   const size_t size, const unsigned char *buffer,
                   const size_t outsize, unsigned char *out,
                   size_t *written) {

  if (enc->width * enc->height * 2 != (int) size) {
    printf ("Error: VP8Enc was not set up for full-size encoding \
or input buffer size is wrong.\n");
    return 0;
  }

  const uint8_t *src_slices[3] = { buffer, buffer + 1, buffer + 3 };
  int stride422 = enc->width * 2;
  const int src_stride[3] = { stride422, stride422, stride422 };
  
  sws_scale (enc->swsctx, src_slices, src_stride, 0, enc->image->h,
	     enc->image->planes, enc->image->stride);

  return encode_image (enc, outsize, out, written);
}
