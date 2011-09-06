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

  unsigned char tmp[size];
  const uint8_t *src_slices[3] = { tmp, tmp + 1, tmp + 3 };
  int stride422 = enc->width * 2;
  const int src_stride[3] = { stride422, stride422, stride422 };

  // only used for iterating during the YUYV scaling.
  int col = 0, row = 0, row_modifier, col_modifier;

  switch (qtr_row) {
    case 0:
      row_modifier = 0;
      break;
    default:
      row_modifier = enc->height / 2;
      break;
  }
  
  switch (qtr_col) {
  case 0:
    col_modifier = 0;
    break;
  default:
    col_modifier = enc->width;
    break;
  }
  
  const unsigned char *input_cursor;
  unsigned char *output_cursor = tmp;

  // take half the rows of the original image.
  for (row = 0; row < enc->height / 2; row++) {

    // move the input cursor to the beginning of the current row.
    input_cursor = buffer + stride422*(row + row_modifier) + col_modifier;

    // take half of each column of the original image.
    for (col = 0; col < enc->width / 2; col++) {
      // YUYV: Y0,U0,Y1,V1 = two pixels (16 bits total, 4 bits per sample)
      uint8_t Y0 = *input_cursor       & 0xF0;
      uint8_t U0 = *input_cursor       & 0x0F;
      uint8_t Y1 = *(input_cursor + 1) & 0xF0;
      uint8_t V0 = *(input_cursor + 1) & 0x0F;
      // first copy the (Y0,U0) byte and (Y1,V0) byte to the beginning and
      // the end of the four-byte stretch, since they don't need to be modified.
      *output_cursor       = *input_cursor;
      *(output_cursor + 3) = *(input_cursor + 1);
      // calculate the missing two bytes.
      *(output_cursor + 1) = Y0 | V0;
      *(output_cursor + 2) = Y1 | U0;
      // now copy the two new pixels to the row below.
      memcpy (output_cursor + stride422, output_cursor, 4);
      // move the input cursor to the next 2-pixel group.
      input_cursor += 2;   
      // position the output cursor so it can place 4 more pixels.
      output_cursor += 4;
    }
    // skip a row if the output cursor is at the end of a row,
    // since rows are filled in two-at-a-time.
    if ((output_cursor - (unsigned char *)tmp) % (enc->width * 2) == 0) {
      output_cursor += stride422;
    }
  }

  sws_scale (enc->swsctx, src_slices, src_stride, 0, enc->image->h,
	     enc->image->planes, enc->image->stride);

  return encode_image (enc, outsize, out, written);
}

int vp8enc_encode (VP8Enc *enc,
                   const size_t size, const unsigned char *buffer,
                   const size_t outsize, unsigned char *out,
                   size_t *written) {

  const uint8_t *src_slices[3] = { buffer, buffer + 1, buffer + 3 };
  int stride422 = enc->width * 2;
  const int src_stride[3] = { stride422, stride422, stride422 };
  
  sws_scale (enc->swsctx, src_slices, src_stride, 0, enc->image->h,
	     enc->image->planes, enc->image->stride);

  return encode_image (enc, outsize, out, written);
}
