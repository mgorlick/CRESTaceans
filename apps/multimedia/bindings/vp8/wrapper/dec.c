#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <libswscale/swscale.h>
#include <vpx/vpx_decoder.h>
#include <vpx/vp8dx.h>

#include "misc.h"

int DISPLAY_FORMAT_BPP = 4;
float PIP_AXIS_PORTION = 0.5;
float PIP_FULL_SIZE = 1.0;

typedef struct VP8Dec {
  vpx_codec_ctx_t *codec;
  int is_init;
  int width;
  int height;
  // libswscale objects for pixel format conversion
  struct SwsContext *swsctx;
} VP8Dec;

void vp8dec_delete (VP8Dec *dec) {
  if (dec == NULL) return;
  if (dec->codec != NULL) free (dec->codec);
  if (dec->swsctx != NULL) sws_freeContext (dec->swsctx);
  free (dec);
}

VP8Dec* vp8dec_new (void) {  
  VP8Dec *dec = malloc (sizeof (VP8Dec));
  if (dec == NULL) {
    printf ("Could not allocate VP8 decoder container object\n");
    return NULL;
  }
  dec->codec = malloc (sizeof (vpx_codec_ctx_t));
  if (dec->codec == NULL) {
    printf ("Could not allocate VP8 decoder codec\n");
    return NULL;
  }
  dec->is_init = 0;
  dec->width = 0;
  dec->height = 0;
  dec->swsctx = NULL;
  return dec;
}

int vp8dec_init (VP8Dec *dec, const size_t size,
                 const unsigned char *data) {

  vpx_codec_stream_info_t si;
  vpx_codec_err_t status;
  vpx_codec_dec_cfg_t cfg;
  vpx_codec_flags_t flags = 0;

  memset (&si, 0, sizeof (si));
  si.sz = sizeof (si);
  status = vpx_codec_peek_stream_info (&vpx_codec_vp8_dx_algo, data, size, &si);
  
  if (!si.is_kf) goto not_kf;
  if (status != VPX_CODEC_OK) goto no_stream_info;

  cfg.threads = 4;
  cfg.h = si.h;
  cfg.w = si.w;

  status = vpx_codec_dec_init (dec->codec, &vpx_codec_vp8_dx_algo, &cfg, flags);
  if (status != VPX_CODEC_OK) goto no_init;
  
  dec->height = si.h;
  dec->width = si.w;
  dec->is_init = 1;
  return 1;

not_kf:
  /* arises when we've tuned into the middle of a stream. no problem,
     just wait for a keyframe to arrive. */
  return 0;
no_stream_info:
  /* stream info is junk...eventually should give the user a chance
     to decide whether to continue, but for now just loop */
  printf ("Decoder: Error getting stream info: %s\n", vpx_codec_err_to_string (status));
  return 0;
no_init:
  /* probably a critical error with VPX itself. don't expect this to happen. */
  printf ("Failed to initialize vp8 decoder: %s\n", vpx_codec_err_to_string (status));
  return 0;

}

inline int is_init (VP8Dec *dec) {
  return 1 == dec->is_init;
}

inline int conditional_init (VP8Dec *dec, 
		      const size_t input_size, const unsigned char *input) {
  if (!(is_init (dec))) {
    vp8dec_init (dec, input_size, input);
    if (!is_init (dec)) { return 0; }
  }
  return 1;
}

/* private to implementation */
int decode_and_scale (VP8Dec *dec, 
		      const size_t input_size, const unsigned char *input,
		      const size_t output_size, unsigned char *output,
		      float scale_factor) {
  vpx_image_t *img;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  uint w = 0;
  uint h = 0;

    // only proceed here if we've seen at least one keyframe
  status = vpx_codec_decode (dec->codec, input, input_size, NULL, 0);
  if (status != VPX_CODEC_OK)
    goto no_decode;

  /* only take first frame. vpx as a generality allows for multiple per pkt but vp8 seems to not */
  if (NULL != (img = vpx_codec_get_frame (dec->codec, &iter))) {

   w = img->d_w;
   h = img->d_h;
   int dest_stride = DISPLAY_FORMAT_BPP * w;

    if (dec->swsctx == NULL) {
      dec->swsctx = sws_getContext (w, h, PIX_FMT_YUV420P, // src info
				    scale_factor * w, scale_factor * h, PIX_FMT_RGB32, // dest info
				    1, NULL, NULL, NULL); // what flags to use? who knows!
    }
    if (dec->swsctx == NULL) goto no_video;
    
    if (output_size == w*h*DISPLAY_FORMAT_BPP) {
      sws_scale (dec->swsctx, (const uint8_t * const *) img->planes, img->stride, 0, h,
		 &output, &dest_stride);
    } else {
      goto bad_size;
    }
  }
  
  return 1;

 bad_size:
  printf ("Unexpected buffer size: %d (actual) != %d (expected)\n", output_size, w*h*DISPLAY_FORMAT_BPP);
  return 0;
no_decode:
  printf ("Failed to decode frame: %s\n", vpx_codec_err_to_string (status));
  return 0;
no_video:
  printf ("no video available: could not initialize libswscale\n");
  return 0;
}

/* public. */
int vp8dec_decode_copy (VP8Dec *dec, 
			const size_t input_size, const unsigned char *input,
			const size_t output_size, unsigned char *output) {
  // this might fail if we tune into a stream in between keyframes.
  // in this case we'll just wait until we get one to move past this if block.
  if ((conditional_init (dec, input_size, input))) {
    return decode_and_scale (dec, input_size, input, output_size, output,
			     PIP_FULL_SIZE);
  }
  return 0;
}

/* public. */
int vp8dec_decode_update_major (VP8Dec *dec_major, VP8Dec *dec_minor,
				const size_t input_size, const unsigned char *input,
				const size_t output_size, unsigned char *output) {
  int stride = dec_major->width * DISPLAY_FORMAT_BPP;
  int bytes_per_row_scaled = stride * PIP_AXIS_PORTION;
  int rows_scaled = dec_major->height * PIP_AXIS_PORTION;
  int row = 0;
  unsigned char tmp[output_size];
  unsigned char *src_row = output;
  unsigned char *dst_row = tmp;

  if (vp8dec_decode_copy (dec_major, input_size, input, output_size, tmp)) {
    // copy the current minor picture (in *output) over the new decoded major
    for (row = 0; row < rows_scaled; row++) {
      memcpy (dst_row, src_row, bytes_per_row_scaled);
      dst_row += stride;
      src_row += stride;
    }
    memcpy (output, tmp, output_size);
    return 1;
  } else {
    return 0;
  }
}

/* public. */
int vp8dec_decode_update_minor (VP8Dec *dec_major, VP8Dec *dec_minor,
				const size_t input_size, const unsigned char *input,
				const size_t output_size, unsigned char *output) {
  if (conditional_init (dec_minor, input_size, input)) {
    return decode_and_scale (dec_minor, input_size, input,
			     output_size, output, PIP_AXIS_PORTION);
  }
  return 0;
}
