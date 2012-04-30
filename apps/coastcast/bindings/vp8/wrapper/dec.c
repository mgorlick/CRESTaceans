#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <libswscale/swscale.h>
#include <vpx/vpx_decoder.h>
#include <vpx/vp8dx.h>

#include "misc.h"

int PROCESS_FORMAT_BPP = 3;
int DISPLAY_FORMAT_BPP = 4;

void vp8dec_get_sizes (int *process, int *display) {
  *process = PROCESS_FORMAT_BPP;
  *display = DISPLAY_FORMAT_BPP;
}

typedef struct VP8Dec {
  // vpx data structures preserved between calls
  vpx_codec_ctx_t *codec;
  // metadata hand managed by programmer from vpx structs
  int is_init;
  // libswscale objects for pixel format conversion
  struct SwsContext *sws_scaler_ctx;
  struct SwsContext *sws_color_conversion_ctx;
} VP8Dec;

void vp8dec_delete (VP8Dec *dec) {
  if (dec == NULL) return;
  if (dec->codec != NULL) free (dec->codec);
  if (dec->sws_scaler_ctx != NULL) sws_freeContext (dec->sws_scaler_ctx);
  if (dec->sws_color_conversion_ctx != NULL) sws_freeContext (dec->sws_color_conversion_ctx);
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
  dec->sws_scaler_ctx = NULL;
  dec->sws_color_conversion_ctx = NULL;
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
		      const size_t output_size, unsigned char *output) {
  vpx_image_t *img;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  unsigned int w = 0;
  unsigned int h = 0;
  int dest_stride = 0;

    // only proceed here if we've seen at least one keyframe
  status = vpx_codec_decode (dec->codec, input, input_size, NULL, 0);
  if (status != VPX_CODEC_OK)
    goto no_decode;

  /* only take first frame. vpx as a generality allows for multiple per pkt but vp8 seems to not */
  if (NULL != (img = vpx_codec_get_frame (dec->codec, &iter))) {
    

   w = img->d_w;
   h = img->d_h;
   dest_stride = w * DISPLAY_FORMAT_BPP;

   printf ("dimensions: %dx%d\n", w, h);
   printf ("strides: %d, %d, %d, %d\n", img->stride[0], img->stride[1], img->stride[2], img->stride[3]);
     
    if (dec->sws_scaler_ctx == NULL) {
      dec->sws_scaler_ctx = sws_getContext (w, h, PIX_FMT_YUV420P, // src info
					    w, h, PIX_FMT_RGB32, // dest info
				    1, NULL, NULL, NULL); // what flags to use? who knows!
    }
    if (dec->sws_scaler_ctx == NULL) goto no_video;
   
    if (output_size == w*h*DISPLAY_FORMAT_BPP) {
      sws_scale (dec->sws_scaler_ctx, 
		 (const uint8_t * const *) img->planes, 
		 img->stride, 0, h,
		 &output, &dest_stride);
      printf ("wrote decoded image into out\n");
    } else {
      goto bad_size;
    }
  } else {
    goto no_decode;
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
    return decode_and_scale (dec, input_size, input, output_size, output);
  }
  return 0;
}
