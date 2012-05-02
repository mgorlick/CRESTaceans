#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <libswscale/swscale.h>
#include <vpx/vpx_decoder.h>
#include <vpx/vp8dx.h>

#include "misc.h"

float PROCESS_FORMAT_BPP = 1.5;
float DISPLAY_FORMAT_BPP = 4.0;

void vp8dec_get_sizes (float *process, float *display) {
  *process = PROCESS_FORMAT_BPP;
  *display = DISPLAY_FORMAT_BPP;
}

typedef struct ColorConverter {
  struct SwsContext *sws_ctx;
  unsigned int width;
  unsigned int height;
} ColorConverter;

void color_converter_sz (ColorConverter *c, unsigned int *w, unsigned int *h) {
  *w = c->width;
  *h = c->height;
}

void color_converter_delete (ColorConverter *c) {
  if (c == NULL) return;
  if (c->sws_ctx != NULL) free (c->sws_ctx);
}

ColorConverter* color_converter_new (unsigned int width, unsigned int height) {
  if (width == 0 || height == 0) return NULL;
  ColorConverter *c = malloc (sizeof(*c));
  if (c == NULL) 
    return NULL;
  
  c->sws_ctx = sws_getContext (width, height, PIX_FMT_YUV420P, 
			       width, height, PIX_FMT_RGB32,
			       1, NULL, NULL, NULL);
  if (c->sws_ctx == NULL) {
    free (c);
    return NULL;
  }

  c->width = width;
  c->height = height;
  
  return c;
}

int yuv420p_to_rgb32 (ColorConverter *c,
		      const size_t input_size, const unsigned char *input,
		      const size_t output_size, unsigned char *output) { 

  if (c == NULL) goto bad_size;
  unsigned int width = c->width;
  unsigned int height = c->height;

  if (output_size != width * height * DISPLAY_FORMAT_BPP) 
    goto bad_size;
  if (input_size != width * height * PROCESS_FORMAT_BPP)
    goto bad_size;
  if (width == 0 || height == 0)
    goto bad_size;

  const unsigned char *y = input;
  const unsigned char *u = y + (width * height);
  const unsigned char *v = u + (width * height / 4);
  const unsigned char *planes[3] = {y, u, v};
  const int strides[3] = {width, width / 2, width / 2};
  const int dest_stride[1] = {width * DISPLAY_FORMAT_BPP};
  
  sws_scale (c->sws_ctx, planes, strides,
	     0, height, &output, dest_stride);

  return 1;
  
 bad_size:
  return 0;
}

typedef struct VP8Dec {
  // vpx data structures preserved between calls
  vpx_codec_ctx_t *codec;
  // metadata hand managed by programmer from vpx structs
  int is_init;

} VP8Dec;

void vp8dec_delete (VP8Dec *dec) {
  if (dec == NULL) return;
  if (dec->codec != NULL) free (dec->codec);
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

unsigned int halve_if_uv (unsigned int plane, unsigned int dimension) {
  return plane ? (dimension + 1) / 2 : dimension;
}

/* private to implementation */
int decode_and_scale (VP8Dec *dec, 
		      const size_t input_size, const unsigned char *input,
		      const size_t output_size, unsigned char *output) {
  vpx_image_t *img = NULL;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  unsigned int w = 0;
  unsigned int h = 0;
  unsigned int expected_size = 0;
  unsigned int plane = 0;
  unsigned int row = 0;
  unsigned char *output_cursor = output;
  unsigned char *decoded_input_cursor = NULL;

    // only proceed here if we've seen at least one keyframe
  status = vpx_codec_decode (dec->codec, input, input_size, NULL, 0);
  if (status != VPX_CODEC_OK) 
    goto no_decode;

  /* 
     only take first frame.
     vpx in general allows for multiple frames per pkt
     but vp8 doesn't use this feature
  */
  if (NULL == (img = vpx_codec_get_frame (dec->codec, &iter)))
    goto no_decode;
  
   w = img->d_w;
   h = img->d_h;
   expected_size = w * h * PROCESS_FORMAT_BPP;   
   if (output_size != expected_size)
     goto bad_size;

   for (plane = 0; plane < 3; plane++) {
     decoded_input_cursor = img->planes[plane];
     for (row = 0;
	  row < halve_if_uv (plane, img->d_h); 
	  row++, 
	    decoded_input_cursor += img->stride[plane],
	    output_cursor += halve_if_uv (plane, img->d_w))
       {
	 memcpy (output_cursor, decoded_input_cursor, halve_if_uv (plane, img->d_w));
       }
   }
   
   return 1;
   
 bad_size:
   printf ("Unexpected buffer size in decode: %d (actual) != %d (expected)\n", 
	   output_size, expected_size);
   return 0;
 no_decode:
  printf ("Failed to decode frame: %s\n", vpx_codec_err_to_string (status));
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
  } else return 0;
}
