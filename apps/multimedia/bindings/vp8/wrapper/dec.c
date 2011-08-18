#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <libswscale/swscale.h>
#include <vpx/vpx_decoder.h>
#include <vpx/vp8dx.h>

typedef struct VP8Dec {
  vpx_codec_ctx_t *codec;
  int is_init;
  int width;
  int height;
  // libswscale objects for pixel format conversion
  struct SwsContext *swsctx;
} VP8Dec;

int init_video (VP8Dec *dec, const vpx_image_t *img);
void display_video (VP8Dec *dec, const vpx_image_t *img);

void vp8dec_delete (VP8Dec *dec) {
  if (dec == NULL) return;
  if (dec->codec != NULL) free (dec->codec);
  if (dec->swsctx != NULL) free (dec->swsctx);
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

  cfg.threads = 8;
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

int conditional_init (VP8Dec *dec, const size_t input_size, const unsigned char *input) {
  if (0 == dec->is_init) {
    vp8dec_init (dec, input_size, input);
    if (0 == dec->is_init)
      return 0;
  }
  return 1;
}


int decode_and_scale (VP8Dec *dec, const size_t input_size, const unsigned char *input,
		      const size_t output_size, unsigned char *output,
		      float scale_factor) {
  vpx_image_t *img;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;

    // only proceed here if we've seen at least one keyframe
  status = vpx_codec_decode (dec->codec, input, input_size, NULL, 0);
  if (status != VPX_CODEC_OK)
    goto no_decode;

  while (NULL != (img = vpx_codec_get_frame (dec->codec, &iter))) {

   int w = img->d_w;
   int h = img->d_h;
   int BPP = 3;
   int dest_stride = BPP * w;
   assert (output_size == (size_t) BPP * w * h);

    if (dec->swsctx == NULL) {
      dec->swsctx = sws_getContext (w, h, PIX_FMT_YUV420P, // src info
				    scale_factor * w, scale_factor * h, PIX_FMT_RGB24, // dest info
				    1, NULL, NULL, NULL); // what flags to use? who knows!
    }
    if (dec->swsctx == NULL) goto no_video;
    
    sws_scale (dec->swsctx, (const uint8_t * const *) img->planes, img->stride, 0, h,
	       &output, &dest_stride);
  }
  
  return 1;

no_decode:
  printf ("Failed to decode frame: %s\n", vpx_codec_err_to_string (status));
  return 0;
no_video:
  printf ("no video available: could not initialize libswscale\n");
  return 0;  
}

int vp8dec_decode_copy (VP8Dec *dec, const size_t input_size, const unsigned char *input,
			const size_t output_size, unsigned char *output) {

  // this might fail if we tune into a stream in between keyframes.
  // in this case we'll just wait until we get one to move past this if block.
  if (!(conditional_init (dec, input_size, input))) {
      goto not_initialized;
  }
  
  return decode_and_scale (dec, input_size, input, output_size, output, 1.0);

 not_initialized: // not a cause for error unless vp8dec_init prints out an error
  return 0;
}

int vp8dec_decode_pip (VP8Dec *dec_1, const size_t input_size_1, const unsigned char *input_1,
		       VP8Dec *dec_2, const size_t input_size_2, const unsigned char *input_2,
		       const size_t output_size, unsigned char *output) {

  unsigned char tmp[output_size];

  // first we do the exact same logic for the simple case in which there is only a major frame.
  // if the major frame decode doesn't work, then we have nothing to show.
  if (!(vp8dec_decode_copy (dec_1, input_size_1, input_1, output_size, tmp))) {
    // an error was already logged
    return 0;
  }
  
  // if the primary is initialized but the secondary is not, just do a regular decode + copy
  // because we're probably still waiting for the first keyframe of the minor stream.
  if (conditional_init (dec_2, input_size_2, input_2)) {
    // if we're here then both streams have valid decoders.
    // if the minor decode fails we can still memcpy + return since
    // we know the major decode succeeded.
    decode_and_scale (dec_2, input_size_2, input_2, output_size, tmp, 1.0);
  }

  memcpy (output, tmp, output_size);
  return 1;
}
