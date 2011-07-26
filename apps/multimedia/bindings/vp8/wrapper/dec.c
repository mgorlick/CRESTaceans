#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <SDL/SDL.h>
#include <libswscale/swscale.h>
#include <vpx/vpx_decoder.h>
#include <vpx/vp8dx.h>

int init_SDL (void) {
  if (SDL_Init (SDL_INIT_VIDEO) != 0 || SDL_VideoInit (NULL) != 0) {
    printf ("Unable to init SDL: %s\n", SDL_GetError ());
    return 0;
  }
  return 1;
}

typedef struct VP8Dec {
  vpx_codec_ctx_t *codec;
  int is_init;
  int width;
  int height;
  // SDL window and surface for drawing
  SDL_Window *wind;
  SDL_Surface *surf;
  // libswscale objects for pixel format conversion
  struct SwsContext *swsctx;
} VP8Dec;

int init_video (VP8Dec *dec, const vpx_image_t *img);
void display_video (VP8Dec *dec, const vpx_image_t *img);

void vp8dec_delete (VP8Dec *dec) {
  if (dec == NULL) return;
  if (dec->wind != NULL) SDL_DestroyWindow (dec->wind);
  
  if (dec->codec != NULL) free (dec->codec);
  free (dec);
}

VP8Dec* vp8dec_new (void) {  
  VP8Dec *dec = malloc (sizeof (VP8Dec));
  dec->codec = malloc (sizeof (vpx_codec_ctx_t));

  dec->is_init = 0;
  dec->width = 0;
  dec->height = 0;

  dec->wind = NULL;
  dec->surf = NULL;

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

int vp8dec_decode (VP8Dec *dec, const size_t size,
                   const unsigned char *data) {
  vpx_image_t *img;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  
  // this might fail if we tune into a stream in between keyframes.
  // in this case we'll just wait until we get one to move past this if block.
  if (0 == dec->is_init) {
    vp8dec_init (dec, size, data);
    if (0 == dec->is_init)
      goto not_initialized;
  }
  
  // only proceed here if we've seen at least one keyframe
  status = vpx_codec_decode (dec->codec, data, size, NULL, 0);
  if (status != VPX_CODEC_OK)
    goto no_decode;

  while (NULL != (img = vpx_codec_get_frame (dec->codec, &iter))) {
    if (NULL == dec->wind || NULL == dec->surf || NULL == dec->swsctx)
      if (0 == init_video (dec, img))
	goto no_video;

    display_video (dec, img);
  }

  return 1;

 not_initialized: // not a cause for error unless vp8dec_init prints out an error
  return 0;
no_decode:
  printf ("Failed to decode frame: %s\n", vpx_codec_err_to_string (status));
  return 0;
no_video:
  printf ("no video available\n");
  return 0;
}

int init_video (VP8Dec *dec, const vpx_image_t *img) {
  SDL_DisplayMode mode;
  
  if (dec->wind == NULL) {    
    dec->wind = SDL_CreateWindow ("A CREST Video Display",
				  SDL_WINDOWPOS_UNDEFINED,
				  SDL_WINDOWPOS_UNDEFINED,
				  img->d_w, img->d_h,
				  SDL_WINDOW_SHOWN);
    if (dec->wind == NULL)
      goto no_window_err;
  }

  // ensure that the output mode is RGB888. 
  // right now, this is the only mode SDL seems to support (on this machine).
  // therefore, I am hardcoding swscale's conversion to target it.
  SDL_GetWindowDisplayMode (dec->wind, &mode);
  mode.format = SDL_PIXELFORMAT_RGB888;
  SDL_GetWindowDisplayMode (dec->wind, &mode);
  assert (mode.format == SDL_PIXELFORMAT_RGB888);
  
  // set up the surface to draw on.
  if (dec->surf == NULL) {
    dec->surf = SDL_GetWindowSurface (dec->wind);
    if (dec->surf == NULL)
      goto no_surf_err;
  }

  if (dec->swsctx == NULL) {
    dec->swsctx = sws_getContext (dec->surf->w, dec->surf->h, PIX_FMT_YUV420P, // src info
				  dec->surf->w, dec->surf->h, PIX_FMT_RGB32, // dest info
				  1, NULL, NULL, NULL); // what flags to use? who knows!
    if (dec->swsctx == NULL)
      goto no_sws_err;
  }

  return 1;
  
no_window_err:
  printf ("Error initializing SDL window: %s\n", SDL_GetError ());
  return 0;
 no_surf_err:
  printf ("Could not get SDL surface: %s\n", SDL_GetError ());
  return 0;
 no_sws_err:
  printf ("Could not initialize swscale\n");
  return 0;
}

void display_video (VP8Dec *dec, const vpx_image_t *img) {

  uint8_t *img_planes[3] = { img->planes[0], img->planes[1], img->planes[2] };
  int img_strides[3] = { img->stride[0], img->stride[1], img->stride[2] };
  uint8_t *out = dec->surf->pixels;

  SDL_LockSurface (dec->surf);

  sws_scale (dec->swsctx, img_planes, img_strides, 0, dec->surf->h,
	     &out, &dec->surf->pitch);

  SDL_UnlockSurface (dec->surf);
  SDL_UpdateWindowSurface (dec->wind);
}
