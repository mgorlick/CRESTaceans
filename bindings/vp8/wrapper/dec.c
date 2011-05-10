#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <SDL/SDL.h>
#include <vpx/vpx_decoder.h>
#include <vpx/vp8dx.h>

typedef struct VP8Dec {
  vpx_codec_ctx_t *codec;
  int is_init;
  int vid_is_init;
  int width;
  int height;
  SDL_Surface *screen;
  SDL_Overlay *yuv_overlay;
  SDL_Rect rect;
} VP8Dec;

int init_video (VP8Dec *dec, const vpx_image_t *img);
void display_video (VP8Dec *dec, const vpx_image_t *img);

/* XXX fixme vp8dec_delete */

VP8Dec* vp8dec_new (void) {  
  VP8Dec *dec = malloc (sizeof (VP8Dec));
  dec->is_init = 0;
  dec->vid_is_init = 0;
  dec->codec = malloc (sizeof (vpx_codec_ctx_t));
  dec->screen = NULL;
  dec->yuv_overlay = NULL;
  if (0 > SDL_Init (SDL_INIT_VIDEO)) {
    printf ("Unable to init SDL: %s\n", SDL_GetError ());
    return NULL;
  }
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

  cfg.threads = 2;
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

void is_kf (const size_t size, const unsigned char *data) {
  vpx_codec_stream_info_t si;
  vpx_codec_err_t status;

  memset (&si, 0, sizeof (si));
  si.sz = sizeof (si);
  status = vpx_codec_peek_stream_info (&vpx_codec_vp8_dx_algo, data, size, &si);

  printf ("is kf? %d\n", si.is_kf);
}

int vp8dec_decode (VP8Dec *dec, const size_t size,
                   const unsigned char *data) {
  vpx_image_t *img;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  
  if (0 == dec->is_init) {
    vp8dec_init (dec, size, data);
    if (0 == dec->is_init) goto not_initialized;
  }

  is_kf (size, data);

  status = vpx_codec_decode (dec->codec, data, size, NULL, 0);
  if (status != VPX_CODEC_OK) goto no_decode;

  while (NULL != (img = vpx_codec_get_frame (dec->codec, &iter))) {
    if (0 == dec->vid_is_init) {
      init_video (dec, img);
      if (0 == dec->vid_is_init) goto no_video;
    }
    display_video (dec, img);
  }
  return 1;

not_initialized:
  return 0;
no_decode:
  printf ("Failed to decode frame: %s\n", vpx_codec_err_to_string (status));
  return 0;
no_video:
  return 0;

}
int init_video (VP8Dec *dec, const vpx_image_t *img) {

  int w, h, ov_w, ov_h;
  w = img->d_w;
  h = img->d_h;
  ov_w = img->w;
  ov_h = img->h;

  dec->screen = SDL_SetVideoMode (w, h, 0, SDL_SWSURFACE);
  if (dec->screen == NULL) goto screen_init_err;
  dec->yuv_overlay = SDL_CreateYUVOverlay (w, h, SDL_YV12_OVERLAY, dec->screen);
  if (dec->yuv_overlay == NULL) goto overlay_init_err;

  printf ("made SDL overlay: %dx%d\n", dec->yuv_overlay->w, dec->yuv_overlay->h);

  dec->rect.x = 0;
  dec->rect.y = 0;
  dec->rect.w = w;
  dec->rect.h = h;
  dec->vid_is_init = 1;
  return 1;
  
screen_init_err:
  printf ("Error initializing SDL screen: %s\n", SDL_GetError());
  return 0;
overlay_init_err:
  printf ("Error initializing SDL overlay: %s\n", SDL_GetError());
  return 0;
}

void display_video (VP8Dec *dec, const vpx_image_t *img) {
  int i;
  
  /* Lock SDL_yuv_overlay */
  if (SDL_MUSTLOCK(dec->screen)) {
    if (0 > SDL_LockSurface(dec->screen)) return;
  }
  if (0 > SDL_LockYUVOverlay(dec->yuv_overlay)) return;
  
  for (i = 0; i < dec->yuv_overlay->h; i++) {
    memcpy (dec->yuv_overlay->pixels[0] + dec->yuv_overlay->pitches[0] * i,
            img->planes[0] + img->stride[0] * i,
            dec->yuv_overlay->w);
  }
  
  /* reverse U and V since SDL thinks we're drawing a YV12 */
  for (i = 0; i < dec->yuv_overlay->h/2; i++) {
    memcpy (dec->yuv_overlay->pixels[1] + dec->yuv_overlay->pitches[1] * i,
            img->planes[2] + img->stride[2] * i,
            dec->yuv_overlay->w/2);
    memcpy (dec->yuv_overlay->pixels[2] + dec->yuv_overlay->pitches[2] * i,
            img->planes[1] + img->stride[1] * i,
            dec->yuv_overlay->w/2);
  }
  
  if (SDL_MUSTLOCK(dec->screen)) SDL_UnlockSurface(dec->screen);
  SDL_UnlockYUVOverlay(dec->yuv_overlay);
  SDL_DisplayYUVOverlay(dec->yuv_overlay, &(dec->rect));
}
