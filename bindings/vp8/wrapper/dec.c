#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <SDL/SDL.h>
#include <vpx/vpx_decoder.h>
#include <vpx/vp8dx.h>

typedef struct VP8Dec {
  vpx_codec_ctx_t *codec;
  int is_init;
  int width;
  int height;
  SDL_Surface *screen;
  SDL_Overlay *yuv_overlay;
  SDL_Rect rect;
} VP8Dec;

int init_video (VP8Dec *dec);
void display_video (VP8Dec *dec, vpx_image_t *img);

/* XXX fixme vp8dec_delete */

VP8Dec* vp8dec_new (void) {  
  VP8Dec *dec = malloc (sizeof (VP8Dec));
  dec->is_init = 0;
  dec->width = 0;
  dec->height = 0;
  dec->codec = malloc (sizeof (vpx_codec_ctx_t));

  dec->screen = NULL;
  dec->yuv_overlay = NULL;
  
  if (0 > SDL_Init (SDL_INIT_VIDEO)) {
    printf ("Unable to init SDL: %s\n", SDL_GetError ());
    return NULL;
  }
  
  return dec;
}

/* useful for debugging */
void peek (size_t size, unsigned char* data) {
  
  vpx_codec_stream_info_t stream_info;
  vpx_codec_err_t status;
  
  memset (&stream_info, 0, sizeof (stream_info));
  stream_info.sz = sizeof (stream_info);
  
  status = vpx_codec_peek_stream_info (&vpx_codec_vp8_dx_algo, data, size, &stream_info);
  if (!stream_info.is_kf) {
    printf ("Not a keyframe. Skipping...\n");
    return;
  }
  printf ("stream info? %d %d %d %d\n", stream_info.w, stream_info.h,
          stream_info.sz, stream_info.is_kf);
  if (status != VPX_CODEC_OK) {
    printf ("Error getting stream info: %s\n",  vpx_codec_err_to_string (status));
  }
}

int vp8dec_init (VP8Dec *dec, size_t size,
                 unsigned char *data) {

  vpx_codec_stream_info_t si;
  vpx_codec_err_t status;
  int flags = 0;

  memset (&si, 0, sizeof (si));
  si.sz = sizeof (si);
  status = vpx_codec_peek_stream_info (&vpx_codec_vp8_dx_algo, data, size, &si);
  
  if (!si.is_kf) goto not_kf;
  if (status != VPX_CODEC_OK) goto no_stream_info;

  printf ("Decoder: stream info? %d %d %d %d\n", si.w, si.h, si.sz, si.is_kf);
  dec->width = si.w;
  dec->height = si.h;

  status = vpx_codec_dec_init (dec->codec, &vpx_codec_vp8_dx_algo, NULL, flags);
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

int vp8dec_decode (VP8Dec *dec, size_t size,
                   unsigned char *data) {
  vpx_image_t *img;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  
  if (0 == dec->is_init) {
    vp8dec_init (dec, size, data);
    init_video (dec);
  }

  if (0 == dec->is_init || dec->screen == NULL) goto not_initialized;

  status = vpx_codec_decode (dec->codec, data, size, NULL, 0);

  if (status != VPX_CODEC_OK) goto no_decode;

  while (NULL != (img = vpx_codec_get_frame (dec->codec, &iter)))
    display_video (dec, img);

  return 1;

not_initialized:
  printf ("Skipping frame. Decoder not initialized yet.\n");
  return 0;

no_decode:
  printf ("Failed to decode frame: %s\n", vpx_codec_err_to_string (status));
  return 0;

}
int init_video (VP8Dec *dec) {

  int w, h;
  w = (dec->width + 1 & ~1) - (0 & ~1);
  h = (dec->height + 1 & ~1) - (0 & ~1);

  dec->screen = SDL_SetVideoMode (w, h, 0, SDL_SWSURFACE);
  if (dec->screen == NULL) goto screen_init_err;
  dec->yuv_overlay = SDL_CreateYUVOverlay (w, h, SDL_YV12_OVERLAY, dec->screen);
  if (dec->yuv_overlay == NULL) goto overlay_init_err;

  dec->rect.x = 0;
  dec->rect.y = 0;
  dec->rect.w = w;
  dec->rect.h = h;
  return 1;
  
screen_init_err:
  printf ("Error initializing SDL screen: %s\n", SDL_GetError());
  return 0;
  
overlay_init_err:
  printf ("Error initializing SDL overlay: %s\n", SDL_GetError());
  return 0;
}

void display_video (VP8Dec *dec, vpx_image_t *img) {
  int i;
  int y_offset, uv_offset;
  
  /* Lock SDL_yuv_overlay */
  if (SDL_MUSTLOCK(dec->screen)) {
    if (0 > SDL_LockSurface(dec->screen)) return;
  }
  if (0 > SDL_LockYUVOverlay(dec->yuv_overlay)) return;
  
  /* let's draw the data on a SDL screen (*screen) */
  /* deal with border stride */
  /* reverse u and v for SDL */
  /* and crop input properly, respecting the encoded frame rect */
  /* problems may exist for odd frame rect for some encodings */

  y_offset = (0 & ~1) + img->stride[0] * (0 & ~1);
  uv_offset = img->stride[1];
  
  for (i = 0; i < dec->yuv_overlay->h; i++)
    memcpy (dec->yuv_overlay->pixels[0] + dec->yuv_overlay->pitches[0] * i,
            img->planes[0] + y_offset + img->stride[0] * i,
            dec->yuv_overlay->w);
  for (i = 0; i < dec->yuv_overlay->h/2; i++) {
    memcpy (dec->yuv_overlay->pixels[1] + dec->yuv_overlay->pitches[1] * i,
            img->planes[2] + uv_offset + img->stride[2] * i,
            dec->yuv_overlay->w/2);
    memcpy (dec->yuv_overlay->pixels[2] + dec->yuv_overlay->pitches[2] * i,
            img->planes[1] + uv_offset + img->stride[1] * i,
            dec->yuv_overlay->w/2);
  }
  
  if (SDL_MUSTLOCK(dec->screen)) SDL_UnlockSurface(dec->screen);
  SDL_UnlockYUVOverlay(dec->yuv_overlay);
  SDL_DisplayYUVOverlay(dec->yuv_overlay, &(dec->rect));
}

