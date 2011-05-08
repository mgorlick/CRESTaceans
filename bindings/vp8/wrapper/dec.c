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

int init_video (VP8Dec *dec) {

  int w, h;

  w = (dec->width + 1 & ~1) - (0 & ~1);
  h = (dec->height + 1 & ~1) - (0 & ~1);

  printf ("setting video mode\n");

  dec->screen = SDL_SetVideoMode (w, h, 0, SDL_SWSURFACE);

  if (dec->screen == NULL) goto screen_init_err;

  printf ("setting up overlay\n");

  dec->yuv_overlay = SDL_CreateYUVOverlay (w, h, SDL_YV12_OVERLAY, dec->screen);

  if (dec->yuv_overlay == NULL) goto overlay_init_err;

  dec->rect.x = 0;
  dec->rect.y = 0;
  dec->rect.w = w;
  dec->rect.h = h;

  /*SDL_DisplayYUVOverlay (dec->yuv_overlay, &(dec->rect));*/
  return 1;
  
screen_init_err:
  printf ("Error initializing SDL screen: %s\n", SDL_GetError());
  
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
  uv_offset = (0/2) + (img->stride[1]) * (0/2);
  
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

int vp8dec_init (VP8Dec *dec, size_t size,
                 unsigned char *data) {

  vpx_codec_stream_info_t stream_info;
  vpx_codec_err_t status;
  int flags = 0;

  memset (&stream_info, 0, sizeof (stream_info));
  stream_info.sz = sizeof (stream_info);
  
  status = vpx_codec_peek_stream_info (&vpx_codec_vp8_dx_algo, data, size, &stream_info);
  
  if (!stream_info.is_kf) {
    printf ("Decoder: Not a keyframe. Skipping...\n");
    return 0;
  }

  printf ("Decoder: stream info? %d %d %d %d\n", stream_info.w,
          stream_info.h,
          stream_info.sz,
          stream_info.is_kf);
      

  if (status != VPX_CODEC_OK) {
    printf ("Decoder: Error getting stream info: %s\n", vpx_codec_err_to_string (status));
    return 0;
  }

  dec->width = stream_info.w;
  dec->height = stream_info.h;
  /* XXX fixme address pixel format */

  status = vpx_codec_dec_init (dec->codec, &vpx_codec_vp8_dx_algo, NULL, flags);
  
  if (status != VPX_CODEC_OK) {
    printf ("Failed to initialize vp8 decoder: %s\n", vpx_codec_err_to_string (status));
    return 0;
  }

  init_video (dec);
  dec->is_init = 1;
  return 1;
}

int vp8dec_decode (VP8Dec *dec, size_t size,
                   unsigned char *data) {
  vpx_image_t *img;
  vpx_codec_err_t status;
  vpx_codec_iter_t iter = NULL;
  
  if (0 == dec->is_init) vp8dec_init (dec, size, data);

  if (0 == dec->is_init)
    goto not_initialized;

  status = vpx_codec_decode (dec->codec, data, size, NULL, 0);

  if (status != VPX_CODEC_OK) {
    printf ("Failed to decode frame: %s\n", vpx_codec_err_to_string (status));
    return 0;
  }

  while (NULL != (img = vpx_codec_get_frame (dec->codec, &iter))) {
    /* XXX fixme display it */
    printf ("Some image info: \t\nPixel format = %d\n\twxh: %dx%d\n",
            img->fmt, img->w, img->h);
    display_video (dec, img);
  }

  return 1;

not_initialized:
  /* printf ("Skipping frame. Decoder not initialized yet.\n"); */
  return 0;
}

