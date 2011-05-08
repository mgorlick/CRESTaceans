#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <vpx/vpx_decoder.h>
#include <vpx/vp8dx.h>

typedef struct VP8Dec {
  vpx_codec_ctx_t *codec;
  int is_init;
  int width;
  int height;
} VP8Dec;

/* XXX fixme vp8dec_delete */

VP8Dec* vp8dec_new (void) {  
  VP8Dec *dec = malloc (sizeof (VP8Dec));
  dec->is_init = 0;
  dec->width = 0;
  dec->height = 0;
  dec->codec = malloc (sizeof (vpx_codec_ctx_t));
  return dec;
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
  }

  return 1;

not_initialized:
  /* printf ("Skipping frame. Decoder not initialized yet.\n"); */
  return 0;
}
