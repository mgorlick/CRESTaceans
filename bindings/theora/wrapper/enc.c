#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <theora/theoraenc.h>

#include "enc_settings.h"

/* these #defines from gstreamer */
#define ROUND_UP_2(num) (((num)+1)&~1)
#define ROUND_UP_4(num) (((num)+3)&~3)
#define ROUND_UP_8(num) (((num)+7)&~7)
#define GEN_MASK(x) ((1<<(x))-1)
#define ROUND_UP_X(v,x) (((v) + GEN_MASK(x)) & ~GEN_MASK(x))
#define DIV_ROUND_UP_X(v,x) (((v) + GEN_MASK(x)) >> (x))

static void yuv422_to_yuv420p (unsigned char *dest, unsigned char *src,
                               int width, int height);

typedef struct TheoraEnc {
  th_info *info;
  th_comment *comment;
  th_enc_ctx *ctx;

  /* a buffer we keep around to convert frames
   * from one pixel format to another.
   * just used to avoid stack allocating things
   * over and over. */
  unsigned char *postconv_buffer;
} TheoraEnc;

typedef int (*theoraenc_each_packet) (ogg_packet *p);

TheoraEnc* theoraenc_new (void) {
  
  TheoraEnc* enc = malloc (sizeof (TheoraEnc));

  if (enc == NULL) {
    printf ("ERROR: couldn't allocate encoder in theoraenc_new\n");
  }
  
  enc->info = malloc (sizeof (th_info));
  enc->comment = malloc (sizeof (th_comment));
  
  if (enc->info && enc->comment) {
    th_info_init (enc->info);
    th_comment_init (enc->comment);
  } else {
    printf ("ERROR: couldn't alloc info/comment in theoraenc_new\n");
    free (enc);
    return NULL;    
  }
  
  /* hardcodes based on the example gstreamer v4l2 -> theora pipeline settings */
  enc->info->frame_width = enc_frame_width;
  enc->info->frame_height = enc_frame_height;
  enc->info->pic_width = enc_pic_width;
  enc->info->pic_height = enc_pic_height;
  enc->info->fps_numerator = enc_fps_numerator;
  enc->info->fps_denominator = enc_fps_denominator;
  enc->info->quality = enc_quality;
  enc->info->target_bitrate = enc_target_bitrate;
  enc->info->keyframe_granule_shift = enc_keyframe_granule_shift;
  
  /* stuff that was already hardcoded in gstreamer's theora encoder */
  enc->info->aspect_numerator = 0;
  enc->info->aspect_denominator = 0;
  enc->info->pixel_fmt = TH_PF_420;
  enc->info->colorspace = TH_CS_UNSPECIFIED;
  
  enc->ctx = th_encode_alloc (enc->info);
  if (enc->ctx == NULL) {
    printf ("Couldn't make a theora context in theoraenc_new.\n");
    printf ("The most likely cause is specifying illegal theora encoder settings.\n");
    printf ("Check the libtheora th_info documentation.\n");
    free (enc);
    return NULL;
  }

  enc->postconv_buffer = calloc (PCONV_BUFFER_TOTAL_SIZE, (sizeof (unsigned char)));
  
  if (enc->postconv_buffer == NULL) {
    printf ("ERROR: couldn't alloc reserve buffer in theoraenc_new\n");
    free (enc);
    return NULL;
  }
  
  return enc;
}

void theoraenc_delete (TheoraEnc *enc) {

  if (!enc) return;
  
  if (enc->info) {
    th_info_clear (enc->info);
    free (enc->info);
  }
  if (enc->comment) {
    th_comment_clear (enc->comment);
    free (enc->comment);
  }
  if (enc->ctx) th_encode_free (enc->ctx);
  if (enc->postconv_buffer) free (enc->postconv_buffer);
  free (enc);
}

th_info* theoraenc_info (TheoraEnc *enc) {
  return (!enc) ? NULL : enc->info;
}

int theoraenc_foreach_header (TheoraEnc *enc, theoraenc_each_packet f) {
  ogg_packet p;

  if (!enc) return 0;

  while (th_encode_flushheader (enc->ctx, enc->comment, &p) > 0)
    f (&p);
  
  return 1;
}

/* These calculations assume the pixel format used for
 * encoding is TH_PF_420, which is more commonly known
 * as I420. V4L2 produces packed formats with the
 * cameras we're using but Theora requires planar formats.
 * Thus, conversion *must* be done before calling init_ycbcr
 * to produce a YCbCr buffer for encoding. */

inline int get_height (int component_index, int frame_height) {
  return (component_index == 0) ?
      frame_height :
      ROUND_UP_2 (frame_height) / 2;
}

inline int get_width (int component_index, int frame_width) {
  return (component_index == 0) ?
      frame_width :
      ROUND_UP_2 (frame_width) / 2;
}

inline int get_offset (int component_index, int pic_width, int pic_height) {
  switch (component_index) {
    case 0:
      return 0;
    case 1:
      return ROUND_UP_4 (pic_width) * ROUND_UP_2 (pic_height);
    case 2:
      return ROUND_UP_4 (pic_width) * ROUND_UP_2 (pic_height) +
        ROUND_UP_4 (ROUND_UP_2 (pic_width) / 2) *
          (ROUND_UP_2 (pic_height) / 2);
    default: /* error */
      return 0;
  }
}

inline int get_row_stride (int component_index, int pic_width) {
  return (component_index == 0) ?
      ROUND_UP_4 (pic_width) :
      ROUND_UP_4 (ROUND_UP_2 (pic_width) / 2);
}

void init_ycbcr (th_ycbcr_buffer y, th_info *info, unsigned char *data) {
  int i;

  for (i = 0; i < 3; i++) {
    y[i].height = get_height (i, info->frame_height);
    y[i].width = get_width (i, info->frame_width);
    y[i].stride = get_row_stride (i, info->pic_width);
    y[i].data = data + get_offset (i, info->pic_width, info->pic_height);
  }
}

int theoraenc_data_in (TheoraEnc *enc, unsigned char *buffer,
                       long buffer_length,
                       theoraenc_each_packet f) {
  
  ogg_packet p;
  th_ycbcr_buffer y;
  
  if (!enc) return 0;

  yuv422_to_yuv420p (enc->postconv_buffer, buffer, enc_frame_width, enc_frame_height);
  init_ycbcr (y, enc->info, enc->postconv_buffer);
  
  if (0 == th_encode_ycbcr_in (enc->ctx, y))
    while (th_encode_packetout (enc->ctx, 0, &p)) f (&p);
  else
    return 0;
  
  return 1;
}

/* this function pieced together from gstreamer ffmpegcolorspace component */
static void yuv422_to_yuv420p (unsigned char *dest,
                               unsigned char *src,
                               int width, int height) {
  const unsigned char *p, *p1;
  unsigned char *lum, *cr, *cb, *lum1, *cr1, *cb1;
  int w;
  
  int dest_stride[3] = { get_row_stride (0, width),
                         get_row_stride (1, width),
                         get_row_stride (2, width) };

  /* this stuff specific to I420 and others in family */
  int x_chroma_shift = 1;
  int y_chroma_shift = 1;
  int offset_cb = ROUND_UP_4 (width) * ROUND_UP_X (height, y_chroma_shift);
  int offset_cr = ROUND_UP_4 (DIV_ROUND_UP_X (width, x_chroma_shift))
      * DIV_ROUND_UP_X (height, y_chroma_shift);
  
  /* src_stride calculation specific to YUV422 and others in family */
  int src_stride = ROUND_UP_4 (width * 2);
  
  p1 = src;
  lum1 = dest;
  cb1 = dest + offset_cb;
  cr1 = cb1 + offset_cr;
  
  for (; height >= 1; height -= 2) {
    p = p1;
    lum = lum1;
    cb = cb1;
    cr = cr1;
    for (w = width; w >= 2; w -= 2) {
      lum[0] = p[0];
      cb[0] = p[1];
      lum[1] = p[2];
      cr[0] = p[3];
      p += 4;
      lum += 2;
      cb++;
      cr++;
    }
    if (w) {
      lum[0] = p[0];
      cb[0] = p[1];
      cr[0] = p[3];
    }
    p1 += src_stride;
    lum1 += dest_stride[0];
    if (height > 1) {
      p = p1;
      lum = lum1;
      for (w = width; w >= 2; w -= 2) {
        lum[0] = p[0];
        lum[1] = p[2];
        p += 4;
        lum += 2;
      }
      if (w) {
        lum[0] = p[0];
      }
      p1 += src_stride;
      lum1 += dest_stride[0];
    }
    cb1 += dest_stride[1];
    cr1 += dest_stride[2];
  }
}
