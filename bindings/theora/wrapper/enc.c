#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <theora/theoraenc.h>

#include "enc_settings.h"

#define ROUND_UP_2(num) (((num)+1)&~1)
#define ROUND_UP_4(num) (((num)+3)&~3)
#define ROUND_UP_8(num) (((num)+7)&~7)

typedef struct TheoraEnc {
  th_info* info;
  th_comment* comment;
  th_enc_ctx* ctx;
} TheoraEnc;

typedef int (*theoraenc_each_packet) (ogg_packet *p);

TheoraEnc* theoraenc_new (void) {
  
  TheoraEnc* enc = malloc (sizeof (TheoraEnc));

  if (enc) {
    enc->info = malloc (sizeof (th_info));
    enc->comment = malloc (sizeof (th_comment));
    enc->ctx = NULL;
  } else {
    printf ("ERROR: couldn't alloc enc in theoraenc_new\n");
    return NULL;
  }
  
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
  enc->info->pixel_fmt = TH_PF_422;
  enc->info->colorspace = TH_CS_UNSPECIFIED;
  
  th_enc_ctx* ctx = th_encode_alloc (enc->info);
  if (ctx) {
    enc->ctx = ctx;
  } else {
    printf ("ERROR: couldn't alloc ctx in theoraenc_new\n");
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
  free (enc);
}

th_info* theoraenc_info (TheoraEnc *enc) {
  if (!enc) return NULL;
  return enc->info;
}

int theoraenc_foreach_header (TheoraEnc *enc, theoraenc_each_packet f) {
  int r = 1;
  ogg_packet p;

  if (!enc) return 0;
  while (r > 0) {
    r = th_encode_flushheader (enc->ctx, enc->comment, &p);
    if (r > 0) {
      f (&p); 
    } else if (r < 0) {
      printf ("ERROR: couldn't flush a header packet in theoraenc_foreach_header\n");
      return 0;
    }
  }
  
  return 1;
}

/* WARNING: ALL THIS ASSUMES PIXEL FORMAT:
   V4L2_PIX_FMT_YUYV
   which *most likely* corresponds to
   TH_PF_422
   CHANGE CALCULATIONS IF THIS CHANGES! */

inline int get_height (int component_index, int frame_height) {
  /* if (component_index == 0) return frame_height;
     else return ROUND_UP_2 (frame_height) / 2; */
  return frame_height;
}

inline int get_width (int component_index, int frame_width) {
  if (component_index == 0) return frame_width;
  else return ROUND_UP_2 (frame_width) / 2;
}

inline int get_offset (int component_index, int pic_width, int pic_height) {

  switch (component_index) {
    case 0:
      return 0;
    case 1:
      return ROUND_UP_4 (pic_width) * pic_height;
    case 2:
      return pic_height * (ROUND_UP_4 (pic_width) + (ROUND_UP_8 (pic_width) / 2));
    default:
      printf ("get_offset called with invalid YCBCR component index\n");
      return 0;
  }
}

inline int get_row_stride (int component_index, int pic_width) {
  if (component_index == 0) return ROUND_UP_4 (pic_width);
  else return (ROUND_UP_8 (pic_width) / 2);
}

void init_ycbcr (th_ycbcr_buffer y, th_info* info, unsigned char *data) {

  int i;
  
  for (i = 0; i < 3; i++) {
    y[i].height = get_height (i, info->frame_height);
    y[i].width = get_width (i, info->frame_width);
    y[i].stride = get_row_stride (i, info->pic_width);
    y[i].data = data + get_offset (i, info->pic_width, info->pic_height);
    /* printf ("y[%d].height = %d\n", i, y[i].height);
    printf ("y[%d].width = %d\n", i, y[i].width);
    printf ("y[%d].stride = %d\n", i, y[i].stride);
    printf ("y[%d].data = %p (offset %d)\n", i, y[i].data, y[i].data - data); */
  }
}

int theoraenc_data_in (TheoraEnc *enc, unsigned char *buffer, long buffer_length,
                       theoraenc_each_packet f) {
  
  ogg_packet p;
  th_ycbcr_buffer y;
  
  if (!enc) return -1;

  init_ycbcr (y, enc->info, buffer);
  
  th_encode_ycbcr_in (enc->ctx, y);

  while (th_encode_packetout (enc->ctx, 0, &p)) {
    /*printf ("enc: a packet of size %ld\n", p.bytes);*/
    f (&p);
  }

  return 1;
}
