#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <theora/theoraenc.h>

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
    if (enc->info && enc->comment) {
      th_info_init (enc->info);
      th_comment_init (enc->comment);
    }
  }

  if (!enc || !enc->info || !enc->comment) {
    printf ("ERROR: couldn't alloc enc in theoraenc_new\n");
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

int theoraenc_init (TheoraEnc *enc) {
  if (!enc) return 0;

  /* hardcodes based on the example gstreamer v4l2 -> theora pipeline settings */
  enc->info->frame_width = 640;
  enc->info->frame_height = 480;
  enc->info->pic_width = 640;
  enc->info->pic_height = 480;
  enc->info->fps_numerator = 30;
  enc->info->fps_denominator = 1;
  enc->info->quality = 48;
  enc->info->target_bitrate = 0;
  enc->info->keyframe_granule_shift = 6;
  
  /* stuff that was already hardcoded in gstreamer's theora encoder */
  enc->info->aspect_numerator = 0;
  enc->info->aspect_denominator = 0;
  enc->info->pixel_fmt = TH_PF_420;
  enc->info->colorspace = TH_CS_UNSPECIFIED;
  
  th_enc_ctx* ctx = th_encode_alloc (enc->info);
  if (ctx) {
    enc->ctx = ctx;
    return 1;
  } else {
    printf ("ERROR: couldn't alloc encoder ctx in theoraenc_init\n");
    return 0;
  }
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

#define ROUND_UP_2(num) (((num)+1)&~1)
#define ROUND_UP_4(num) (((num)+3)&~3)

int get_height (int component_index, int frame_height) {
  if (component_index == 0) return frame_height;
  else return ROUND_UP_2 (frame_height) / 2;
}

int get_width (int component_index, int frame_width) {
  if (component_index == 0) return frame_width;
  else return ROUND_UP_2 (frame_width) / 2;
}

int get_offset (int component_index, int pic_width, int pic_height) {

  switch (component_index) {
    case 0:
      return 0;
    case 1:
      return ROUND_UP_4 (pic_width) * ROUND_UP_4 (pic_height);
    case 2:
      return ROUND_UP_4 (pic_width) * ROUND_UP_4 (pic_height) +
          ROUND_UP_4 (ROUND_UP_2 (pic_width) / 2) * (ROUND_UP_2 (pic_height) / 2);
    default:
      return -1;
  }
}

int get_row_stride (int component_index, int pic_width) {
  if (component_index == 0) return ROUND_UP_4 (pic_width);
  else return ROUND_UP_4 (ROUND_UP_2 (pic_width) / 2);
}

void init_ycbcr (th_ycbcr_buffer y, th_info* info, uint8_t *data) {

  int i;
  
  for (i = 0; i < 3; i++) {
    y[i].width = get_width (i, info->frame_width);
    y[i].height = get_height (i, info->frame_height);
    y[i].data = data + get_offset (i, info->pic_width, info->pic_height);
    y[i].stride = get_row_stride (i, info->pic_width);
  }
}

int theoraenc_data_in (TheoraEnc *enc, unsigned char *buffer, long buffer_length,
                       theoraenc_each_packet f) {
  
  int r = 1;
  ogg_packet p;
  th_ycbcr_buffer y;

  init_ycbcr (y, enc->info, buffer);
  
  if (!enc) return -1;
  th_encode_ycbcr_in (enc->ctx, y);

  while ((r = th_encode_packetout (enc->ctx, 0, &p))) {    
      f (&p);
  }

  if (r < 0) {
    printf ("Error flushing header packets\n");
    return 0;
  }

  return 1;
}
