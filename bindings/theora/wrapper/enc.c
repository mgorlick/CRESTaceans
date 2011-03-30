#include <stdlib.h>
#include <stdio.h>
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
    } else {
      printf ("ERROR: couldn't alloc enc in theoraenc_new\n");
      return NULL;
    }
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
  if (!enc) return -1;
  
  th_enc_ctx* ctx = th_encode_alloc (enc->info);
  if (ctx) {
    enc->ctx = ctx;
    return 0;
  } else {
    printf ("ERROR: couldn't alloc encoder ctx in theoraenc_init\n");
    return -1;
  }
}

int theoraenc_foreach_header (TheoraEnc *enc, theoraenc_each_packet f) {
  int r = 1;
  ogg_packet p;

  if (!enc) return -1;
  while (r > 0) {
    r = th_encode_flushheader (enc->ctx, enc->comment, &p);
    if (r > 0) {
      f (&p); 
    } else if (r < 0) {
      printf ("ERROR: couldn't flush a header packet in theoraenc_foreach_header\n");
      return r;
    }
  }
  
  return 0;
}

int theoraenc_data_in (TheoraEnc *enc, th_ycbcr_buffer y, theoraenc_each_packet f) {
  
  int r = 1;
  ogg_packet p;
  
  if (!enc) return -1;
  th_encode_ycbcr_in (enc->ctx, y);

  while (r > 0) {
    r = th_encode_packetout (enc->ctx, 0, &p);
    
    if (r > 0) {
      f (&p);
    } else if (r < 0) {
      printf ("ERROR: couldn't flush a data packet in theoraenc_data_in\n");
      if (r == TH_EINVAL) {
        printf ("buffer size != initial frame size, or encoding already completed");
      }
      return r;
    }
  }

  return 0;
}
