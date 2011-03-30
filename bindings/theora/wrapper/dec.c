#include <stdlib.h>
#include <theora/theoradec.h>

typedef struct TheoraDec {
  int have_a_header;
  th_info* info;
  th_comment* comment;
  th_setup_info* setup;
  th_dec_ctx* ctx;
} TheoraDec;

TheoraDec* theoradec_new (void) {
  
  TheoraDec *dec = malloc (sizeof (TheoraDec));
  
  if (dec) {
    dec->have_a_header = 0;
    dec->info = malloc (sizeof (th_info));
    dec->comment = malloc (sizeof (th_comment));
    dec->setup = NULL;
    dec->ctx = NULL;

    if (dec->info && dec->comment) {
      th_info_init (dec->info);
      th_comment_init (dec->comment);
    } else {
      return NULL;
    }
  }
  
  return dec;
}

void theoradec_delete (TheoraDec *dec) {

  if (!dec) return;
  
  if (dec->info != NULL) {
    th_info_clear (dec->info);
    free (dec->info);
  }
  
  if (dec->comment != NULL) {
    th_comment_clear (dec->comment);
    free (dec->comment);
  }
  
  if (dec->setup != NULL) th_setup_free (dec->setup);
  if (dec->ctx != NULL)  th_decode_free (dec->ctx);
  free (dec);
}

int theoradec_header_in (TheoraDec *dec, unsigned char *buffer, long buffer_length) {

  ogg_packet p;
  int r;
  th_dec_ctx *ctx;

  if (!dec) return -1;
  
  p.packet = buffer;
  p.bytes = buffer_length;
  p.b_o_s = (!(dec->have_a_header)) ? 1 : 0;
  p.e_o_s = 0;
  p.granulepos = 0;
  p.packetno = 0;

  r = th_decode_headerin (dec->info, dec->comment, &dec->setup, &p);
  dec->have_a_header = 1;
  if (r == 0) {
    ctx = th_decode_alloc (dec->info, dec->setup);
    if (ctx) {
      dec->ctx = ctx;
    } else {
      return -1;
    }
  }
  return r;
}

void theoradec_ycbcr_to_buffer (TheoraDec *dec, th_ycbcr_buffer y,
                                unsigned char **out, long *out_written) {

  int plane;

  switch (dec->info->pixel_fmt) {
    case TH_PF_420:
      break;
    case TH_PF_422:
      break;
    case TH_PF_444:
      break;
    default:
      break;
  }

  for (plane = 0; plane < 3; plane++) {
    
  }
}

int theoradec_data_in (TheoraDec *dec, unsigned char *buffer, long buffer_length,
                       unsigned char **out, long *out_written) {
  ogg_packet p;
  int r;
  th_ycbcr_buffer y;

  if (!dec) return -1;
  
  p.packet = buffer;
  p.bytes = buffer_length;
  p.b_o_s = 0;
  p.e_o_s = 0;
  p.granulepos = 0;
  p.packetno = 0;

  r = th_decode_packetin (dec->ctx, &p, NULL);

  if (r == 0) {
    r = th_decode_ycbcr_out (dec->ctx, y);
    theoradec_ycbcr_to_buffer (dec, y, out, out_written);
  }

  return r;
}
