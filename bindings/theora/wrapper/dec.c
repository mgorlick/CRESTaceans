#include <stdlib.h>
#include <theora/theoradec.h>

typedef struct TheoraDec {
  th_info* info;
  th_comment* comment;
  th_setup_info* setup;
  th_dec_ctx* ctx;
} TheoraDec;

TheoraDec* theoradec_new (void) {
  
  TheoraDec* dec = malloc (sizeof (TheoraDec));
  
  if (dec != NULL) {

    dec->info = malloc (sizeof (th_info));
    dec->comment = malloc (sizeof (th_comment));
    dec->setup = NULL;
    dec->ctx = NULL;

    if (dec->info != NULL && dec->comment != NULL) {
      th_info_init (dec->info);
      th_comment_init (dec->comment);
    } else {
      return NULL;
    }
  }
  
  return dec;
}

void theoradec_delete (TheoraDec* dec) {
  
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

int theoradec_headerin (TheoraDec* dec, unsigned char* buffer, int buffer_length) {

  ogg_packet p;
  int r;
  
  p.packet = buffer;
  p.bytes = buffer_length;
  p.b_o_s = 0;
  p.e_o_s = 0;
  p.granulepos = 0;
  p.packetno = 0;

  r = th_decode_headerin (dec->info, dec->comment, &dec->setup, &p);
  if (r == 0) dec->ctx = th_decode_alloc (dec->info, dec->setup);
  return r;
}

int theoradec_datain (TheoraDec* dec, unsigned char* buffer, int buffer_length) {
  ogg_packet p;
  int r;
  
  p.packet = buffer;
  p.bytes = buffer_length;
  p.b_o_s = 0;
  p.e_o_s = 0;
  p.granulepos = 0;
  p.packetno = 0;

  r = th_decode_packetin (dec->ctx, &p, NULL);
  
  return r;
}

int theoradec_dataout (TheoraDec* dec) {
  int r;
  th_ycbcr_buffer y;

  r = th_decode_ycbcr_out (dec->ctx, y);
  return r;
}
