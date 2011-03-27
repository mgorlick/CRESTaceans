#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <vorbis/codec.h>

typedef struct {
  int is_init;
  vorbis_info* vi;
  vorbis_comment* vc;
  vorbis_dsp_state* vd;
  vorbis_block* vb;
} vorbisdec;


vorbisdec* vorbisdec_new (void) {
  
  vorbis_info* vi;
  vorbis_comment* vc;
  vorbis_dsp_state* vd;
  vorbis_block* vb;
  vorbisdec* dec;
  
  dec = malloc (sizeof (vorbisdec));
  vc = malloc (sizeof (vorbis_comment));
  vi = malloc (sizeof (vorbis_info));
  vd = malloc (sizeof (vorbis_dsp_state));
  vb = malloc (sizeof (vorbis_block));

  dec->is_init = 0;
  dec->vi = vi;
  dec->vc = vc;
  dec->vd = vd;
  dec->vb = vb;
  vorbis_info_init (dec->vi);
  vorbis_comment_init (dec->vc);

  return dec;
}

int vorbisdec_finish_init (vorbisdec* dec) {

  int res;
  
  res = vorbis_synthesis_init (dec->vd, dec->vi);
  if (res == 0) {
    res = vorbis_block_init (dec->vd, dec->vb);
    if (res == 0) {
      dec->is_init = 1;
      return 0;
    } else return -1;
  } else return -1;
}

void vorbisdec_delete (vorbisdec* dec) {
  if (dec->is_init)  vorbis_block_clear (dec->vb);
  vorbis_dsp_clear (dec->vd);
  vorbis_comment_clear (dec->vc);
  vorbis_info_clear (dec->vi);

  free (dec->vb);
  free (dec->vd);
  free (dec->vc);
  free (dec->vi);
  free (dec);
}

int vorbisdec_is_init (vorbisdec* dec) {
  return dec->is_init;
}

/* print_buffer here for testing purposes */
void print_buffer (unsigned char *buff, long buff_len) {
  
  int i;
  unsigned char* buff_pos = buff;
  printf ("buffer (size %ld): [BEGIN]", buff_len);
  for (i = 0; i < buff_len; i++) {
    printf ("%uc", *buff_pos);
    buff_pos++;
  }
  printf ("[END]\n");
}

/* print_stream_info here for testing purposes */
void print_stream_info (vorbisdec* dec) {
  
  printf ("version: %d\n", dec->vi->version);
  printf ("channels: %d\n", dec->vi->channels);
  printf ("rate: %ld\n", dec->vi->rate);
  
}

/* header_packet_in: process one of the header packets in the stream
   (identified by (buffer[0] && 1 == 1). decoder should call this with
   the first three packets: expect the identification packet, comment
   packet and type packet in order. Once all three have been called,
   the vorbisdec* is ready to use for synthesizing data packets.

   returns a negative num if error processing header packet; or
   0, 1, or 2, to signify the type of header packet processed.
   successive calls to header_packet_in MUST return 0, 1, and 2 in order
  for the subsequent decoding to work. */
int header_packet_in (vorbisdec* dec, unsigned char *buff, long buff_len) {

  ogg_packet pkt;
  int hi;

  if (buff_len < 1) {
    return -1;
  }
  
  pkt.packet = buff;
  pkt.bytes = buff_len;
  pkt.b_o_s = (buff[0] == 0x01) ? 1 : 0;
  pkt.e_o_s = 0;
  pkt.granulepos = -1;
  pkt.packetno = 0;
  
  switch (buff[0]) {
    case 0x01:
      hi = vorbis_synthesis_headerin (dec->vi, dec->vc, &pkt);
      if (hi == 0) return 0;
      break;
    case 0x03:
      hi = vorbis_synthesis_headerin (dec->vi, dec->vc, &pkt);
      if (hi == 0) return 1;
      break;
    case 0x05:
      hi = vorbis_synthesis_headerin (dec->vi, dec->vc, &pkt);
      print_stream_info (dec);
      if (hi == 0 && (hi = vorbisdec_finish_init (dec)) == 0) return 2;
      break;
    default: /* not a valid header packet */
      hi = -1;
  }
  return hi;
}

/* header_packet_blockin: process one of the header packets in the stream
   (identified by (buffer[0] && 1 == 0). decoder should call this with
   any non-empty data packets after header_packet_in has been successfully
   called three times with the three header packets.

   returns -1 if error processing data packet; or 0 or other non-negative
   number to indicate how many samples are available to read. decoder
   must call data_packet_pcmout after data_packet_blockin returns. */
int data_packet_blockin (vorbisdec* dec, unsigned char *buff, long buff_len) {

  ogg_packet pkt;
  int res;
  
  if (buff_len < 0 || !dec->is_init) {
    return -1;
  }
  
  pkt.packet = buff;
  pkt.bytes = buff_len;
  pkt.b_o_s = 0;
  pkt.e_o_s = 0;
  pkt.granulepos = 0;
  pkt.packetno = 0;
  
  if ((res = vorbis_synthesis (dec->vb, &pkt)) != 0) return res;
  if ((res = vorbis_synthesis_blockin (dec->vd, dec->vb)) != 0) return res;
  if ((res = vorbis_synthesis_pcmout (dec->vd, NULL)) != 0) return res;
  else if (res == 0) vorbis_synthesis_read (dec->vd, 0);
  return res;
}

/* data_packet_pcmout: after calling header_packet_blockin with a new buffer,
   use its return value to instantiate a list/vector/array/...
   and pass a point to it to data_packet_pcmout to actually retrieve the
   available values.

   returns a count of the samples actually put into the provided storage,
   or -1 if the decoder initially reported an incorrect count
   (don't expect this error case to happen, but...). */

int data_packet_pcmout (vorbisdec *dec, int16_t **v) {

  float **pcm;
  int16_t *p = *v;
  int j, k, channels = dec->vi->channels;
  int sample_count = vorbis_synthesis_pcmout (dec->vd, &pcm);
  
  for (j = 0; j < sample_count; j++) {
    for (k = 0; k < channels; k++) {
      int32_t s = (int32_t) rint (32767.f * pcm[k][j]);
      if (s > 32767) s = 32767;
      if (s < -32768) s = -32768;
      *p++ = (int16_t) s;
    }
  }

  if (vorbis_synthesis_read (dec->vd, sample_count) < 0) return -1;
  return sample_count;
}

int stream_channels (vorbisdec *dec) {
  return dec->vi->channels;
}

int stream_rate (vorbisdec *dec) {
  return dec->vi->rate;
}

int main (void) {
  
  return 0;
  
}
