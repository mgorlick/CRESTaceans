#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <vorbis/codec.h>
#include <ao/ao.h>

void init (void) {
  ao_initialize ();
}

typedef struct {
  int block_is_init;
  int dsp_is_init;
  int ao_is_init;
  
  vorbis_info* vi;
  vorbis_comment* vc;
  vorbis_dsp_state* vd;
  vorbis_block* vb;

  ao_device *device;
  char *sample_buffer;
  long sample_buffer_size;
} vorbisdec;

int stream_channels (vorbisdec *dec) {
  return dec->vi->channels;
}

int stream_rate (vorbisdec *dec) {
  return dec->vi->rate;
}

int vorbisdec_is_init (vorbisdec* dec) {
  return dec->block_is_init && dec->dsp_is_init && dec->ao_is_init;
}

void print_stream_info (vorbisdec* dec) {  
  printf ("version: %d\n", dec->vi->version);
  printf ("channels: %d\n", dec->vi->channels);
  printf ("rate: %ld\n", dec->vi->rate);
}

int main (void) {
  return 0;  
}

void vorbisdec_delete (vorbisdec* dec) {
  if (dec->block_is_init) {
    vorbis_block_clear (dec->vb);
  }
  if (dec->dsp_is_init) {
    vorbis_dsp_clear (dec->vd);
  }
  vorbis_comment_clear (dec->vc);
  vorbis_info_clear (dec->vi);

  free (dec->vb);
  free (dec->vd);
  free (dec->vc);
  free (dec->vi);

  if (dec->ao_is_init) {
    ao_close (dec->device);
    free (dec->sample_buffer);
  }
  
  free (dec);
}

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

  dec->block_is_init = 0;
  dec->dsp_is_init = 0;
  dec->vi = vi;
  dec->vc = vc;
  dec->vd = vd;
  dec->vb = vb;
  vorbis_info_init (dec->vi);
  vorbis_comment_init (dec->vc);

  dec->device = NULL;
  dec->ao_is_init = 0;

  return dec;
}

int vorbisdec_finish_init (vorbisdec* dec) {

  int res;
  ao_device *device;

  ao_sample_format format;
  memset (&format, 0, sizeof (format));

  res = vorbis_synthesis_init (dec->vd, dec->vi);
  if (res == 0) {
    dec->dsp_is_init = 1;
    res = vorbis_block_init (dec->vd, dec->vb);
    if (res == 0) {
      dec->block_is_init = 1;

      /* we have all the info needed to set up ao device
         once the three header packets are in
         (actually, we have the info before that,
         but this simplifies control flow) */
      format.bits = 16;
      format.channels = dec->vi->channels;
      format.rate = dec->vi->rate;
      format.byte_format = AO_FMT_NATIVE;
      /* format.matrix = (dec->vi->channels == 1 ? "M" : "L,R"); */

      /* open up the driver */
      device = ao_open_live (ao_driver_id ("pulse"), &format, NULL);
      if (!device) {
        printf ("Error opening device\n");
        goto err;
      }
      
      /* device OK, allocate the rest of the ao stuff */
      dec->device = device;
      dec->sample_buffer_size = format.bits * format.channels * format.rate;
      dec->sample_buffer = calloc (dec->sample_buffer_size, sizeof (char));
      memset (dec->sample_buffer, 0, dec->sample_buffer_size);
      dec->ao_is_init = 1;
      
      return 0;
    }
  }

err:
  return -1;
}

/* header_packet_in: process one of the header packets in the stream
   (identified by (buffer[0] && 1 == 1). decoder should call this with
   the first three packets: expect the identification packet, comment
   packet and type packet in order. Once all three have been called,
   the vorbisdec is ready to use for synthesizing data packets.

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
      if (hi == 0 && vorbisdec_finish_init (dec) == 0) return 2;
      break;
    default: /* not a valid header packet */
      hi = -1;
  }
  return hi;
}

inline int16_t float_to_int16 (float v) {
  int32_t s;
  s = 32767 * v;
  if (s > 32767) s = 32767;
  if (s < -32768) s = -32768;
  return s;
}

int data_packet_pcmout (vorbisdec *dec) {

  float **pcm;
  int i, j, k = 0;
  int sample_count = vorbis_synthesis_pcmout (dec->vd, &pcm);
  int16_t sample;
  char *buffer = dec->sample_buffer;
  
  for (j = 0; j < sample_count; j++) {
    for (i = 0; i < dec->vi->channels; i++) {
      sample = float_to_int16 (pcm[i][j]);
      buffer[k] = sample & 0xff;
      buffer[k+1] = (sample >> 8) & 0xff;
      k += 2;
    }
  }

  ao_play (dec->device, buffer, k);
  vorbis_synthesis_read (dec->vd, sample_count);

  return sample_count;
}

/* header_packet_blockin: process one of the header packets in the stream.
   decoder should call this with any non-empty data packets after header_packet_in
   has been successfully called three times with the three header packets.

   returns -1 if error processing data packet; or 0 or other non-negative
   number to indicate how many samples are available to read. decoder
   must call data_packet_pcmout after data_packet_blockin returns. */
int data_packet_blockin (vorbisdec* dec, unsigned char *buff, long buff_len) {
  
  ogg_packet pkt;
  int res;
  
  if (buff_len < 0 || !vorbisdec_is_init (dec)) {
    printf ("Error: trying to use uninitialized vorbis decoder\n");
    return -1;
  }

  pkt.packet = buff;
  pkt.bytes = buff_len;
  pkt.b_o_s = 0;
  pkt.e_o_s = 0;
  pkt.granulepos = 0;
  pkt.packetno = 0;
  
  if ((res = vorbis_synthesis (dec->vb, &pkt)) != 0) {
    return res;
  }
  if ((res = vorbis_synthesis_blockin (dec->vd, dec->vb)) != 0) return res;
  return data_packet_pcmout (dec);
}
