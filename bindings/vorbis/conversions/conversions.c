#include <stdio.h>
#include <string.h>
#include <vorbis/codec.h>

int pcmout_wrapper (vorbis_dsp_state* dspstate, float** v, int channels) {

  float** pcm;
  int sample_count;
  /*  int i = 0, j, k;
      float* p = *v;*/
  
  sample_count = vorbis_synthesis_pcmout (dspstate, &pcm);
  /*  printf ("sample_count = %d\n", sample_count); *
  if (sample_count > 0) {
    for (j = 0; j < sample_count; j++) {
      for (k = 0; k < channels; k++) {
         *p++ = pcm[k][j];
         printf ("    v[%d] = %f (%f)\n", i, v[i], pcm[k][j]);
        i++;
      }
    }
    }*/
  return sample_count;
}

void copy_buffer_to_packet (ogg_packet* pkt, unsigned char** buff) {
  memcpy (pkt->packet, *buff, pkt->bytes);
}

void print_buffer (ogg_packet* pkt) {
  int i;
  unsigned char* buf = pkt->packet;

  for (i = 0; i < pkt->bytes; i++) {
    printf ("%c", *buf);
    buf++;
  }
  printf ("\n");
}

int main (void) {
  return 0;
}
