#include <stdio.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <vorbis/codec.h>
#include <vorbis/vorbisenc.h>

int RATE = 44100;
int CHANNELS = 1;
float QUALITY = 0.5;

typedef struct {
  int is_init;
  vorbis_info* vi;
  vorbis_comment* vc;
  vorbis_dsp_state* vd;
  vorbis_block* vb;
} vorbisenc;

typedef enum {
  VORBIS_ID_PACKET,
  VORBIS_COMMENT_PACKET,
  VORBIS_CODEBOOK_PACKET,
  VORBIS_DATA_PACKET
} vorbis_packet_type;
 
typedef int (*vorbisenc_process_packet_ft) (ogg_packet* p, vorbis_packet_type t);

vorbisenc* vorbisenc_new (void) {

  vorbisenc* enc;

  enc = malloc (sizeof (vorbisenc));
  enc->is_init = 0;
  enc->vi = malloc (sizeof (vorbis_info));
  enc->vc = malloc (sizeof (vorbis_comment));
  enc->vd = malloc (sizeof (vorbis_dsp_state));
  enc->vb = malloc (sizeof (vorbis_block));

  vorbis_info_init (enc->vi);
  vorbis_comment_init (enc->vc);
  vorbis_encode_init_vbr (enc->vi, CHANNELS, RATE, QUALITY); 
  vorbis_analysis_init (enc->vd, enc->vi);

  return enc;
  
}

void vorbisenc_delete (vorbisenc* enc) {
  if (enc->is_init) {
    vorbis_block_clear (enc->vb);
  }
  vorbis_dsp_clear (enc->vd);
  vorbis_comment_clear (enc->vc);
  vorbis_info_clear (enc->vi);

  free (enc->vb);
  free (enc->vd);
  free (enc->vc);
  free (enc->vi);
  free (enc);
}

int vorbisenc_is_init (vorbisenc* enc) {
  return enc->is_init;
}

int vorbisenc_init (vorbisenc* enc, vorbisenc_process_packet_ft f) {
  int r;

  ogg_packet id;
  ogg_packet comment;
  ogg_packet codebook;

  r = vorbis_analysis_headerout (enc->vd, enc->vc, &id, &comment, &codebook);
  if (r == 0) {
    vorbis_block_init (enc->vd, enc->vb);
    enc->is_init = 1;
    f (&id, VORBIS_ID_PACKET);
    f (&comment, VORBIS_COMMENT_PACKET);
    f (&codebook, VORBIS_CODEBOOK_PACKET);
  }
  return r;
}

long bytes_to_floats_htno (unsigned char* buffer, long buffer_length, /* input parameters */
                           float samples[]) /* output parameters */ {
  long i, j;
  
  for (i = 0, j = 0; i < buffer_length; i += sizeof (uint32_t), j++) {
    samples[j] = (float) ntohl ((uint32_t) buffer[i]);
  }
  return j;

}

int vorbisenc_encode_pcm_samples (vorbisenc* enc, unsigned char* buffer, long buffer_length,
                                   vorbisenc_process_packet_ft f) {
  int r, keep_going = 1; /* error signals */
  long i, j;

  /* assume the buffer has already been aligned
     so that buffer[0] starts the first float */

  /*float samples[buffer_length / (sizeof (uint32_t))];
    long sample_count = bytes_to_floats_htno (buffer, buffer_length, samples);*/
  long sample_count = buffer_length / (sizeof (float));
  
  float **vorbis_input = vorbis_analysis_buffer (enc->vd, sample_count);

  ogg_packet op;
  
  /* assume that the buffer represents a single-channel stream
     and duplicate the buffer into N channels */
  for (i = 0; i < CHANNELS; i++) {
    float* data = (float*) buffer;
    for (j = 0; j < sample_count; j++) {
      /*vorbis_input[i][j] = samples[j];*/
      vorbis_input[i][j] = *data++;
    }
  }
  
  if ((r = vorbis_analysis_wrote (enc->vd, sample_count)) < 0) {
    printf ("sample encoding failed when calling vorbis_analysis_wrote, ct = %ld, error = %d\n", sample_count, r);
    return r;
  }

  while (keep_going && (r = vorbis_analysis_blockout (enc->vd, enc->vb)) == 1) {
    r = vorbis_analysis (enc->vb, &op);
    if (r < 0) {
      printf ("sample encoding failed when calling vorbis_analysis, ct = %ld, error = %d\n", sample_count, r);
      keep_going = 0;
    } else {
      keep_going = f (&op, VORBIS_DATA_PACKET);
      if (keep_going < 1) {
        printf ("higher layer signaled premature end of encoding from callback return val, ct = %ld", sample_count);
      }
    }
  }

  if (r < 0) {
    printf ("sample encoding failed when calling vorbis_analysis_blockout, ct = %ld, error = %d\n", sample_count, r);
    return r;
  }

  return 1;
}
