#include <stdio.h>
#include <stdlib.h>
#include <vorbis/codec.h>
#include <vorbis/vorbisenc.h>

#include "conversions.h"

typedef struct {
  vorbis_info* vi;
  vorbis_comment* vc;
  vorbis_dsp_state* vd;
  vorbis_block* vb;

  int is_init;
  
  int channels;
  int rate;
  float quality;
} vorbisenc;

typedef enum {
  VORBIS_ID_PACKET,
  VORBIS_COMMENT_PACKET,
  VORBIS_CODEBOOK_PACKET,
  VORBIS_DATA_PACKET
} vorbis_packet_type;

typedef enum {
  VORBIS_NAIVE,
  VORBIS_NTOH
} vorbis_byte_conversion_type;

typedef int (*vorbisenc_process_packet_ft) (ogg_packet* p, vorbis_packet_type t);

vorbisenc* vorbisenc_new (int channels, int rate, float quality) {

  vorbisenc* enc;

  enc = malloc (sizeof (vorbisenc));
  enc->is_init = 0;
  enc->vi = malloc (sizeof (vorbis_info));
  enc->vc = malloc (sizeof (vorbis_comment));
  enc->vd = malloc (sizeof (vorbis_dsp_state));
  enc->vb = malloc (sizeof (vorbis_block));
  enc->channels = channels;
  enc->rate = rate;
  enc->quality = quality;

  vorbis_info_init (enc->vi);
  vorbis_comment_init (enc->vc);
  vorbis_encode_init_vbr (enc->vi, channels, rate, quality); 
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

int vorbisenc_encode_pcm_samples (vorbisenc* enc, unsigned char* buffer, long buffer_length,
                                  vorbis_byte_conversion_type b, vorbisenc_process_packet_ft f) {
  int r, keep_going = 1; /* error signals */
  long i, j;
  ogg_packet op;
  float samples[buffer_length];
  long sample_count;

  switch (b) {
    case VORBIS_NAIVE:
      sample_count = bstofs_naive (buffer, buffer_length, samples);
      break;
    default:
      sample_count = bstofs_ntoh (buffer, buffer_length, samples);
      break;
  }

  float **vorbis_input = vorbis_analysis_buffer (enc->vd, sample_count);
  
  /* assume that the buffer represents a single-channel stream
     and duplicate the buffer into N channels */
  for (j = 0; j < sample_count; j++) {
    for (i = 0; i < enc->channels; i++) {  
        vorbis_input[i][j] = samples[j];
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
