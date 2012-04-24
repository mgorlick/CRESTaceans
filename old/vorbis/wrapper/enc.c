#include <stdio.h>
#include <stdlib.h>
#include <vorbis/codec.h>
#include <vorbis/vorbisenc.h>

#include "conversions.h"
#include "ogg.h"

long SAMPLE_BUFFER_SIZE = 100000;

typedef struct {
  vorbis_info* vi;
  vorbis_comment* vc;
  vorbis_dsp_state* vd;
  vorbis_block* vb;
  ogg_packet *op;
  
  int is_init;
  int channels;
  int rate;
  float quality;

  float *sample_buffer;
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

int vorbisenc_is_init (vorbisenc* enc) {
  return enc->is_init;
}

vorbisenc* vorbisenc_new (int channels, int rate, float quality) {

  vorbisenc* enc;

  enc = malloc (sizeof (vorbisenc));
  

  enc->vi = malloc (sizeof (vorbis_info));
  enc->vc = malloc (sizeof (vorbis_comment));
  enc->vd = malloc (sizeof (vorbis_dsp_state));
  enc->vb = malloc (sizeof (vorbis_block));
  enc->op = ogg_packet_new ();
  
  enc->is_init = 0;  
  enc->channels = channels;
  enc->rate = rate;
  enc->quality = quality;

  enc->sample_buffer = calloc (SAMPLE_BUFFER_SIZE, sizeof (float));

  /* step 1 */
  vorbis_info_init (enc->vi);
  vorbis_encode_init_vbr (enc->vi, channels, rate, 1.0);
  /* step 2 */
  vorbis_analysis_init (enc->vd, enc->vi);
  vorbis_block_init (enc->vd, enc->vb);
  /* step 3a */
  vorbis_comment_init (enc->vc);
  return enc;
  
}

vorbisenc* vorbisenc_init (int channels, int rate, float quality,
                           vorbisenc_process_packet_ft f) {
  int r;
  ogg_packet id;
  ogg_packet comment;
  ogg_packet codebook;
  vorbisenc* enc;

  enc = vorbisenc_new (channels, rate, quality);
  /* step 3b */
  r = vorbis_analysis_headerout (enc->vd, enc->vc, &id, &comment, &codebook);
  if (r == 0) {
    f (&id, VORBIS_ID_PACKET);
    f (&comment, VORBIS_COMMENT_PACKET);
    f (&codebook, VORBIS_CODEBOOK_PACKET);
    /* step 4 */
    enc->is_init = 1;
  }
  
  return enc;
}

int vorbisenc_encode_pcm_samples (vorbisenc* enc,
                                  unsigned char* buffer, long buffer_length,
                                  vorbis_byte_conversion_type b,
                                  vorbisenc_process_packet_ft f) {
  int r; /* error signals */
  long i, j, size;
  float **vorbis_input, *samples = enc->sample_buffer;

  /*switch (b) {
    case VORBIS_NAIVE:
      size = bstofs_naive (buffer, buffer_length, enc->channels, samples);
      break;
    default:
      size = bstofs_ntoh (buffer, buffer_length, enc->channels, samples);
      break;
      }*/

  /* step 5.1 */
  samples = (float *) buffer;
  size = buffer_length / (enc->channels * sizeof (float));
    
  vorbis_input = vorbis_analysis_buffer (enc->vd, size);
  
  /* this deinterleaves the samples:
     assume they were interleaved in the input buffer */
  for (i = 0; i < size; i++) {
    for (j = 0; j < enc->channels; j++) {
        vorbis_input[j][i] = *samples++;
    }
  }
  
  if ((r = vorbis_analysis_wrote (enc->vd, size)) < 0) {
    printf ("sample encoding failed, ct = %ld, error = %d\n", size, r);
    return r;
  }

  /* step 5.2 */
  while (vorbis_analysis_blockout (enc->vd, enc->vb)) {
    /* step 5.2.1 */
    r = vorbis_analysis (enc->vb, NULL);
    if (r < 0) {
      break;
    } else {
      /* 5.2.2 and 5.2.3 */
      vorbis_bitrate_addblock (enc->vb);
      while (vorbis_bitrate_flushpacket (enc->vd, enc->op)) {
        f (enc->op, VORBIS_DATA_PACKET);
      }
      
      /* don't free unowned memory in ogg_packet_delete
         if it's called after this func returns
         since the op's buffer points to libvorbis memory */
      enc->op->packet = NULL; 
      enc->op->bytes = 0;
   }
  }

  if (r < 0) {
    printf ("sample encoding failed, ct = %ld, error = %d\n", size, r);
    return r;
  }

  return 1;
}

/* FIXME: implement step 6, end of stream packet generation */

void vorbisenc_delete (vorbisenc* enc) {

  if (!enc) return;

  free (enc->sample_buffer);

  /* step 7 */
  if (enc->is_init) {
    vorbis_block_clear (enc->vb);
  }
  vorbis_dsp_clear (enc->vd);
  vorbis_comment_clear (enc->vc);
  vorbis_info_clear (enc->vi);
  ogg_packet_delete (enc->op);

  free (enc->vb);
  free (enc->vd);
  free (enc->vc);
  free (enc->vi);
  free (enc);
}

