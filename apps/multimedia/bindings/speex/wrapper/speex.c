#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <pulse/simple.h>
#include <pulse/error.h>
#include <ao/ao.h>
#include <speex/speex.h>
#include <speex/speex_echo.h>
#include <speex/speex_preprocess.h>

typedef struct SpeexEncoder {
  void *state;
  SpeexBits bits;
  int frame_size;
  int sampling_rate;

  SpeexPreprocessState *pp;
  SpeexEchoState *echo;
  int16_t *echo_frame; // used to save "the last input frame" for echo cancellation 
 
  pa_simple *pa;
} SpeexEncoder;

SpeexEncoder * new_speex_encoder (uint8_t tail_coeff,
				  unsigned int *frame_size) {
  int quality = 10, cpu = 10, denoise = 1;
  SpeexEncoder *enc = malloc (sizeof (SpeexEncoder));

  speex_bits_init (&enc->bits);
  enc->state = speex_encoder_init (&speex_uwb_mode);
  speex_encoder_ctl (enc->state, SPEEX_GET_FRAME_SIZE, &enc->frame_size);
  speex_encoder_ctl (enc->state, SPEEX_GET_SAMPLING_RATE, &enc->sampling_rate);
  speex_encoder_ctl (enc->state, SPEEX_SET_QUALITY, &quality);
  speex_encoder_ctl (enc->state, SPEEX_SET_COMPLEXITY, &cpu);
  *frame_size = enc->frame_size;

  enc->echo = speex_echo_state_init (enc->frame_size, enc->frame_size * tail_coeff);
  enc->echo_frame = calloc (enc->frame_size, sizeof (int16_t));
  enc->pp = speex_preprocess_state_init (enc->frame_size, enc->sampling_rate);
  speex_preprocess_ctl (enc->pp, SPEEX_PREPROCESS_SET_DENOISE, &denoise);
  speex_preprocess_ctl (enc->pp, SPEEX_PREPROCESS_SET_ECHO_STATE, enc->echo);

  pa_sample_spec ss;
  ss.rate = 32000;
  ss.channels = 1;
  ss.format = PA_SAMPLE_S16LE;
  enc->pa = pa_simple_new (NULL, "CREST", PA_STREAM_RECORD, NULL,
			   "Voice", &ss, NULL, NULL, NULL);

  return enc;
}

void delete_speex_encoder (SpeexEncoder *enc) {
  if (enc == NULL) return;
  if (enc->echo != NULL) speex_echo_state_destroy (enc->echo);
  if (enc->pp != NULL) speex_preprocess_state_destroy (enc->pp);
  if (enc->state != NULL) speex_encoder_destroy (enc->state);
  speex_bits_destroy (&enc->bits);
  free (enc->echo_frame);
  free (enc);
}

size_t speex_encoder_encode (SpeexEncoder *enc, size_t output_buff_size, char *output_buff) {

  int error = 0, i = 0, k = 0;
  size_t available;
  int16_t input_frame[enc->frame_size], output_frame[enc->frame_size];
  char input_buff[enc->frame_size * 2];

  // read Speex-defined amount of sound from PA server
  pa_simple_read (enc->pa, input_buff, enc->frame_size * 2, &error);

  // pack char* buffer into int16s. this is endianness-specific
  for (i = 0, k = 0; i < enc->frame_size; i++, k+=2) {
     input_frame[i] = (input_buff[k+1] << 8) | (input_buff[k] & 0xFF);
  }

  // unclear what order this "should" go in
  speex_preprocess_run (enc->pp, input_frame);
  speex_echo_cancellation (enc->echo, input_frame, enc->echo_frame, output_frame);
  
  // save the frame to run through echo cancellation on next pass
  memcpy (enc->echo_frame, output_frame, sizeof (output_frame));

  error = speex_encode_int (enc->state, output_frame, &enc->bits);
  speex_bits_insert_terminator (&enc->bits);
  available = speex_bits_write (&enc->bits, output_buff, output_buff_size);
  speex_bits_reset (&enc->bits);
 
  
  return available;
}

// ---------------------------------------

typedef struct SpeexDecoder {
  void *state;
  SpeexBits bits;
  int frame_size;
  int sampling_rate;

  ao_device *device;
} SpeexDecoder;

SpeexDecoder * new_speex_decoder (unsigned int frame_size) {
  ao_sample_format format;
  SpeexDecoder *dec = malloc (sizeof (SpeexDecoder));

  speex_bits_init (&dec->bits);
  dec->state = speex_decoder_init (&speex_uwb_mode);
  dec->frame_size = frame_size;

  ao_initialize ();
  format.bits = 16;
  format.rate = 32000;
  format.channels = 1;
  format.byte_format = AO_FMT_LITTLE;
  dec->device = ao_open_live (ao_driver_id ("pulse"), &format, NULL);

  return dec;
}

void delete_speex_decoder (SpeexDecoder *dec) {
  if (dec == NULL) return;
  if (dec->state != NULL) speex_decoder_destroy (dec->state);
  if (dec->device != NULL) {
    ao_close (dec->device);
    ao_shutdown ();
  }

  speex_bits_destroy (&dec->bits);
  free (dec);
}

void speex_decoder_decode (SpeexDecoder *dec, size_t available, char *output_buff) {
  int16_t output_frame[dec->frame_size];
  char play_buff[dec->frame_size * 2];
  int i = 0, k = 0, error = 0;

  speex_bits_read_from (&dec->bits, output_buff, available);
  error = speex_decode_int (dec->state, &dec->bits, output_frame);

  for (i = 0, k = 0; i < dec->frame_size; i++, k+=2) {
    play_buff[k] = output_frame[i] & 0xFF;
    play_buff[k+1] = (output_frame[i] >> 8) & 0xFF;
  }
  
  ao_play (dec->device, play_buff, k); 
}

// ---------------------------------------
/*
int main (void) {
  
  int frame_size;
  SpeexEncoder *enc = new_speex_encoder (3, &frame_size);
  SpeexDecoder *dec = new_speex_decoder (frame_size);

  // ------------------

  char output_buff[1000];
  size_t available;

  while (1) {
    // server side
    available = speex_encode (enc, 1000, output_buff);

    // client side
    speex_decode (dec, available, output_buff);
  };

  delete_speex_encoder (enc);
  delete_speex_decoder (dec);

  return 1;
  }*/
