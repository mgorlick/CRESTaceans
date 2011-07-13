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
  SpeexEchoState *echo;
  SpeexPreprocessState *pp;
  int frame_size;
  int sampling_rate;
  int16_t *echo_frame;
} SpeexEncoder;

SpeexEncoder * new_speex_encoder (void) {
  int quality = 10, cpu = 10, denoise = 1;
  SpeexEncoder *enc = malloc (sizeof (SpeexEncoder));

  speex_bits_init (&enc->bits);
  enc->state = speex_encoder_init (&speex_uwb_mode);
  speex_encoder_ctl (enc->state, SPEEX_GET_FRAME_SIZE, &enc->frame_size);
  speex_encoder_ctl (enc->state, SPEEX_GET_SAMPLING_RATE, &enc->sampling_rate);
  speex_encoder_ctl (enc->state, SPEEX_SET_QUALITY, &quality);
  speex_encoder_ctl (enc->state, SPEEX_SET_COMPLEXITY, &cpu);

  enc->echo = speex_echo_state_init (enc->frame_size, enc->frame_size * 5);
  enc->pp = speex_preprocess_state_init (enc->frame_size, enc->sampling_rate);
  speex_preprocess_ctl (enc->pp, SPEEX_PREPROCESS_SET_DENOISE, &denoise);
  speex_preprocess_ctl (enc->pp, SPEEX_PREPROCESS_SET_ECHO_STATE, enc->echo);

  enc->echo_frame = calloc (enc->frame_size, sizeof (int16_t));

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

int encode (SpeexEncoder *enc, size_t input_buff_size, const char *input_buff,
	    size_t output_buff_size, char *output_buff) {
  
  if (input_buff_size != enc->frame_size * 2) {
    printf ("Input buffer size mismatched\n");
    return -1;
  }

  // server side
  int error = 0, i = 0, k = 0, available;
  int16_t input_frame[enc->frame_size], output_frame[enc->frame_size];
  
  for (i = 0, k = 0; i < enc->frame_size; i++, k+=2) {
     input_frame[i] = (input_buff[k+1] << 8) | (input_buff[k] & 0xFF);
  }
  
  speex_preprocess_run (enc->pp, input_frame);    
  speex_echo_cancellation (enc->echo, input_frame, enc->echo_frame, output_frame);  
  error = speex_encode_int (enc->state, output_frame, &enc->bits);
  speex_bits_insert_terminator (&enc->bits);
  available = speex_bits_write (&enc->bits, output_buff, output_buff_size);
  speex_bits_reset (&enc->bits);
  
  memcpy (enc->echo_frame, input_frame, sizeof (input_frame)); // save for next input pass
  
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

SpeexDecoder * new_speex_decoder (void) {
  ao_sample_format format;
  SpeexDecoder *dec = malloc (sizeof (SpeexDecoder));

  speex_bits_init (&dec->bits);
  dec->state = speex_decoder_init (&speex_uwb_mode);
  
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

void decode (SpeexDecoder *dec, char *output_buff, int available) {
  int16_t output_frame[dec->frame_size];
  int i = 0, k = 0, error = 0;

  speex_bits_read_from (&dec->bits, output_buff, available);
  error = speex_decode_int (dec->state, &dec->bits, output_frame);

  for (i = 0, k = 0; i < dec->frame_size; i++, k+=2) {
    output_buff[k+1] = (output_frame[i] >> 8) & 0xFF;
    output_buff[k] = output_frame[i] & 0xFF;
  }
  
  ao_play (dec->device, output_buff, k); 
}

// ---------------------------------------

int main (void) {

  SpeexEncoder *enc = new_speex_encoder ();
  SpeexDecoder *dec = new_speex_decoder ();
  dec->frame_size = enc->frame_size;

  // ----------------

  pa_sample_spec ss;
  ss.rate = 32000;
  ss.channels = 1;
  ss.format = PA_SAMPLE_S16LE;
  pa_simple *s = pa_simple_new (NULL, "CREST", PA_STREAM_RECORD, NULL,
				"Voice", &ss, NULL, NULL, NULL);

  // ------------------

  int error = 0;
  int bytes_count = enc->frame_size * 2;
  
  char input_buff[bytes_count], output_buff[65535];
  int available;

  while (1) {
    // server side
    pa_simple_read (s, input_buff, bytes_count, &error);
    available = encode (enc, bytes_count, input_buff, 65535, output_buff);

    // client side
    decode (dec, output_buff, available);
  };

  delete_speex_encoder (enc);
  delete_speex_decoder (dec);

  return 1;
}
