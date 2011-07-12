#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <pulse/simple.h>
#include <pulse/error.h>
#include <ao/ao.h>
#include <speex/speex.h>
#include <speex/speex_echo.h>

typedef struct SpeexEncoder {
  void *state;
  SpeexBits bits;
  SpeexEchoState *echo;
  int frame_size;
} SpeexEncoder;

typedef struct SpeexDecoder {
  void *state;
  SpeexBits bits;
  int frame_size;
  ao_device *device;
} SpeexDecoder;

SpeexEncoder * new_speex_encoder (void) {
  int quality = 10, cpu = 10;
  SpeexEncoder *enc = malloc (sizeof (SpeexEncoder));

  speex_bits_init (&enc->bits);
  enc->state = speex_encoder_init (&speex_uwb_mode);

  speex_encoder_ctl (enc->state, SPEEX_GET_FRAME_SIZE, &enc->frame_size);
  speex_encoder_ctl (enc->state, SPEEX_SET_QUALITY, &quality);
  speex_encoder_ctl (enc->state, SPEEX_SET_COMPLEXITY, &cpu);

  enc->echo = speex_echo_state_init (enc->frame_size, enc->frame_size * 5);

  return enc;
}

int encode (SpeexEncoder *enc, const char *input_buff,
	    int16_t *input_frame, const int16_t *echo_frame,
	    size_t output_buff_size,
	    // output
	    char *output_buff) {
  // server side
  int error = 0, i = 0, k = 0, available;
  int16_t output_frame[enc->frame_size];
   
  for (i = 0, k = 0; i < enc->frame_size; i++, k+=2) {
    input_frame[i] = (input_buff[k+1] << 8) | (input_buff[k] & 0xFF);
  }
      
  speex_echo_cancellation (enc->echo, input_frame, echo_frame, output_frame);
      
  error = speex_encode_int (enc->state, output_frame, &enc->bits);
  speex_bits_insert_terminator (&enc->bits);
  available = speex_bits_write (&enc->bits, output_buff, output_buff_size);
  speex_bits_reset (&enc->bits);
  return available;
}
    
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
  int bytes_count = enc->frame_size * sizeof (int16_t);
  
  int16_t *input_frame = calloc (enc->frame_size, sizeof (int16_t));
  int16_t *echo_frame = calloc (enc->frame_size, sizeof (int16_t));
  int16_t *tmp;
  
  while (1) {
    // server side
    char input_buff[bytes_count], output_buff[65535];
    int available;

    pa_simple_read (s, input_buff, bytes_count, &error);
    available = encode (enc, input_buff, input_frame, echo_frame, 65535, output_buff);
    // swap input and echo frame so we can run them
    // through echo cancellation on the next input pass
    tmp = input_frame;
    input_frame = echo_frame;
    echo_frame = tmp;

    // client side
    decode (dec, output_buff, available);
  };

  return 1;
}
