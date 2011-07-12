#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <pulse/simple.h>
#include <pulse/error.h>
#include <ao/ao.h>
#include <speex/speex.h>
#include <speex/speex_echo.h>

int main (void) {

  SpeexBits enc_bits;
  speex_bits_init (&enc_bits);
  void *enc = speex_encoder_init (&speex_uwb_mode);
  int frame_size, quality = 10, cpu = 10;
  speex_encoder_ctl (enc, SPEEX_GET_FRAME_SIZE, &frame_size);
  speex_encoder_ctl (enc, SPEEX_SET_QUALITY, &quality);
  speex_encoder_ctl (enc, SPEEX_SET_COMPLEXITY, &cpu);

  SpeexBits dec_bits;
  speex_bits_init (&dec_bits);
  void *dec = speex_decoder_init (&speex_uwb_mode);

  SpeexEchoState *echo = speex_echo_state_init (frame_size, frame_size * 5);

  // -----------------

  pa_sample_spec ss;
  ss.rate = 32000;
  ss.channels = 1;
  ss.format = PA_SAMPLE_S16LE;
  pa_simple *s = pa_simple_new (NULL, "CREST", PA_STREAM_RECORD, NULL,
				"Voice", &ss, NULL, NULL, NULL);

  // ------------------

  ao_initialize ();
  ao_sample_format format;
  format.bits = 16;
  format.rate = 32000;
  format.channels = 1;
  format.byte_format = AO_FMT_LITTLE;
  ao_device *device = ao_open_live (ao_driver_id ("pulse"), &format, NULL);

  // ------------------

  int i = 0, k = 0, error = 0;
  int bytes_count = frame_size * sizeof (int16_t);
  int available = 0;
  
  char *input_buff = calloc (bytes_count, sizeof (char));
  char *output_buff = calloc (bytes_count, sizeof (char));

  int16_t *input_frame = calloc (frame_size, sizeof (int16_t));
  int16_t *output_frame = calloc (frame_size, sizeof (int16_t));
  int16_t *echo_frame = calloc (frame_size, sizeof (int16_t));
  int16_t *tmp;
  
  // ------------------

  while (1) {

    // server side

    // step 1: capture and convert to an int16_t buffer
    pa_simple_read (s, input_buff, bytes_count, &error);
    for (i = 0, k = 0; i < frame_size; i++, k+=2) {
      input_frame[i] = (input_buff[k+1] << 8) | (input_buff[k] & 0xFF);
    }

    // step 2: run input frame through echo cancellation
    speex_echo_cancellation (echo, input_frame, echo_frame, output_frame);

    // step 3: encode and write out echo-cancelled input frame
    error = speex_encode_int (enc, output_frame, &enc_bits);
    speex_bits_insert_terminator (&enc_bits);
    available = speex_bits_write (&enc_bits, output_buff, 10000);
    speex_bits_reset (&enc_bits);

    tmp = input_frame;
    input_frame = echo_frame;
    echo_frame = tmp;
    
    // client side

    // step 4: decode back to int16_t and convert to char*
    speex_bits_read_from (&dec_bits, output_buff, available);
    error = speex_decode_int (dec, &dec_bits, output_frame);

    for (i = 0, k = 0; i < frame_size; i++, k+=2) {
      output_buff[k+1] = (output_frame[i] >> 8) & 0xFF;
      output_buff[k] = output_frame[i] & 0xFF;
    }
    
    // step 5: play
    ao_play (device, output_buff, bytes_count);
  };

  return 1;
}
