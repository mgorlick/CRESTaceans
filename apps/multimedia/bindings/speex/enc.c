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
  void *enc = speex_encoder_init (&speex_wb_mode);
  int frame_size;
  speex_encoder_ctl (enc, SPEEX_GET_FRAME_SIZE, &frame_size);

  SpeexBits dec_bits;
  speex_bits_init (&dec_bits);
  void *dec = speex_decoder_init (&speex_wb_mode);

  // -----------------

  pa_sample_spec ss;
  ss.rate = 16000;
  ss.channels = 1;
  ss.format = PA_SAMPLE_S16BE;
  pa_simple *s = pa_simple_new (NULL, "CREST", PA_STREAM_RECORD, NULL,
				"Voice", &ss, NULL, NULL, NULL);

  // ------------------

  ao_initialize ();
  ao_sample_format format;
  format.bits = 16;
  format.rate =16000;
  format.channels = 1;
  format.byte_format = AO_FMT_BIG;
  ao_device *device = ao_open_live (ao_driver_id ("pulse"), &format, NULL);

  // ------------------

  int i = 0, k = 0, error = 0;
  int sample_count = frame_size * sizeof (short);
  char input_buff[sample_count], output_buff[10000];
  int available = 0;
  
  short *input_frame = calloc (frame_size, sizeof (short));
  short *output_frame = calloc (frame_size, sizeof (short));
  
  // ------------------

  while (1) {

    pa_simple_read (s, input_buff, sample_count, &error);

    for (i = 0, k = 0; i < frame_size; i++, k+=2) {
      //input_frame[i] = (short) input_buff[k];
      input_frame[i] = (input_buff[k+1] << 8) + input_buff[k];
    }

    /*error = speex_encode_int (enc, input_frame, &enc_bits);
    speex_bits_insert_terminator (&enc_bits);
    available = speex_bits_write (&enc_bits, output_buff, 10000);
    speex_bits_reset (&enc_bits);
    
    speex_bits_read_from (&dec_bits, output_buff, available);
    error = speex_decode_int (dec, &dec_bits, input_frame);*/

    for (i = 0, k = 0; i < frame_size; i++, k+=2) {
      input_buff[k+1] = (input_frame[i] >> 8) & 0xFF;
      input_buff[k] = input_frame[i] & 0xFF;
    }

    ao_play (device, input_buff, sample_count);
    
  };

  return 1;
}
