#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <pulse/pulseaudio.h>
#include <pulse/simple.h>
#include <pulse/error.h>

typedef struct PulseSrc {
  pa_simple *s;
} PulseSrc;

void pulsesrc_delete (PulseSrc *src) {
  if (src == NULL) return;

  pa_simple_free (src->s);
  free (src);
}

PulseSrc* pulsesrc_new (uint32_t rate, uint8_t channels) {
  PulseSrc *src;
  int error;
  pa_sample_spec ss;
  
  ss.format = PA_SAMPLE_FLOAT32NE;
  ss.rate = rate;
  ss.channels = channels;

  src = malloc (sizeof (PulseSrc));
  if (src == NULL) goto no_src;

  src->s = pa_simple_new (NULL, "CREST Pipeline", PA_STREAM_RECORD,
                          NULL, /* default device name */
                          "Voice recording", &ss, NULL, NULL, &error);
  if (src->s == NULL) goto no_pa;
  
  return src;

no_src:
  printf ("Couldn't malloc PulseSrc component: %s\n", pa_strerror (error));
  return NULL;
no_pa:
  printf ("Couldn't connect to PulseAudio device: %s\n", pa_strerror (error));
  free (src);
  return NULL;
}

int pulsesrc_read (PulseSrc *src, const size_t size, unsigned char *buff) {
  int error;

  if (0 > pa_simple_read(src->s, buff, size, &error)) {
    fprintf(stderr, __FILE__": pa_simple_read() failed: %s\n", pa_strerror(error));
    return 0;
  }
  return 1;
}
