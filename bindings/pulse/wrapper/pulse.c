#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <pulse/pulseaudio.h>
#include <pulse/simple.h>

typedef struct PulseSrc {
  pa_simple *s;
} PulseSrc;

void pulsesrc_delete (PulseSrc *src) {
  if (src == NULL) return;

  pa_simple_free (src->s);
  free (src);
}

PulseSrc* pulsesrc_new (void) {
  PulseSrc *src;
  pa_sample_spec ss;

  src = malloc (sizeof (PulseSrc));
  if (src == NULL) goto no_src;

  ss.format = PA_SAMPLE_S16NE;
  ss.channels = 2;
  ss.rate = 44100;
  src->s = pa_simple_new (NULL, "CREST Pipeline", PA_STREAM_RECORD,
                          NULL, /* default device name */
                          "Voice recording", &ss, NULL, NULL, NULL);
  
  if (src->s == NULL) goto no_pa;
  return src;
  
no_src:
  printf ("Couldn't malloc PulseSrc component\n");
  return NULL;
no_pa:
  printf ("Couldn't connect to PulseAudio device\n");
  free (src);
  return NULL;
}
