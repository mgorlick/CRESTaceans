#include <stdlib.h>
#include <ogg/ogg.h>

ogg_packet* ogg_packet_new (void) {
  return malloc (sizeof (ogg_packet));
}

void ogg_packet_delete (ogg_packet* p) {
  ogg_packet_clear (p);
  free (p);
}

long ogg_packet_size (ogg_packet* p) {
  return p->bytes;
}

void ogg_packet_copy_data (ogg_packet* p, unsigned char** buffer) {
  long i;
  long j = p->bytes;
  unsigned char* d = p->packet;
  unsigned char* b = *buffer;

  for (i = 0; i < j; i++) *b++ = *d++;
}
