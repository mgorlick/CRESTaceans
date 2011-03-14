#include <stdlib.h>
#include <stdio.h>
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

unsigned char* ogg_packet_data (ogg_packet* p) {
  return p->packet;
}
