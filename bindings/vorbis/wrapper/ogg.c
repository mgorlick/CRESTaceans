#include <stdlib.h>
#include <string.h>
#include <ogg/ogg.h>

#include "ogg.h"

ogg_packet* ogg_packet_new (void) {
  ogg_packet *p = malloc (sizeof (ogg_packet));
  p->packet = NULL;
  p->bytes = 0;
  p->b_o_s = 0;
  p->e_o_s = 0;
  p->granulepos = 0;
  p->packetno = 0;
  return p;
}

void ogg_packet_delete (ogg_packet *p) {
  ogg_packet_clear (p);
  free (p);
}

ogg_packet* ogg_packet_copy (ogg_packet *p) {
  ogg_packet *ret = ogg_packet_new ();
  ret->packet = calloc (p->bytes, sizeof (unsigned char));
  memcpy (ret->packet, p->packet, p->bytes);
  ret->bytes = p->bytes;
  ret->b_o_s = p->b_o_s;
  ret->e_o_s = p->e_o_s;
  ret->granulepos = p->granulepos;
  ret->packetno = p->packetno;

  return ret;
}

long ogg_packet_size (ogg_packet *p) {
  return p->bytes;
}

unsigned char* ogg_packet_data (ogg_packet *p) {
  return p->packet;
}
