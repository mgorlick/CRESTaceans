#include <stdlib.h>
#include <string.h>
#include <stdio.h>
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
  
  *ret = *p;
  ret->packet = calloc (p->bytes, sizeof (unsigned char));
  memcpy (ret->packet, p->packet, p->bytes);

  return ret;
}

long ogg_packet_size (ogg_packet *p) {
  return p->bytes;
}

unsigned char* ogg_packet_data (ogg_packet *p) {
  return p->packet;
}

typedef struct OggDemux {
  ogg_sync_state *sync;
  ogg_stream_state *stream;
  int stream_is_init;
} OggDemux;

OggDemux *ogg_demux_new (void) {
  OggDemux *d = malloc (sizeof (OggDemux));
  if (d == NULL) goto no_demux;
  
  d->sync = malloc (sizeof (ogg_sync_state));
  if (d->sync == NULL) goto no_sync;
  if (0 > ogg_sync_init (d->sync)) goto no_sync_init;
  d->stream = malloc (sizeof (ogg_stream_state));
  if (d->stream == NULL) goto no_stream;

  d->stream_is_init = 0;
  return d;

no_stream:
no_sync_init:
  free (d->sync);
no_sync:
  free (d);
no_demux:
  printf ("Error allocating and initializing demuxer structs\n");
  return NULL;
}

void ogg_demux_delete (OggDemux *d) {
  if (d == NULL) return;

  ogg_stream_destroy (d->stream);
  ogg_sync_destroy (d->sync);
  free (d);
}

int handle_page (OggDemux *d, ogg_page *pg, ogg_packet_for_each f) {
  ogg_packet p;
  int sstate;
  
  if (!d->stream_is_init) {    
    if (0 > ogg_stream_init (d->stream, ogg_page_serialno (pg)))
      goto no_streaminit;
    d->stream_is_init = 1;
  }
  
  /*else if (d->stream->serialno != ogg_page_serialno (pg)) {
    XXX fixme accommodate multiple streams in file
    }*/
  
  if (0 > ogg_stream_pagein (d->stream, pg)) { goto no_pagein; }
  while (0 < ogg_stream_packetout (d->stream, &p)) { f (&p); }
  
  return 1;

no_streaminit:
  printf ("error initializing stream\n");
  return 0;
no_pagein:
  printf ("stream_pagein error\n");
  return 0;
}

int ogg_demux_data (OggDemux *d, size_t size, unsigned char *data,
                    ogg_packet_for_each f) {
  char *buffer;
  int sstate, more = 1;

  /* steps 1-3 */
  buffer = ogg_sync_buffer (d->sync, size);
  memcpy (buffer, data, size);
  sstate = ogg_sync_wrote (d->sync, size);
  if (sstate < 0) printf ("write failed\n");

  while (more) {
    ogg_page pg;
    switch (ogg_sync_pageout (d->sync, &pg)) {
      case 0:
        more = 0;
        break;
      case 1:
        more = handle_page (d, &pg, f);
        break;
      default:
        more = 1;
        break;
    }
  }
    
  return sstate == 0 ? 1 : 0;
}
