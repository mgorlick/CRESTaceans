typedef void (*ogg_packet_for_each) (ogg_packet *p);

ogg_packet* ogg_packet_new (void);
void ogg_packet_delete (ogg_packet* p);
