typedef long (*bstofs_t) (unsigned char* buffer, long buffer_length, float floats[buffer_length]);

bstofs_t bstofs_naive;
bstofs_t bstofs_ntoh;
