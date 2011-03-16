#include <stdlib.h>
#include <arpa/inet.h>


long bstofs_naive (unsigned char* buffer, long buffer_length,
                   float (*floats)[buffer_length]) {
  long i, j;
  float* data = (float*) buffer;
  for (i = 0, j = 0; i < buffer_length; i += sizeof (float), j++) {
    (*floats)[j] = *data++;
  }
  return j;
}

long bstofs_htno (unsigned char* buffer, long buffer_length,
                  float (*floats)[buffer_length]) {
  long i, j;
  
  for (i = 0, j = 0; i < buffer_length; i += sizeof (uint32_t), j++) {
    (*floats)[j] = (float) ntohl ((uint32_t) buffer[i]);
  }
  return j;

}
