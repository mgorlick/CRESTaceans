#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>


long bstofs_naive (unsigned char* buffer, long buffer_length, int channels,
                   float floats[]) {
  long i, j;
  float* data = (float*) buffer;
  printf ("buffer length %ld\n", buffer_length);
  for (i = 0, j = 0; i < buffer_length; i += sizeof (float), j++) {
    floats[j] = *data++;
  }
  printf ("j = %ld\n", j);
  return buffer_length / (channels * (sizeof (float)));
}

long bstofs_ntoh (unsigned char* buffer, long buffer_length, int channels,
                  float floats[]) {
  long i, j;
  
  for (i = 0, j = 0; i < buffer_length; i += sizeof (uint32_t), j++) {
    floats[j] = (float) ntohl ((uint32_t) buffer[i]);
  }
  return buffer_length / (channels * (sizeof (uint32_t)));

}
