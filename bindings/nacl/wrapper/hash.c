#include "crypto_hash.h"

int crypto_hash_get_bytes (void) {
  return crypto_hash_BYTES;
}

int crypto_hash_wrap (unsigned char *h,
		      const unsigned char *m,
		      unsigned long long mlen) {
  return crypto_hash (h, m, mlen);
}
