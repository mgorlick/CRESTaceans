#include "crypto_sign.h"

int crypto_sign_get_publickeybytes (void) {
    return crypto_sign_PUBLICKEYBYTES;
}

int crypto_sign_get_secretkeybytes (void) {
    return crypto_sign_SECRETKEYBYTES;
}

int crypto_sign_get_bytes (void) {
  return crypto_sign_BYTES;
}

int crypto_sign_keypair_wrap (unsigned char *pk, unsigned char *sk) {
  return crypto_sign_keypair (pk, sk);
}

int crypto_sign_wrap (unsigned char *sm,
		      unsigned long long *smlen,
		      const unsigned char *m,
		      unsigned long long mlen,
		      const unsigned char *sk) {
  return crypto_sign (sm, smlen, m, mlen, sk);

}

int crypto_sign_open_wrap (unsigned char *m,
			   unsigned long long *mlen,
			   const unsigned char *sm,
			   unsigned long long smlen,
			   const unsigned char *pk) {
  return crypto_sign_open (m, mlen, sm, smlen, pk);
}
