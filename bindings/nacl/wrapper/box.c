#include "crypto_box.h"

int crypto_box_get_publickeybytes (void) {
  return crypto_box_PUBLICKEYBYTES;
}

int crypto_box_get_secretkeybytes (void) {
  return crypto_box_SECRETKEYBYTES;
}

int crypto_box_get_noncebytes (void) {
  return crypto_box_NONCEBYTES;
}

int crypto_box_get_beforenmbytes (void) {
  return crypto_box_BEFORENMBYTES;
}

int crypto_box_get_zerobytes (void) {
  return crypto_box_ZEROBYTES;
}

int crypto_box_get_boxzerobytes (void) {
  return crypto_box_BOXZEROBYTES;
}

int crypto_box_keypair_wrap (unsigned char *pk, unsigned char *sk) {
  return crypto_box_keypair (pk, sk);
}

int crypto_box_wrap (unsigned char *c,
		     unsigned char *m,
		     unsigned long long mlen,
		     unsigned char *n,
		     unsigned char *pk,
		     unsigned char *sk) {
  return crypto_box (c, m, mlen, n, pk, sk);
}

int crypto_box_open_wrap (unsigned char *m,
		     unsigned char *c,
		     unsigned long long clen,
		     unsigned char *n,
		     unsigned char *pk,
		     unsigned char *sk) {
  return crypto_box_open (m, c, clen, n, pk, sk);
}

int crypto_box_beforenm_wrap (unsigned char *k,
			       unsigned char *pk,
			       unsigned char *sk) {
  return crypto_box_beforenm (k, pk, sk);
}

int crypto_box_afternm_wrap (unsigned char *c,
					unsigned char *m,
					unsigned long long mlen,
					unsigned char *n,
					unsigned char *k) {
  return crypto_box_afternm (c, m, mlen, n, k);
}

int crypto_box_open_afternm_wrap (unsigned char *m,
			     unsigned char *c,
			     unsigned long long clen,
			     unsigned char *n,
			     unsigned char *k) {
  return crypto_box_open_afternm (m, c, clen, n, k);
}
