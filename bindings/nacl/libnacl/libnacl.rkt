#lang racket/base

(require "diviner.rkt"
         ffi/unsafe
         racket/runtime-path)
(provide (all-defined-out))

(define libnacl (ffi-lib "libnacl"))

(define (und->dash name)
  (regexp-replaces name '((#rx"-" "_"))))

(define-syntax define-constants
  (syntax-rules ()
    [(_ ahash id)
     (define id (let ([v (hash-ref ahash (und->dash 'id))]) (or (string->number v) v)))]
    [(_ ahash id1 id2 ...)
     (begin (define-constants ahash id1) (define-constants ahash id2 ...))]))

(define-syntax define-functions
  (syntax-rules ()
    [(_ ahash (id funspec))
     (define id (get-ffi-obj (hash-ref ahash (und->dash 'id)) libnacl funspec))]
    [(_ ahash e1 e2 ...)
     (begin (define-functions ahash e1) (define-functions ahash e2 ...))]))

(define-runtime-path box-header "headers/crypto_box.h")

(define cryptobox (get-real-names box-header))

(define-constants cryptobox
  crypto-box-SECRETKEYBYTES crypto-box-PUBLICKEYBYTES crypto-box-ZEROBYTES
  crypto-box-BEFORENMBYTES crypto-box-BOXZEROBYTES crypto-box-NONCEBYTES
  crypto-box-IMPLEMENTATION crypto-box-PRIMITIVE)

(define-functions cryptobox
  #|C NaCl provides a crypto_box_keypair function callable as follows:
     #include "crypto_box.h"
     
     unsigned char pk[crypto_box_PUBLICKEYBYTES];
     unsigned char sk[crypto_box_SECRETKEYBYTES];
     
     crypto_box_keypair(pk,sk);
The crypto_box_keypair function randomly generates a secret key and a corresponding public key. It puts the secret key into sk[0], sk[1], ..., sk[crypto_box_SECRETKEYBYTES-1] and puts the public key into pk[0], pk[1], ..., pk[crypto_box_PUBLICKEYBYTES-1]. It then returns 0.|#
  
  (crypto-box-keypair (_fun (pk : (_bytes o crypto-box-PUBLICKEYBYTES))
                            (sk : (_bytes o crypto-box-SECRETKEYBYTES))
                            -> (r : _int) -> (values pk sk r)))
  
  #|C NaCl also provides a crypto_box function callable as follows:

     #include "crypto_box.h"
     
     const unsigned char pk[crypto_box_PUBLICKEYBYTES];
     const unsigned char sk[crypto_box_SECRETKEYBYTES];
     const unsigned char n[crypto_box_NONCEBYTES];
     const unsigned char m[...]; unsigned long long mlen;
     unsigned char c[...];
     
     crypto_box(c,m,mlen,n,pk,sk);

The crypto_box function encrypts and authenticates a message m[0], ..., m[mlen-1] using the sender's secret key sk[0], sk[1], ..., sk[crypto_box_SECRETKEYBYTES-1], the receiver's public key pk[0], pk[1], ..., pk[crypto_box_PUBLICKEYBYTES-1], and a nonce n[0], n[1], ..., n[crypto_box_NONCEBYTES-1]. The crypto_box function puts the ciphertext into c[0], c[1], ..., c[mlen-1]. It then returns 0.|#
  
  (crypto-box (_fun (m n pk sk) ::
                    (ciphertext : (_bytes o message-length))
                    (message : _bytes = (bytes-append (make-bytes crypto-box-ZEROBYTES) m))
                    (message-length : _ullong = (bytes-length message))
                    (n : _bytes) (pk : _bytes) (sk : _bytes)
                    -> (r : _int) -> (values (subbytes ciphertext crypto-box-BOXZEROBYTES) r)))
  
  #|C NaCl also provides a crypto_box_open function callable as follows:

     #include "crypto_box.h"
     
     const unsigned char pk[crypto_box_PUBLICKEYBYTES];
     const unsigned char sk[crypto_box_SECRETKEYBYTES];
     const unsigned char n[crypto_box_NONCEBYTES];
     const unsigned char c[...]; unsigned long long clen;
     unsigned char m[...];
     
     crypto_box_open(m,c,clen,n,pk,sk);

The crypto_box_open function verifies and decrypts a ciphertext c[0], ..., c[clen-1] using the receiver's secret key sk[0], sk[1], ..., sk[crypto_box_SECRETKEYBYTES-1], the sender's public key pk[0], pk[1], ..., pk[crypto_box_PUBLICKEYBYTES-1], and a nonce n[0], ..., n[crypto_box_NONCEBYTES-1]. The crypto_box_open function puts the plaintext into m[0], m[1], ..., m[clen-1]. It then returns 0.|#
  
  (crypto-box-open (_fun (c n pk sk) :: 
                         (message : (_bytes o cipher-length))
                         (ciphertext : _bytes = (bytes-append (make-bytes crypto-box-BOXZEROBYTES) c))
                         (cipher-length : _ullong = (bytes-length ciphertext))
                         (n : _bytes) (pk : _bytes) (sk : _bytes)
                         -> (r : _int) -> (values (subbytes message crypto-box-ZEROBYTES) r)))
  
  #|Applications that send several messages to the same receiver can gain speed by splitting crypto_box into two steps, crypto_box_beforenm and crypto_box_afternm. Similarly, applications that receive several messages from the same sender can gain speed by splitting crypto_box_open into two steps, crypto_box_beforenm and crypto_box_open_afternm.

The crypto_box_beforenm function is callable as follows:

     #include "crypto_box.h"
     
     unsigned char k[crypto_box_BEFORENMBYTES];
     const unsigned char pk[crypto_box_PUBLICKEYBYTES];
     const unsigned char sk[crypto_box_SECRETKEYBYTES];
     
     crypto_box_beforenm(k,pk,sk);|#
  
  (crypto-box-beforenm (_fun (k : (_bytes o crypto-box-BEFORENMBYTES))
                             (pk : _bytes) (sk : _bytes)
                             -> (r : _int) -> (values k r)))
  
  #|The crypto_box_afternm function is callable as follows:
     #include "crypto_box.h"
     
     const unsigned char k[crypto_box_BEFORENMBYTES];
     const unsigned char n[crypto_box_NONCEBYTES];
     const unsigned char m[...]; unsigned long long mlen;
     unsigned char c[...];
     
     crypto_box_afternm(c,m,mlen,n,k);|#
  
  (crypto-box-afternm (_fun (m n k) ::
                            (ciphertext : (_bytes o message-length))
                            (message : _bytes = (bytes-append (make-bytes crypto-box-ZEROBYTES) m)) 
                            (message-length : _ullong = (bytes-length message))
                            (n : _bytes) (k : _bytes)
                            -> (r : _int) -> (values (subbytes ciphertext crypto-box-BOXZEROBYTES) r)))
  
  #|The crypto_box_afternm function is callable as follows:
     #include "crypto_box.h"
     
     const unsigned char k[crypto_box_BEFORENMBYTES];
     const unsigned char n[crypto_box_NONCEBYTES];
     const unsigned char m[...]; unsigned long long mlen;
     unsigned char c[...];
     
     crypto_box_afternm(c,m,mlen,n,k);|#
  
  (crypto-box-open-afternm (_fun (c n k) ::
                                 (message : (_bytes o cipher-length))
                                 (ciphertext : _bytes = (bytes-append (make-bytes crypto-box-BOXZEROBYTES) c))
                                 (cipher-length : _ullong = (bytes-length ciphertext))
                                 (n : _bytes) (k : _bytes)
                                 -> (r : _int) -> (values (subbytes message crypto-box-ZEROBYTES) r))))