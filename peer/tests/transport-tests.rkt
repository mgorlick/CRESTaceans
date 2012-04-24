#lang racket/base
(require racket/unit
         "../src/net/encryption.rkt"
         "../src/net/compression.rkt"
         "../../Motile/compile/serialize.rkt"
         "../../Motile/tests/compile-test.rkt"
         "../../Motile/compile/compile.rkt"
         "../../Motile/baseline.rkt"
         "../../Motile/generate/baseline.rkt")
(define-values/invoke-unit nacl-encryption@
  (import)
  (export encryption-unit^))

(define (Motile-tests)
  (define (writable->bytes t)
  (define o (open-output-bytes))
  (write t o)
  (get-output-bytes o))
  (define-values (pk-A crypt-factory-A) (make-pk/encrypter/decrypter))
  (define-values (pk-B crypt-factory-B) (make-pk/encrypter/decrypter))
  (define-values (encrypt-A decrypt-A) (crypt-factory-A pk-B))
  (define-values (encrypt-B decrypt-B) (crypt-factory-B pk-A))
  
  (define compile/serialize/start
    (lambda (e) (motile/call (motile/deserialize (motile/serialize (motile/compile e)) #f)
                             ENVIRON/TEST)))
  
  (define (compile/encrypt/serialize/start e)
    (define m (motile/serialize (motile/compile e)))
    (define-values (cipher nonce) (encrypt-A (writable->bytes m)))
    (define decrypted-expr (decrypt-B cipher nonce))
    (motile/call (motile/deserialize (read (open-input-bytes decrypted-expr)) #f) 
                 ENVIRON/TEST))
  
  (define (compile/compress/encrypt/serialize/start e)
    (define m (motile/serialize (motile/compile e)))
    (define-values (cipher nonce) (encrypt-A (writable->bytes (compress m))))
    (define decrypted-expr (decrypt-B cipher nonce))
    (motile/call (motile/deserialize (decompress (read (open-input-bytes decrypted-expr))) #f)
                 ENVIRON/TEST))
  
  (parameterize ([compile/start compile/serialize/start])
    (compile/tests/all))
  (parameterize ([compile/start compile/encrypt/serialize/start])
    (compile/tests/all))
  (parameterize ([compile/start compile/compress/encrypt/serialize/start])
    (compile/tests/all)))