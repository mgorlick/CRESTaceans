#lang racket

(require "../libnacl/libnacl.rkt")

(let-values ([(pk sk r) (crypto-box-keypair)])
  (values pk sk))