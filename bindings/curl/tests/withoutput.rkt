#! /usr/bin/env racket
#lang racket

(require "../libcurl/libcurl.rkt"
         ffi/unsafe)

(define curl (curl-easy-init))

(define-syntax-rule (define-write-to-outport id port)
  (define (id bytes size nmemb *data)
    (printf "data size: ~a*~a~n" size nmemb)
    (write-bytes bytes port 0 (* size nmemb))
    (* nmemb size)))

(define (GET url body headers)
  (define-write-to-outport write-body body)
  (define-write-to-outport write-header headers)
  (curl-easy-setopt curl CURLOPT_URL url)
  (curl-easy-setopt-writefunction curl CURLOPT_WRITEFUNCTION write-body)
  (curl-easy-setopt-writefunction curl CURLOPT_HEADERFUNCTION write-header)
  
  (curl-easy-perform curl)
  (curl-easy-getinfo-string curl Effective-URL)
  (curl-easy-getinfo-string curl Content-Type))

(define (my-program)
  (define body (open-output-bytes))
  (define headers (open-output-bytes))
  (GET "http://www.google.com" body headers)
  ;;; ... process data from the output ports ...
  (printf "Headers: ~a~n" (get-output-bytes headers))
  (printf "Body: ~a~n" (get-output-bytes body)))

(my-program)
(curl-easy-cleanup curl)