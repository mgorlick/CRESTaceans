#! /usr/bin/env racket
#lang racket

(require xml)
(require "../net/base64-url.rkt"
         "../../../bindings/curl/libcurl/libcurl.rkt")

(define login-url-path "/auth/login")

(struct splunk-client (curl service-base session-key))

(define-syntax-rule (define-write-to-outport id port)
  (define (id bytes size nmemb *data)
    (write-bytes bytes port 0 (* size nmemb))
    (* nmemb size)))

(define (POST curl url x/www-form-urlencoded-string bodyport headerport)
  (define-write-to-outport write-body bodyport)
  (define-write-to-outport write-header headerport)
  (curl-easy-setopt-writefunction curl CURLOPT_WRITEFUNCTION write-body)
  (curl-easy-setopt-writefunction curl CURLOPT_HEADERFUNCTION write-header)
  (curl-easy-setopt curl CURLOPT_URL url)
  (curl-easy-setopt curl CURLOPT_POST 1)
  (curl-easy-setopt curl CURLOPT_POSTFIELDS x/www-form-urlencoded-string)
  (let ([res (curl-easy-perform curl)])
    res))

(define (login curl service-base username password)
  (define body-port (open-output-bytes))
  (define header-port (open-output-bytes))
  (POST curl (string-append service-base login-url-path)
        (format "username=~a&password=~a" username password)
        body-port header-port)
  
  ;; chomp through the body looking for a session key
  ;; expected form: (response () (sessionKey () "1d67d05afb0e43b9769d485b95c7c28f"))
  (let* ([body (get-output-bytes body-port)]
         [bytes (regexp-replace* #"\n" (regexp-replace* #"\r" body "") "")]
         [response (xml->xexpr (document-element (read-xml (open-input-bytes bytes))))])
    (match response
      [(list 'response '() (list 'sessionKey '() a)) a]
      [_ (error (format "Could not log in to splunk service at ~a" 
                (string-append service-base login-url-path)))])))

(define (make-splunk-client service-base username password)
  (define curl (curl-easy-init))
  (splunk-client
   curl
   service-base
   (login curl service-base username password)))

(make-splunk-client "http://ubuntu:8089/services" "admin" "morefuntocompute")
