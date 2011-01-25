#lang racket

(require "../libcurl/libcurl.rkt")

(define curl (curl-easy-init))
(curl-easy-setopt curl CURLOPT_URL "http://www.google.com")
(curl-easy-perform curl)
(curl-easy-getinfo-string curl Effective-URL)
(curl-easy-getinfo-string curl Content-Type)
(curl-easy-cleanup curl)