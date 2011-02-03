#lang racket

(require xml
         "../util/xpath.rkt"
         "../../../bindings/curl/libcurl/libcurl.rkt")
(provide (all-defined-out))

;; Remove all instances of #"\r", #"\n" and #"\t" plus extra whitespace
; in a bytestring
(define (strip-crlf bytes)
  (regexp-replace* #"[\r|\n|\t] *" bytes #""))

;; Return an xexpr from a bytestring XML literal
;; (if any \r or \n in bytestring, they need to be stripped first)
(define (bytes->xexpr bytes)
  (xml->xexpr (document-element (read-xml (open-input-bytes bytes)))))

;; convert the bytestring inside an output port to an xexpr,
;; optionaly stripping CRLF if specified
(define (output-port->xexpr o [strip? #t])
  (if strip?
      (bytes->xexpr (strip-crlf (get-output-bytes o)))
      (bytes->xexpr (get-output-bytes o))))

;; given an ID for a procedure and an output port to write to,
;; define a procedure that acts as a callback for 
;; CURLOPT_WRITEFUNCTION and similar libcurl options
(define-syntax-rule (define-write-to-outport id port)
  (define (id bytes size nmemb *data)
    (write-bytes bytes port 0 (* size nmemb))
    (* nmemb size)))

;; attach all headers to the curl handle
(define (attach-headers curl kv . kvs)
  (let ([header-list (if (empty? kvs)
                         (curl-slist-new kv)
                         (apply curl-slist-new (cons kv kvs)))])
    (curl-easy-setopt curl CURLOPT_HTTPHEADER header-list)))

;; urlencode a '((key . value) (key1 . value1)) data set
(define (make-wwwform-post-data data)
  (apply string-append (add-between 
                        (map (λ (pair) (format "~a=~a" (car pair) (cdr pair))) 
                             data) 
                        "&")))

;; given an URL-encoded form, attach it to the given curl handle
(define (attach-wwwform-post-data curl x/www-form-urlencoded-string)
  (curl-easy-setopt curl CURLOPT_POST 1)
  (curl-easy-setopt curl CURLOPT_POSTFIELDS x/www-form-urlencoded-string))

;; set the curl handle to post mode with the given data
;; and then do the POST
;; provide a '((key . value) (data . set)) to urlencode
(define (POST-wwwform sc url data)
  (attach-wwwform-post-data (splunk-client-curl-handle sc) (make-wwwform-post-data data))
  (perform sc url))

;; do an HTTP GET or POST on the given url (depending on state of the curl handle)
;; using the credentials of the provided splunk client
(define (perform sc url)
  (define curl (splunk-client-curl-handle sc))
  (define header-port (open-output-bytes))
  (define body-port (open-output-bytes))
  (define-write-to-outport write-header header-port)
  (define-write-to-outport write-body body-port)
  (curl-easy-setopt-writefunction curl CURLOPT_HEADERFUNCTION write-header)
  (curl-easy-setopt-writefunction curl CURLOPT_WRITEFUNCTION write-body)
  (curl-easy-setopt curl CURLOPT_URL url)
  (let ([res (curl-easy-perform curl)])
    (values res header-port body-port)))

(define GET perform)

(struct splunk-client (curl-handle base-url [session-key #:mutable]))

(define (attach-auth-header sc)
  (attach-headers (splunk-client-curl-handle sc) 
                  (format "Authorization: Splunk ~a" (splunk-client-session-key sc))))

(define (reset-client/except-auth sc)
  (curl-easy-reset (splunk-client-curl-handle sc))
  (attach-auth-header sc))

(define (new-handle)
  (curl-easy-init))

(define (clean-handle sc) 
  (curl-easy-cleanup (splunk-client-curl-handle sc)))