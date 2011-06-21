;; A wrapper around java.net.URI for URIs as denotations for CREST resources.
;; See http://java.sun.com/j2se/1.5.0/docs/api/java/net/URI.html for the detailed documentation.

;; Copyright 2009 Michael M. Gorlick

(define-java-class <java.net.uri> |java.net.URI|)
(define :uri-default-port: -1) ; By Java convention -1 denotes the default port for the given scheme.
(define-generic-java-method absolute?)
(define-generic-java-method get-authority)
(define-generic-java-method get-host)
(define-generic-java-method get-path)
(define-generic-java-method get-port)
(define-generic-java-method get-query)
(define-generic-java-method get-scheme)
(define-generic-java-method to-ascii-string |toASCIIString|)

;; Returns the URI scheme as a Scheme string.
(define (uri/scheme u)
  (->string (get-scheme u)))

;; Returns the URI authority (host:port) as a Scheme string.
(define (uri/authority u)
  (->string (get-authority u)))

;; Returns the URI host as a Scheme string. 
(define (uri/host u)
  (->string (get-host u)))

;; Returns the URI port as a Scheme integer.
(define (uri/port u)
  (->number (get-port u)))

;; Returns the URI path as a Scheme string.
(define (uri/path u)
  (->string (get-path u)))

;; If a query portion is present in the URI returns it as a Scheme string otherwise #f.
(define (uri/query u)
  (let ((s (get-query u)))
    (and (not (java-null? s)) (->string s))))

;; Returns #t is the URI path is absolute and #f if it is relative.
(define (uri/absolute? u)
  (->boolean (absolute? u)))

;; Returns a fully quoted and encoded URI string that does not contain any "other" characters.
(define (uri/ascii u)
  (if u
      (->string (to-ascii-string u))
      ""))

;; Explodes the URI as a list (scheme host port path query) where everything is a Scheme string
;; excepting port, a Scheme integer, and query which may be #f if the URI is query-free.
(define (uri/explode u)
  (list
   (uri/scheme u)
   (uri/host u)
   (uri/port u)
   (uri/path u)
   (uri/query u)))

(define (uri/path+query u)
  (let ((p (uri/path u))
	(q (uri/query u)))
    (if q
	(string-append p "&" q)
	p)))

(define (uri/uri? x)
  (and (java-object? x) (instance-of? x <java.net.uri>)))

(define (uri/new . arguments)
  (match
   arguments
   ((,s)
    (cond
     ((string? s)
      (java-new <java.net.uri> (->jstring s)))
     ((java-object? s) ; Assume String.
      (java-new <java.net.uri> s))
     (else #f)))
      
   ((,scheme ,host ,port ,path)
    (java-new
     <java.net.uri>
     (->jstring scheme)
     :java-null-string:   ; user-info
     (->jstring host)
     (->jint port)
     (->jstring path)
     :java-null-string:   ; query.
     :java-null-string:)) ; fragment.

   ((,scheme ,host ,port ,path ,query)
    (java-new
     <java.net.uri>
     (->jstring scheme)
     :java-null-string:   ; user-info
     (->jstring host)
     (->jint port)
     (->jstring path)
     (->jstring query)
     :java-null-string:)))) ; fragment.
    
;; A few notes about HTTP URL parameter syntax.
;; General form is /path_1/path_2/ ... /path_n;p_1;p_2; ... ;p_m
;; where each p is a parameter of the form n=v, a name/value pair.
;; A value v in a name/value pair may consist of multiple subvalues separated by commas,
;; for example n=a,b,c
