;; A modest wrapper around java.net.URI to support URLs as denotations for CREST resources.
;; See http://java.sun.com/j2se/1.5.0/docs/api/java/net/URI.html for the detailed documentation.
;; Michael M. Gorlick 2009.01.24

;; TODO
;;  * Handle java exceptions that may be raised by the constructors, such as URISyntaxException.

;(import s2j)
;(import generic-procedures)
;(import type-system)

(define-java-class <java.net.uri> |java.net.URI|)
(define-java-class <java.net.url> |java.net.URL|)

(define-generic-java-methods
  get-scheme
  get-protocol  ; The URL equivalent of get-scheme.
  get-authority
  get-host
  get-port
  get-path
  get-query
  ; Returns true is the path is absolute /a/b/c/... and false if it is relative a/b/c/...
  is-absolute
  ; Returns an URI whose path is normalized (all . and .. path elements have been removed to the extent possible).
  ; If (is-absolute (normalize u)) returns a <jboolean> true then you are guaranteed that the path does NOT contain . or ..
  normalize
  ; Returns a fully quoted and encoded URI string that does not contain any other characters.
  (to-ascii-string  |toASCIIString|)
  to-string
  (to-url |toURL|)
  equals)

(define :uri-default-port: -1) ; By convention -1 denotes the default port for the given scheme.

;; Various constructors for <java.net.uri>
(define-generic make-uri)

(define-method (make-uri (<java-string> s))
  (java-new <java.net.uri> s))

(define-method (make-uri (<string> s))
  (java-new <java.net.uri> (->jstring s)))

(define-method (make-uri (<string> scheme) (<string> host) (<number> port) (<string> path))
  (java-new
   <java.net.uri>
   (->jstring scheme)
   :java-null-string:   ; user-info
   (->jstring host)
   (->jint port)
   (->jstring path)
   :java-null-string:   ; query.
   :java-null-string:)) ; fragment.

(define-method (make-uri (<string> scheme) (<string> host) (<number> port) (<string> path) (<string> query))
  (java-new
   <java.net.uri>
   (->jstring scheme)
   :java-null-string:   ; user-info
   (->jstring host)
   (->jint port)
   (->jstring path)
   (->jstring query)
   :java-null-string:)) ; fragment.

(define-generics
  :scheme :authority
  :host :path
  :port :query
  normalized?
  absolute? stringify
  uri=? explode)

(define-method (:scheme (<java.net.uri> u))
  (->string (get-scheme u)))

(define-method (:authority (<java.net.uri> u))
  (->string (get-authority u)))

(define-method (:host (<java.net.uri> u))
  (->string (get-host u)))

(define-method (:port (<java.net.uri> u))
  (->number (get-port u)))

(define-method (:path (<java.net.uri> u))
  (->string (get-path u)))

(define-method (:query (<java.net.uri> u))
  (let ((s (get-query u)))
    (and (not (java-null? s)) (->string s))))

;; Returns #t is the URI path is absolute and #f if it is relative.
(define-method (absolute? (<java.net.uri> u))
  (->boolean (is-absolute u)))

(define-method (stringify (<java.net.uri> u))
  (->string (to-ascii-string u)))

(define-method (uri=? (<java.net.uri> x) (<java.net.uri> y))
  (->boolean (equals x y)))

(define-method (explode (<java.net.uri> u))
  (list
   (:scheme u)
   (:host u)
   (:port u)
   (:path u)
   (:query u)))

;; A few notes about HTTP URL parameter syntax.
;; General form is /path_1/path_2/ ... /path_n;p_1;p_2; ... ;p_m
;; where each p is a parameter of the form n=v, a name/value pair.
;; A value v in a name/value pair may consist of multiple subvalues separated by commas,
;; for example n=a,b,c
