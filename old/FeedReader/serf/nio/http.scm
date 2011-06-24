(import s2j)
;; Note: always-string comes from logger.scm.

(require-library 'sisc/libs/srfi/srfi-13)
(import srfi-13) ; String library.

; Predefined Java strings for various HTTP/CREST methods.
(define :http-get-method:    (->jstring "GET"))
(define :http-put-method:    (->jstring "PUT"))
(define :http-post-method:   (->jstring "POST"))
(define :http-spawn-method:  (->jstring "SPAWN"))  ; CREST
(define :http-remote-method: (->jstring "REMOTE")) ; CREST
(define :http-relay-method:  (->jstring "RELAY"))  ; CREST

;; HTTP header.
(define-java-class <java-http-header> |org.apache.http.message.BasicHeader|)
(define-generic-java-methods
  get-name get-value) ; Both methods are String ().

(define (make-http-header n v)
  (let ((name (always-string n))
	(value (always-string v)))
  (java-new <java-http-header> (->jstring name) (->jstring value))))

(define (unmake-http-header h)
  (cons (->string (get-name h)) (->string (get-value h))))

;; Constructors: (String source) or (String source, String character-set).
(define-java-class <java-string-entity> |org.apache.http.nio.entity.NStringEntity|)
(define-generic-java-methods
  get-content-length ; long ()
  get-content        ; InputStream ()
  write-to)          ; void (OutputStream)

;; HTTP request without an entity body.
;; Constructors: (String method, String uri)
(define-java-class <java-basic-http-request> |org.apache.http.message.BasicHttpRequest|)
(define-generic-java-methods
  add-header       ; void (String name, String value)
  contains-header  ; boolean (String name)
  get-all-headers  ; Header[] ()
  get-first-header ; Header (String name)
  get-request-line ; String ()
  remove-headers   ; void (String name)
  set-header)      ; void (String name, String value)
(define :null-http-request: (java-null <java-basic-http-request>))

(define (make-basic-http-request method uri)
  (java-new <java-basic-http-request> (->jstring method) (->jstring uri)))

;; Returns method of request r as a Scheme string.
(define (http-method-of r)
  (let* ((line (->string (get-request-line r)))
	 (tokens (string-tokenize line)))
    (and (= (length tokens) 3) (car tokens))))

;; HTTP request with an entity body.
;; Constructors: (String method, String uri)
(define-java-class <java-basic-http-entity-request> |org.apache.http.message.BasicHttpEntityEnclosingRequest|)
(define-generic-java-methods
  set-entity  ; Sets the entity of the request.
  get-entity) ; Returns an HTTP entity.

(define-java-class <java-http-version> |org.apache.http.HttpVersion|)
(define :http/1.1: (java-new <java-http-version> (->jint 1) (->jint 1))) ; HttpVersion object for HTTP/1.1

(define-java-class <java-basic-status-line> |org.apache.http.message.BasicStatusLine|)
;; code: java int giving HTTP response code
;; reason: java String giving HTTP reason phrase
(define (make-http-status-line code reason)
  (java-new <java-basic-status-line> :http/1.1: code reason))

(define :http-ok-reason: (->jstring "OK"))
(define :http-ok-code    (->jint    200))
(define :http-not-found-reason: (->jstring "Not Found"))
(define :http-not-found-code:   (->jint 404))


(define-java-class <java-http-status-line> |org.apache.http.message.BasicStatusLine|)
(define-generic-java-methods
  get-status-code     ; int ()
  get-reason-phrase)  ; String ()

(define-java-class <java-http-response> |org.apache.http.message.BasicHttpResponse|)
(define-generic-java-methods
  get-entity
  get-status-line   ; StatusLine ()
  get-all-headers   ; Header []()
  get-first-header  ; Header (String)
  set-entity        ; void (HttpEntity)
  set-reason-phrase ; void (String)
  set-status-code)   ; void (int)

(define-java-class <java-http-entity> |org.apache.http.HttpEntity|)
(define-generic-java-methods
  get-content-encoding
  get-content-length
  get-content-type
  is-chunked)

(define-java-class <java-http-entity-utility> |org.apache.http.util.EntityUtils|)
(define-generic-java-methods
  to-byte-array
  to-string)
(define :http-entity-utility: (java-null <java-http-entity-utility>))
(define (http-entity-to-string entity)
  (->string (to-string :http-entity-utility: entity)))
(define (http-entity-to-vector entity)
  (->vector (to-byte-array :http-entity-utility: entity)))
