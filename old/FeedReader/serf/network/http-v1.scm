;; Bridging interfaces to the HTTP request/response elements of the Apache HTTPComponents core
;; library..
;; Copyright 2009 Michael M. Gorlick

;  (import s2j)
;  (import generic-procedures)
;  (import* type-system <boolean>)
;  (import serf/utility/base)
;  (import* serf/log always-string)

; Predefined Java strings for various HTTP/CREST methods.
(define :http-get-method:)    ; HTTP/1.1
(define :http-put-method:)    ; HTTP/1.1
(define :http-post-method:)   ; HTTP/1.1
(define :http-spawn-method:)  ; CREST
(define :http-remote-method:) ; CREST
(define :http-relay-method:)  ; CREST

;; HTTP headers are just name/value pairs.
(define <java-http-header>)
(define get-name)  ; String getName()
(define get-value) ; String getValue()

;; Create an BasicHeader with name n and value v.
(define (http/header/new n v)
  (let ((name  (always-string n))
	(value (always-string v)))
  (java-new <java-http-header> (->jstring name) (->jstring value))))

;; Deconstruct a BasicHeader as a dotted pair.
(define (http/header/unwrap h)
  (cons (->string (get-name h)) (->string (get-value h))))

;; HTTP request without an entity body.
(define <java-basic-http-request>)
(define-generic http/request/new)
(define :null-http-request:)
(define add-header)       ; void addHeader(String name, String value)
(define contains-header)  ; boolean containsHeader(String name)
(define get-all-headers)  ; Header[] getAllHeaders()
(define get-first-header) ; Header getFirstHeader(String name)
(define get-request-line) ; String getRequestLine()
(define remove-headers)   ; void removeHeaders(String name)
(define set-header)       ; void setHeader(String name, String value)

;; HTTP request with an entity body.
(define <java-basic-http-entity-request>)
(define set-entity) ; void setEntity(HttpEntity)
(define get-entity) ; HttpEntity getEntity()

(define <java-http-entity>)
(define get-content-encoding) ; Header getContentEncoding()
(define get-content-length)   ; long getContentLength()
(define get-content-type)     ; Header getContentType()
(define get-content)          ; InputStream getContent()
(define is-chunked)           ; Boolean isChunked()
(define write-to)             ; void writeTo(OutputStream)

;; HTTP request/response entity (implements <java-http-entity>).
(define <java-string-entity>)

;; Helper class for extracting content from an HttpEntity.
(define <java-http-entity-utility>)
(define to-byte-array)
;(define to-string)
(define :http-entity-utility:)
(define (http/entity/string entity)
  (->string (to-string :http-entity-utility: entity)))
(define (http/entity/vector entity)
  (->vector (to-byte-array :http-entity-utility: entity)))

;; HTTP versions (done with the usual Java overkill).
(define <java-http-version>)
(define :http/1.1:)

;; HTTP status line object comprising [version code reason].
(define <java-http-status-line>)
; Construct a new HTTP/1.1 status line with code (Java int) and reason (Java String).
(define (http/status-line/new code reason)
  (java-new <java-http-status-line> :http/1.1: code reason))
(define get-status-code)   ; int getStatusCode()
(define get-reason-phrase) ; String getReasonPhrase()

;; Predefined HTTP/1.1 status codes and reasons (woefully incomplete for now).
(define :http-ok-code           (->jint    200))
(define :http-ok-reason:        (->jstring "OK"))
(define :http-not-found-code:   (->jint 404))
(define :http-not-found-reason: (->jstring "Not Found"))

;; HTTP response.
(define <java-http-response>)
;(define get-entity)        ; HttpEntity getEntity()
(define get-status-line)   ; StatusLine getStatusLine()
;(define get-all-headers)   ; Header [] getAllHeaders()
;(define get-first-header)  ; Header getFirstHeader(String)
;(define set-entity)        ; void setEntity(HttpEntity)
(define set-reason-phrase) ; void setReasonPhrase(String)
(define set-status-code)   ; void setStatusCode(int)

;; Expressions.

(set! :http-get-method:    (->jstring "GET"))    ; HTTP/1.1
(set! :http-put-method:    (->jstring "PUT"))    ; HTTP/1.1
(set! :http-post-method:   (->jstring "POST"))   ; HTTP/1.1
(set! :http-spawn-method:  (->jstring "SPAWN"))  ; CREST
(set! :http-remote-method: (->jstring "REMOTE")) ; CREST
(set! :http-relay-method:  (->jstring "RELAY"))  ; CREST

(set-java-class <java-http-header> |org.apache.http.message.BasicHeader|)
(set-generic-java-method get-name)  ; String getName()
(set-generic-java-method get-value) ; String getValue()

(set-java-class <java-basic-http-request> |org.apache.http.message.BasicHttpRequest|)
(set! :null-http-request: (java-null <java-basic-http-request>))
(set-generic-java-method add-header)       ; void addHeader(String name, String value)
(set-generic-java-method contains-header)  ; boolean containsHeader(String name)
(set-generic-java-method get-all-headers)  ; Header[] getAllHeaders()
(set-generic-java-method get-first-header) ; Header getFirstHeader(String name)
(set-generic-java-method get-request-line) ; String getRequestLine()
(set-generic-java-method remove-headers)   ; void removeHeaders(String name)
(set-generic-java-method set-header)       ; void setHeader(String name, String value)

;; m: Java String of method (use one of the :http-xxx-method: constants)
;; uri: Java String of URI in request line
;; entity: #t if request will contain an entity body and #f otherwise
(add-methods
 http/request/new
 (list
  (method
   ((<java-string> m) (<java-string> uri) (<boolean> entity))
   (java-new 
    (if entity <java-basic-http-entity-request> <java-basic-http-request>)
    m uri))

  (method
   ((<java-string> m) (<java-string> uri))
   (java-new <java-basic-http-request> m uri))))

(set-java-class <java-http-entity> |org.apache.http.HttpEntity|)
(set-generic-java-method get-content-encoding) ; Header getContentEncoding()
(set-generic-java-method get-content-length)   ; long getContentLength()
(set-generic-java-method get-content-type)     ; Header getContentType()
(set-generic-java-method get-content)          ; InputStream getContent()
(set-generic-java-method is-chunked)           ; Boolean isChunked()
(set-generic-java-method write-to)             ; void writeTo(OutputStream)

(set-java-class <java-string-entity> |org.apache.http.nio.entity.NStringEntity|)

(set-java-class <java-basic-http-entity-request> |org.apache.http.message.BasicHttpEntityEnclosingRequest|)
(set-generic-java-method get-entity) ; HttpEntity getEntity()
(set-generic-java-method set-entity) ; setEntity(HttpEntity)

(set-java-class <java-http-entity-utility> |org.apache.http.util.EntityUtils|)
(set-generic-java-method to-byte-array) ; byte[] toByteArray(HttpEntity)
;(set-generic-java-method to-string)     ; String toString(HttpEntity)
(set! :http-entity-utility: (java-null <java-http-entity-utility>))

(set-java-class <java-http-version> |org.apache.http.HttpVersion|)
(set! :http/1.1: (java-new <java-http-version> (->jint 1) (->jint 1))) ; HttpVersion object for HTTP/1.1

(set-java-class <java-http-status-line> |org.apache.http.message.BasicStatusLine|)
(set-generic-java-method get-status-code)   ; int getStatusCode()
(set-generic-java-method get-reason-phrase) ; String getReasonPhrase()

(set-java-class <java-http-response> |org.apache.http.message.BasicHttpResponse|)
;(set-generic-java-method get-entity)        ; HttpEntity getEntity()
(set-generic-java-method get-status-line)   ; StatusLine getStatusLine()
;(set-generic-java-method get-all-headers)   ; Header [] getAllHeaders()
;(set-generic-java-method get-first-header)  ; Header getFirstHeader(String)
;(set-generic-java-method set-entity)        ; void setEntity(HttpEntity)
(set-generic-java-method set-reason-phrase) ; void setReasonPhrase(String)
(set-generic-java-method set-status-code)   ; void setStatusCode(int)
