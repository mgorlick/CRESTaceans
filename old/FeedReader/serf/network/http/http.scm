;; Copyright 2009 Michael M. Gorlick

(define-java-class <java-http-header> |org.apache.http.message.BasicHeader|)
(define-generic-java-method get-name)  ; String getName()
(define-generic-java-method get-value) ; String getValue()

(define-java-class <java-basic-http-request> |org.apache.http.message.BasicHttpRequest|)
(define :null-http-request:)
(define-java-class <java-basic-http-entity-request> |org.apache.http.message.BasicHttpEntityEnclosingRequest|)

(define-java-class <java-string-entity>    |org.apache.http.entity.StringEntity|)
(define-java-class <java-entity-utility>   |org.apache.http.util.EntityUtils|)
(define-generic-java-method to-string) ; String toString(HttpEntity, String) for EntityUtils
(define :java-null-entity-utility:)

(define :utf-8: (->jstring "UTF-8"))

;; Create a StringEntity.
(define (http/entity/new s)
  (cond
   ((string? s)
    (java-new <java-string-entity> (->jstring s) :utf-8:))
   (else #f))) ; Will add byte arrays later.

;; Deconstruct a StringEntity returning its contents as a Scheme string.
(define (http/entity/string e)
  (->string (to-string :java-null-entity-utility: e :utf-8:)))

;; Create an BasicHeader with name n and value v.
(define (http/header/new . arguments)
  (if (> (length arguments) 1)
      (let ((name  (car arguments))
	    (value (cadr arguments)))
	(java-new <java-http-header> (->jstring name) (->jstring value)))
      (let ((header (car arguments)))
	(java-new <java-http-header> (->jstring (car header)) (->jstring (cdr header))))))

 ;; Deconstruct a BasicHeader as a dotted pair.
(define (http/header/unwrap h)
  (cons (->string (get-name h)) (->string (get-value h))))

(set! :null-http-request: (java-null <java-basic-http-request>))
(set! :java-null-entity-utility: (java-null <java-entity-utility>))
