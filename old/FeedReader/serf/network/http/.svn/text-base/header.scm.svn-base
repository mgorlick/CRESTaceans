;; Copyright 2009 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Wrapper around the Apache HTTP components HttpHeader.

(define-java-class <java-http-header>         |org.apache.http.message.BasicHeader|)
(define-java-class <java-header-value-parser> |org.apache.http.message.BasicHeaderValueParser|)
(define-java-class <java-header-element>      |org.apache.http.message.BasicHeaderElement|)

(define :null-java-header-value-parser:)
(define-generic-java-method get-name)  ; String getName()
(define-generic-java-method get-value) ; String getValue()
(define-generic-java-method parse-elements) ; HeaderElement[] parseElements(String, HeaderValueParser) for HeaderValueParser
(define-generic-java-method get-parameters) ; NameValuePair[] getParameters() for BasicHeaderElement

;; Create an BasicHeader with name n and value v.
(define (http/header/new . arguments)
  (let ((n (length arguments)))
    (cond
     ((> n 1)
      ; (http/header/new name value)
      (let ((name  (car arguments))
	    (value (cadr arguments)))
	(java-new <java-http-header> (->jstring name) (->jstring value))))
     ((= n 1)
      ; (http/header.new (name . value))
      (let ((header (car arguments)))
	(java-new <java-http-header> (->jstring (car header)) (->jstring (cdr header)))))
     (else #f))))

 ;; Deconstruct a BasicHeader as a dotted pair.
(define (http/header/unwrap h)
  (cons (->string (get-name h)) (->string (get-value h))))

;; Parse the value portion of a header.
(define (http/header/parse v)
  (parse-elements :null-java-header-value-parser: (->jstring v) :null-java-header-value-parser:))

;; Parse the value of the Content-Type header.
;; media-type     = type "/" subtype *( ";" parameter )
;; type           = token
;; subtype        = token
;; Returns (media ((p_0 . v_0) (p_1 . v_1) ...)), the media-type and an association list of parameters.
(define (http/header/content/type/parse v)
  (let* ((elements (http/header/parse v))
	 (element  (and (> (java-array-length elements) 0) (java-array-ref elements 0)))
	 (media    (and element   (get-name element)))
	 (parameters (and element (get-parameters element))))
    (list
     (->string media)
     (map
      (lambda (pair) (cons (->string (get-name pair)) (->string (get-value pair))))
      (->list parameters)))))
    
(define :http/content/csv:               "text/csv") ; Comma-Separated Values.
(define :http/content/form/urlencoded:   "application/x-www-form-urlencoded")
(define :http/content/html:              "text/html")
(define :http/content/json:              "application/json ; charset=UTF-8")
(define :http/content/serf/binary:       "application/x-crest-serf ; encoding=SISC/1.16.6")
(define :http/content/serf/base64:       "application/x-crest-serf ; encoding=base64")
(define :http/content/text:              "text/plain ; charset=UTF-8")
(define :http/content/xml:               "text/xml")

(define :mime/csv:               "text/csv") ; Comma-Separated Values.
(define :mime/form/urlencoded:   "application/x-www-form-urlencoded")
(define :mime/html:              "text/html")
(define :mime/json:              "application/json")
(define :mime/serf/binary:       "application/x-crest-serf")
(define :mime/serf/base64:       "application/x-crest-serf")
(define :mime/text:              "text/plain")
(define :mime/xml:               "text/xml")

(set! :null-java-header-value-parser: (java-null <java-header-value-parser>))