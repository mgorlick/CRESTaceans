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

;; Wrapper around the Apache HttpComponents StringEntity, ByteArrayEntity, BufferingNHttpEntity, and EntityUtils.

(define-java-class <java-http-entity>      |org.apache.http.HttpEntity|)
(define-java-class <java-string-entity>    |org.apache.http.entity.StringEntity|)
(define-java-class <java-byte-entity>      |org.apache.http.entity.ByteArrayEntity|)
(define-java-class <java-buffering-entity> |org.apache.http.nio.entity.BufferingNHttpEntity|)
(define-java-class <java-entity-utility>   |org.apache.http.util.EntityUtils|)

(define :null-java-entity-utility:)
(define-generic-java-method to-string)
(define-generic-java-method to-byte-array)
(define-generic-java-method get-content-length)
(define :utf-8: (->jstring "UTF-8"))
(define-java-class <java-string> |java.lang.String|)

(define (java-string? s)
  (type<= (type-of s) <java-string>))

(define (http/entity? x)
  (instance-of? x <java-http-entity>))
  
(define (http/entity/length e)
  (->number (get-content-length e)))

;; Create a new HTTPEntity whose contents is x.
(define (http/entity/new x)
  (cond
   ((string? x)      (java-new <java-string-entity> (->jstring x)))
   ((buffer? x)      (java-new <java-byte-entity>   (buffer/java-byte-array x))) ; Used to transport serializations.
   ((java-string? x) (java-new <java-string-entity> x))
   ((java-array? x)  (java-new <java-byte-entity> x)) ; Assume x is byte[].
   (else #f)))

;; Deconstruct a StringEntity returning its contents as a Scheme string.
(define (http/entity/string e)
  (->string (to-string :null-java-entity-utility: e :utf-8:)))

(define (http/entity/string/force e)
  (->string (java-new <java-string>
                      (to-byte-array :null-java-entity-utility: e)
                      :utf-8:)))

;; Deconstruct a StringEntity returnings its contents as a Java String.
(define (http/entity/java-string e)
  (to-string :null-java-entity-utility: e :utf-8:))

;; Deconstruct a ByteArrayEntity returning its contents as a Java byte[]
(define (http/entity/java-byte-array e)
  (to-byte-array :null-java-entity-utility: e))

;; Deconstruct a ByteArrayEntity returning its contents as a Scheme Buffer.
(define (http/entity/buffer e)
  (java-byte-array/buffer (to-byte-array :null-java-entity-utility: e)))

(set! :null-java-entity-utility: (java-null <java-entity-utility>))

