;; Copyright 2009 Michael M. Gorlick

;; A wrapper around the Apache HTTP Components URIPatternMatcher.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Method declarations.
(define matcher/register   (make-generic-method))
(define matcher/unregister (make-generic-method))
(define matcher/match      (make-generic-method))

;; Object definition.
(define <sham/matcher>
  (let ()
    ; Hide the Java from view.
    (define-java-class <java-uri-matcher> |org.apache.http.protocol.UriPatternMatcher|)
    (define-generic-java-method register)   ; void register(String, Object)
    (define-generic-java-method unregister) ; void unregister(String)
    (define-generic-java-method lookup)     ; Object lookup(String)

    ; Object class.
    (lambda ()
      (let ((self (make-object))
	    (java-matcher (java-new <java-uri-matcher>)))

	(make-method!
	 self instantiate
	 (lambda (self) #t))

        ; pattern: any Scheme string of the form "*<uri>", "<uri>*" or "*"
	; object: any Scheme object
	(make-method!
	 self matcher/register
	 (lambda (self pattern object)
	   (register java-matcher (->jstring pattern) (java-wrap object))
	   self))

        ; pattern: any Scheme string of the form "*<uri>", "<uri>*" or "*"
	(make-method!
	 self matcher/unregister
	 (lambda (self pattern)
	   (unregister java-matcher (->jstring pattern))
	   self))

        ; uri: a String or Scheme string (the URI portion of an HTTP request)
	(make-method!
	 self matcher/match
	 (lambda (self uri) ; uri is either a Java String or a Scheme string.
	   (let ((object (lookup java-matcher (if (java-object? uri) uri (->jstring uri)))))
	     (if (java-null? object) #f (java-unwrap object)))))

	self))))



