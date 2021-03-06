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

;; A Scheme interface to the JSON Simple library (version 1.1)
;; at http://code.google.com/p/json-simple/

(define-java-class <java-json-parser> |jsonParserTranslator|)
(define :null-java-json-parser:)
(define-generic-java-method parse)
(define-generic-java-method flavor) ; static int flavor(...) in <java-json-parser>
(define-generic-java-method size) ; int size() for JSONArray
(define-generic-java-method get)  ; Object get(int i) for JSONArray
(define-generic-java-method map-iterator) ; Returns java.util.Map.Entry
(define-generic-java-method next)       ; Map.Entry next(Iterator) in <java-json-parser>
(define-generic-java-method get-key)    ; Object getKey() for Map.Entry
(define-generic-java-method get-value) ; Object getValue() for Map.Entry

;; Mappings for JSON values
;; JSON         Scheme
;; string       string
;; integer      integer
;; float        double
;; double       double
;; true         #t
;; false        #f
;; null         ()
;; vector       list
;; collection   hashtable

;; Create and return an instance of the JSON parser.
(define (json/parser/new)
  (java-new <java-json-parser> (->jint 0)))

;; Given a JSON entity return the translator for transforming it into a Scheme object.
(define (json/translator entity)
  (if (java-null? entity)
      (lambda (x) '()) ; The translator for the JSON null.
      (vector-ref :translation-vector: (->number (flavor :null-java-json-parser: entity)))))

;; Translate JSONArray instance into a Scheme vector.
(define  (json/translate/array a)
  (let* ((n (->number (size a)))
	 (v (make-vector n)))
    (let loop ((i (- n 1)))
      (cond
       ((>= i 0)
	(let* ((item (get a (->jint i))) ; Get the i'th Java item.
	       (translator (json/translator item))) ; Find the translator for the item type.
	  (vector-set! v i (translator item)) ; v[i] = Scheme translation of item.
	  (loop (- i 1))))
       (else v)))))

;; Given a JSONObject m return an iterator for it.
(define (json/iterator/new m)
  (map-iterator :null-java-json-parser: m))

;; Returns the next name/value pair in the JSONObject or <java-null> if the iterator is exhausted.
(define (json/iterator/next i)
  (next :null-java-json-parser: i))

;; Given a JSONObject m translate it into an association list where the keys are symbols.
(define (json/translate/map m)
  (let ((iterator (json/iterator/new m)))
    (let loop ((item (json/iterator/next iterator))
	       (associations '()))
      (cond
       ((java-null? item) associations)
       (else
	(loop (json/iterator/next iterator)
	      (cons
	       (cons (json/translate/any (get-key item)) (json/translate/any (get-value item))) ; (name . value)
	       associations)))))))
;; For any JSON entity x generated by the JSON simple parser translate it into the corresponding
;; Scheme value.
(define (json/translate/any x)
  (let ((translator (json/translator x)))
    (translator x)))
 
;; Accepts a Java String or Scheme string for a JSON list or collection and parses it into a Scheme list
;; or hash table respectively.
(define (json/translate s)
  (let ((parser (json/parser/new)))
    (json/translate/any (json/parse parser s))))

;; Using the given JSON parser parse the string s representation returning either a JSONArray
;; or a JSONObject object.
(define (json/parse parser s)
  (if (java-object? s)
      (parse parser s) ; Assume s is a Java String.
      (parse parser (->jstring s)))) ; Assume s is a Scheme string.

;; Given the Scheme representation j of a JSON collection or list generate the ASCII text representation.
(define (json/string j)
  (cond
   ((null? j)      "null")
   ((list? j)      (json/list/string j)) ; For association lists only.
   ((pair? j)      (format "~a:~a" (car j) (json/string (cdr j))))
   ((vector? j)    (json/vector/string j))
   ((hashtable? j) (json/hashtable/string j))
   ((string? j)    (format "~s" j))
   ((number? j)    (format "~a" j))
   ((boolean? j)   (if j "true" "false"))
   (else           "")))

;; Convert a Scheme list into the equivalent JSON list representation.
; (define (json/list/string j)
;   (let ((parts (map (lambda (x) (json/string x)) j)))
;     (format "[~a]" (string-join parts ", "))))

;; Convert a Scheme hashtable into the equivalent JSON collection representation.
(define (json/hashtable/string j)
  (let ((parts
	 (hashtable/map
	  (lambda (key value)
	    (format "\"~a\":~a" (symbol->string key) (json/string value)))
	  j)))
    (if (null? parts)
	"{}"
	(format "{~a}" (string-join parts ", ")))))

;; Convert a Scheme association list into the equivalent JSON collection respresentation.
(define (json/list/string j)
  (let ((parts (map (lambda (pair) (json/string pair)) j)))
    (if (null? parts)
	"{}"
	(format "{~a}" (string-join parts ", ")))))
	 
(define (vector/map f v)
  (let ((n (vector-length v)))
    (let loop ((i (- n 1)) (parts '()))
      (if (>= i 0)
	  (loop (- i 1) (cons (json/string (vector-ref v i)) parts))
	  parts))))

;; Convert a Scheme vector into the equivalent JSON array representation.
(define (json/vector/string v)
  (let ((parts (vector/map (lambda (element) (json/string element)) v)))
    (if (null? parts)
	"[]"
	(format "[~a]" (string-join parts ", ")))))

(define :translation-vector:)

(set! :null-java-json-parser: (java-null <java-json-parser>))
(set! :translation-vector:
  (vector
   (lambda (x) (->boolean x)) ; 0
   (lambda (x) (->number x))  ; 1
   (lambda (x) (->string x))  ; 2
   json/translate/array       ; 3
   json/translate/map))       ; 4

#|
(define (json/test/01)
  (let ((json (java-new <java-json-parser> (->jint 0)))
	(x #f))
    (set! x (parse json (->jstring "[99]")))
    (display (format "flavor:~d\n" (->number (flavor :null-java-json-parser: x))))
    (display (format "size:~d\n" (->number (size x))))
    (display (format "java:~a\n" (get x (->jint 0))))
    (display (format "value:~d\n" (->number (get x (->jint 0)))))
    x))

(define (json/test/02)
  (let* ((json (json/parser/new))
	 (x (parse json (->jstring "[99, true, 88, \"silly\", false, 3.14]"))))
    (display (format "size:~d\n" (->number (size x))))
    (display x) (newline)
    (display (json/translate/array x)) (newline)
    x))

(define (json/test/03)
  (let* ((json (json/parser/new))
	 (x (parse json (->jstring "{\"a\":99, \"b\":true, \"c\":88, \"d\":\"silly\", \"e\":false, \"f\":3.14}"))))
    (display (format "size:~d\n" (->number (size x))))
    (display x) (newline)
    (display (json/translate/map x)) (newline)
    x))

(define (json/test/04)
  (let ((x (json/translate
	    "{\"a\":99, \"b\":true, \"c\":[88, 44, 22, 11], \"d\":\"silly\", \"e\":false, \"f\":3.14, \"g\":null}")))
    (pretty-print x) (newline)
    x))
|#
