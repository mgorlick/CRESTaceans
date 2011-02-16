#lang racket

(require ffi/unsafe
         racket/runtime-path)
(provide (all-defined-out))

(define libnacl (ffi-lib "libnacl"))

; read-lines: pathstring -> (listof string)
; return all the lines in a file
(define (read-lines file-path)
  (with-input-from-file file-path
    (λ () (stream->list (in-lines)))))

; find-includes: (listof string) -> (listof string)
; return only the strings in the input list that match
; C's #include preprocessor macro
(define (find-includes filecontents)
  (filter-map 
   (λ (i) (and (regexp-match-exact? "^#include +\".*\\.h\"$" i) i))
   filecontents))

; find-defines: (listof string) -> (listof string)
; return only the strings in the input list that match 
; the simplest form of C's #define preprocessor macro
; of form #define <identifier-or-number> <identifier-or-number>
;; e.g., returned:
;; #define CONSTANT1 CONSTANT2
;; #define CONSTANT2 42
;; not returned:
;; #define CONSTANT1(x,y) ((x + y)
(define (find-defines filecontents)
  (filter-map 
   (λ (i) (and (regexp-match-exact? "^#define +[A-Z|a-z|0-9|_]+ +\"?[-|A-Z|a-z|0-9|_|/]+\"?$" i) i))
   filecontents))

; get-aliases: (listof string) -> (hash string string)
; ake a list of lines of the format produced by `find-defines'
; e.g., "#define CONSTANT1 CONSTANT2"
; and return a hash map cons cell form of the definition
; e.g., #hash(("CONSTANT1" . "CONSTANT2"))
(define (get-aliases dfs)
  (make-immutable-hash 
   (map (λ (df)
          (let ([matches (regexp-split " " df)])
            (cons (second matches) (third matches))))
        dfs)))

; second-level-names: pathstring (listof string) -> (hash string string)
; return the nested (i.e., #included)
; #define definitions from the given input lines:
; also provide the path of the file they were extracted from
(define (second-level-names original-path lines)
  (define (find-included-defines file-prefix i)
    (find-defines (read-lines (build-path file-prefix (substring i 10 (- (string-length i) 1))))))
  (let-values ([(base name dir?) (split-path original-path)])
    ; assume we only want the first include. 
    ; (in NaCl there's only one per top level "prettified" file)
    (get-aliases (find-included-defines base (first (find-includes lines))))))

; top-level-names: (listof string) -> (hash string string)
; return the #define definitions in the provided input list
(define (top-level-names lines)
  (get-aliases (find-defines lines)))

; cleanup: remove all values J from hash table H
; that also act as keys to H
; and reassign all V in (J,V) to K in (K,J)
; to yield a new hash of the form (K,V)
; plus the extra rows that do not fall into this formulation
; key1 -> key2   }
; ke2 -> value2  } => key1 -> value2
; key3 -> value3   => key3 -> val3
(define (cleanup h)
  ;; to foldl over an immutable hash means to do an operation on the 
  ;; "current state" of that hash, yielding a new hash, then applying 
  ;; the same operation to the next value in the list.
  ;; the result is the hash after applying the operation to all items in the list
  ;; and is equivalent to, but faster than, making a new hash from a list of pairs
  (define (double-lookup k ahash)
    (if (hash-has-key? ahash k) 
        (let ([j (hash-ref ahash k)])
          (if (hash-has-key? ahash j)
              (hash-remove (hash-set ahash k (hash-ref ahash j)) j)
              ahash))
        ahash))
  (foldl double-lookup h (hash-keys h)))

; merge: given two hashtables with corresponding entries: H1: (K,J) and H2: (J,V)
; produce a new hashtable with entries (K,V)
(define (merge h1 h2)
  (define (merge-an-entry k h)
    (let ([j (hash-ref h k)])
      (if (hash-has-key? h2 j)
          (hash-set h k (hash-ref h2 j))
          h)))
  (let ([h1-keys (hash-keys h1)])
    (foldl merge-an-entry h1 h1-keys)))

; hash-values-filter: remove all entries in the hash
; whose values match the regex
(define (hash-values-filter ahash rx do-if-match)
  (define (filter-matches key ahash)
    (if (regexp-match-exact? rx (hash-ref ahash key))
        (do-if-match ahash key)
        ahash))
  (foldl filter-matches ahash (hash-keys ahash)))

(define (hash-dequote-string-value ahash key)
  (hash-set ahash key (regexp-replace* "\"" (hash-ref ahash key) "")))

; get-real-names: pathstring -> (hash string string)
; return the final combination of "prettified" names and constants in the API
; with their actual (installation-specific) values, for use in looking up
; in the shared lib (or defining as constant)
(define (get-real-names file-path)
  (let ([lines (read-lines file-path)])
    (hash-values-filter
     (hash-values-filter 
      (merge (top-level-names lines) 
             (cleanup (second-level-names file-path lines)))
      "\"-\"" hash-remove) ; get rid of "\"-\"" values
     "\"[A-Z|a-z|0-9|_|/)]+\"" hash-dequote-string-value) ; replace "\"sha512\"" with "sha512"
    ))

(define (und->dash name)
  (regexp-replaces name '((#rx"-" "_"))))
(define (dash->und name)
  (regexp-replaces name '((#rx"_" "-"))))

(define-syntax define-constants
  (syntax-rules ()
    [(_ ahash id)
     (define id (let ([v (hash-ref ahash (und->dash 'id))]) (or (string->number v) v)))]
    [(_ ahash id1 id2 ...)
     (begin (define-constants ahash id1) (define-constants ahash id2 ...))]))

(define-syntax define-functions
  (syntax-rules (=)
    [(_ ahash (id funspec))
     (define id (get-ffi-obj (hash-ref ahash (und->dash 'id)) libnacl funspec))]
    [(_ ahash e1 e2 ...)
     (begin (define-functions ahash e1) (define-functions ahash e2 ...))]))