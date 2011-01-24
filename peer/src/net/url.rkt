#lang racket/base


(require (prefix-in rfc: net/url)
         racket/contract
         racket/match
         racket/list)

; CREST URLs do not have the same semantics as RFC 2396 URLs
; but an RFC 2396-compliant parser can be used to parse one
; assuming the program respects the additional constraints of
; a CREST URL, illustrated as follows

; crest://alice@bob.org:8000/d41d8cd98f00b204e9800998ecf8427e/226155721/application/specific/path;add2num?x=5&y=4#cont
; {-1-}  {--2--}{--3--}{-4-} {-------------5----------------} {---6---} {----------7------------} {--8--} {--9--} {10}
; 1: scheme
; 2: user
; 3: host
; 4: port
; 5: public key
; 6: public swiss number
; 7: path
; 8: param
; 9: query
; 10: fragment

; we currently ignore user (2) and group param through fragment (7-10) into one
; opaque (application-specific) element, which is irrelevant for purposes of
; network exchange

(struct crest-url (u public-key swiss-num) #:transparent)

(define (crest-url-scheme curl)
  (rfc:url-scheme (crest-url-u curl)))

(define (crest-url-host curl)
  (rfc:url-host (crest-url-u curl)))

(define (crest-url-port curl)
  (rfc:url-port (crest-url-u curl)))

(define (crest-url-path curl)
  (cddr (rfc:url-path (crest-url-u curl))))

(define (crest-url-pathstring curl)
  (define (path/param->string pp)
    (cond [(param? pp) 
           (string-append (path/param-path->string pp) ";" 
                          (apply string-append (add-between (rfc:path/param-param pp) ";")))]
          [else (path/param-path->string pp)]))
  (define (path/param-path->string pp)
    (match (rfc:path/param-path pp)
      ['up ".."] 
      ['same "."] 
      [".." "%2e%2e"]
      ["." "%2e"]
      [else (rfc:path/param-path pp)]))
  (define (param? pp) (not (empty? (rfc:path/param-param pp))))
  (apply string-append (add-between (map path/param->string (cddr (rfc:url-path (crest-url-u curl)))) "/")))

(define (crest-url-query curl)
  (rfc:url-query (crest-url-u curl)))

(define (crest-url-fragment curl)
  (rfc:url-fragment (crest-url-u curl)))

; convert a string to a CREST URL by using the URL parser,
; then verifying that (1) the URL has at least two RFC 2396-termed
; path subcomponents (which we term above to be items #5 and #6, the
; public key and public swiss number), and (2) that they
; are legal keys and swiss numbers respectively
; return #f if the string would make an invalid CREST URL
(define (string->crest-url s)
  (with-handlers ([exn:fail:contract? (λ (e) #f)]) ; return #f if empty url path
    (let* ([rfc-url (rfc:string->url s)]
           [pub-key (rfc:path/param-path (first (rfc:url-path rfc-url)))]
           [swiss-num (rfc:path/param-path (second (rfc:url-path rfc-url)))])
      (let ([c (crest-url rfc-url pub-key swiss-num)])
        (and (valid-crest-url? c) c)))))

; check to see that no one has constructed a CREST URL
; with URL components that don't match the duplicated
; public key and swiss number fields
; we allow functions that produce CREST URLs to either
; produce `#f' or a value that satisfies valid-crest-url?
; so this just enforces the equality and encoding constraints
; on any functions that produce valid CREST URLs
(define (valid-crest-url? curl)
  (and curl
       (crest-url? curl)
       (base64-url-encoded? (crest-url-public-key curl))
       (isnum? (crest-url-swiss-num curl))))

; test whether a string represents a base64-URL-encoded value
(define (base64-url-encoded? s)
  (regexp-match-exact? #rx"[-|0-9|A-Z|a-z|_]+" s))

(define (isnum? s)
  (regexp-match-exact? #rx"[0-9]+" s))

; don't enforce any valid-crest-url? constraints in logic here;
; just let the contract catch it
(define (crest-url->string curl)
  (rfc:url->string (crest-url-u curl)))

(provide/contract
 ; accessors
 [crest-url-host (valid-crest-url? . -> . string?)]
 [crest-url-port (valid-crest-url? . -> . number?)]
 [crest-url-path (valid-crest-url? . -> . (listof rfc:path/param?))]
 [crest-url-pathstring (valid-crest-url? . -> . path-string?)]
 [crest-url-public-key (valid-crest-url? . -> . string?)]
 [crest-url-swiss-num (valid-crest-url? . -> . string?)]
 [crest-url-query (valid-crest-url? . -> . (listof (cons/c symbol? (or/c string? #f))))]
 [crest-url-fragment (valid-crest-url? . -> . (or/c string? #f))]
 ; extra stuff
 [valid-crest-url? ((or/c crest-url? #f) . -> . boolean?)]
 [base64-url-encoded? (string? . -> . boolean?)]
 [isnum? (string? . -> . boolean?)]
 [crest-url->string (valid-crest-url? . -> . string?)]
 ; allow string->crest-url to return #f for invalid URLs,
 ; so exceptions are not thrown deep in the peer
 [string->crest-url (string? . -> . (or/c valid-crest-url? #f))])