#lang typed/racket

(require "base64-url-typed.rkt")
(require/typed net/url
               [opaque URL url?]
               [opaque Path/Param path/param?]
               [url-scheme (URL -> (Option String))]
               [url-user (URL -> (Option String))]
               [url-host (URL -> (Option String))]
               [url-port (URL -> (Option Integer))]
               [url-path (URL -> (Listof Path/Param))]
               [url-query (URL -> (Listof (Pairof Symbol (Option String))))]
               [url-fragment (URL -> (Option String))]
               [string->url (String -> URL)]
               [url->string (URL -> String)]
               [path/param-param (Path/Param -> (Listof String))]
               [path/param-path (Path/Param -> (U 'same 'up String))]
               )

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

(struct: crest-url
         ([u : URL]
          [public-key : String]
          [swiss-num : String]) #:transparent)

(: crest-url-scheme (crest-url -> (Option String)))
(define (crest-url-scheme curl)
  (url-scheme (crest-url-u curl)))

(: crest-url-host (crest-url -> (Option String)))
(define (crest-url-host curl)
  (url-host (crest-url-u curl)))

(: crest-url-port (crest-url -> (Option Integer)))
(define (crest-url-port curl)
  (url-port (crest-url-u curl)))

(: crest-url-path (crest-url -> (Listof Path/Param)))
(define (crest-url-path curl)
  (cddr (url-path (crest-url-u curl))))

(: crest-url-pathstring (crest-url -> String))
(define (crest-url-pathstring curl)
  
  (: path/param->string (Path/Param -> String))
  (define (path/param->string pp)
    (cond [(param? pp) 
           (string-append (path/param-path->string pp) ";" 
                          (apply string-append (add-between (path/param-param pp) ";")))]
          [else (path/param-path->string pp)]))
  
  (: path/param-path->string (Path/Param -> String))
  (define (path/param-path->string pp)
    (let ([p (path/param-path pp)])
      (cond
        [(eq? 'up p) ".."] 
        [(eq? 'same p) "."]
        [else p])))
  
  (: param? (Path/Param -> Boolean))
  (define (param? pp) (not (empty? (path/param-param pp))))
  
  (apply string-append (add-between (map path/param->string (cddr (url-path (crest-url-u curl)))) "/")))

(: crest-url-query (crest-url -> (Listof (Pairof Symbol (Option String)))))
(define (crest-url-query curl)
  (url-query (crest-url-u curl)))

(: crest-url-fragment (crest-url -> (U String False)))
(define (crest-url-fragment curl)
  (url-fragment (crest-url-u curl)))

; convert a string to a CREST URL by using the URL parser,
; then verifying that (1) the URL has at least two RFC 2396-termed
; path subcomponents (which we term above to be items #5 and #6, the
; public key and public swiss number), and (2) that they
; are legal keys and swiss numbers respectively
; return #f if the string would make an invalid CREST URL
(: string->crest-url (String -> (Option crest-url)))
(define (string->crest-url s)
  (with-handlers ([exn:fail:contract? (λ (e) #f)]) ; return #f if empty url path
    (let* ([rfc-url (string->url s)]
           [pub-key (path/param-path (first (url-path rfc-url)))]
           [swiss-num (path/param-path (second (url-path rfc-url)))])
      (if (and (string? pub-key) (string? swiss-num))
          (let ([c (crest-url rfc-url pub-key swiss-num)])
            (and (valid-crest-url? c) c))
          #f))))

; check to see that no one has constructed a CREST URL
; with URL components that don't match the duplicated
; public key and swiss number fields
; we allow functions that produce CREST URLs to either
; produce `#f' or a value that satisfies valid-crest-url?
; so this just enforces the equality and encoding constraints
; on any functions that produce valid CREST URLs
(: valid-crest-url? (Any -> Boolean))
(define (valid-crest-url? curl)
  (and (crest-url? curl)
       (base64-url-encoded? (crest-url-public-key curl))
       (base64-url-encoded? (crest-url-swiss-num curl))))

; don't enforce any valid-crest-url? constraints in logic here;
; just let the contract catch it
(: crest-url->string (crest-url -> String))
(define (crest-url->string curl)
  (url->string (crest-url-u curl)))

(provide (all-from-out "base64-url-typed.rkt"))
(provide (all-defined-out))