#lang racket/base

(require racket/contract racket/list)
  
  (require "depends.rkt"
           "scurl.rkt"
           "peer-validation-msgs.rkt")
  
  (provide scurl->strings
           strings->scurl)
  
  ; Create the logger for the scurl-utils module.
  (define logger (make-logger 'scurl-utils-logger peer-validation-parent-logger))
  
  ; Convert a scurl into a string which can be used to convert back to a scurl.
  (define (scurl->strings s)
    (let ((is-private (private-scurl? s)))
      (list
       ; URL contains host-id.
       (url->string (scurl-url s))
       (number->string (digest->integer (scurl-digest-type s)))
       (number->string (pkey->integer (scurl-key-type s)))
       (if is-private "1" "0")
       (bytes->string/latin-1 (if is-private
                                  (private-key->bytes (scurl-key s))
                                  (public-key->bytes (scurl-key s))))
     )))
  
  ; Converts a string into a scurl if the string is in the same form that scurl->string
  ; produces.
  (define (strings->scurl l)
    (if (= (length l) 5)
        ; Extract string parts from the list.
        (let ((url-string (first l))
              (digest-type-string (second l))
              (key-type-string (third l))
              (is-private-string (fourth l))
              (key-string (fifth l)))
          ; Perform conversion from strings.
          (with-handlers ((exn? (lambda (e)
                            (debug logger "Failed to convert the string values into scurl components." e)
                            #f)))
            (let ((digest-type (integer->digest (string->number digest-type-string)))
                  (key-type (integer->pkey (string->number key-type-string)))
                  (is-private (string=? is-private-string "1"))
                  (key (string->bytes/latin-1 key-string)))
              (if (and (!digest? digest-type)
                       (!pkey? key-type)
                       (bytes? key))
                  (let ((key (if is-private
                                 (bytes->private-key key-type key)
                                 (bytes->public-key key-type key))))
                    (string->scurl url-string digest-type key-type key))
                  #f))))
        #f))

