#lang racket/base

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

(require "../../../Motile/persistent/tuple.rkt"
         racket/function
         racket/match)


(provide
 message/ask/new
 :message/ask/method
 :message/ask/url
 :message/ask/body
 :message/ask/metadata
 :message/ask/reply
 :message/ask/echo
 
 message/tell/new
 :message/tell/status
 :message/tell/reason
 :message/tell/body
 :message/tell/metadata
 :message/tell/echo
 
 message/uri/new
 :message/uri/scheme
 :message/uri/authority
 :message/uri/path
 :message/uri/query
 
 message/any?
 message/ask?
 message/tell?
 message/uri?
 
 :no-body:
 :no-echo:
 :no-metadata:
 :no-reply:
 
 ask
 tell
 uri)

(define :no-body:     #f)
(define :no-echo:     #f)
(define :no-metadata: null)
(define :no-query:    null)
(define :no-reply:    #f)

(define MOTILE/MESSAGE/ASK  '(motile message ask))
(define MOTILE/MESSAGE/TELL '(motile message tell))
(define MOTILE/MESSAGE/URI  '(motile message uri))

(define-match-expander ask
  (syntax-rules ()
    [(_ method uri body metadata reply echo)
     (vector '<tuple> (? (curry equal? MOTILE/MESSAGE/ASK) flavor) method uri body metadata reply echo)]))

(define-match-expander tell
  (syntax-rules ()
    [(_ status reason body metadata echo)
     (vector '<tuple> (? (curry equal? MOTILE/MESSAGE/TELL) flavor) status reason body metadata echo)]))

(define-match-expander uri
  (syntax-rules ()
    [(_ scheme authority path query)
     (vector '<tuple> (? (curry equal? MOTILE/MESSAGE/URI) flavor) scheme authority path query)]))

;; An "ask" message is a 7-tuple with the structure #(flavor method url body metadata reply echo) where:
;;  flavor   - always the list (motile message ask)
;;  method   - a symbol, one of GET, POST, PUT, DELETE, REMOTE, or SPAWN
;;  url      - an url denoting the target thread of the message
;;  body     - the payload of the request
;;  metadata - metadata regarding the body and/or the request as a whole
;;  reply    - an url denoting the thread to which a reply should be directed
;;  echo     - arbitrary context provided by sender to be echoed in the response


;; A "tell" message is a response to an "ask" and is represented as a 6-tuple 
;; with the structure #(flavor status reason body metadata echo) where:
;;  flavor   - always the list (motile message tell)
;;  status   - a numeric code (positive integer) denoting the nature of the response
;;  reason   - a human-readable string expanding on the status code
;;  body     - the payload of the response
;;  metadata - metadata regarding the body and/or the response as a whole
;;  echo     - the echo of the ask to which this tell corresponds

;; Returns #t iff prefix, a list of symbols, is a prefix of, or equal to, actual.
(define (list/prefix? prefix actual)
  (let loop ((prefix prefix) (actual actual))
    (cond
      ((null? prefix) #t)
      ((null? actual)  #f)
      ((eq? (car prefix) (car actual))
       (loop (cdr prefix) (cdr actual)))
      (else #f))))

(define message/ask/new
  (case-lambda
    ((method url)
     (tuple MOTILE/MESSAGE/ASK method url :no-body: :no-metadata: :no-reply: :no-echo:))
    ((method url body metadata)
     (tuple MOTILE/MESSAGE/ASK method url body      metadata      :no-reply: :no-echo:))
    ((method url body metadata reply echo)
     (tuple MOTILE/MESSAGE/ASK method url body      metadata      reply      echo))))

(define (message/ask? x)
  (and
   (tuple? x)
   (= (tuple/length x) 7)
   (list/prefix? MOTILE/MESSAGE/ASK (tuple/ref x 0))))

(define-syntax-rule (message/ask/field m i field)
  (if (message/ask? m)
      (tuple/ref m i)
      (error (quote field) "not a message/ask: ~s" m)))

(define (:message/ask/method   m) (message/ask/field m 1 message/ask/method))
(define (:message/ask/url      m) (message/ask/field m 2 message/ask/url))
(define (:message/ask/body     m) (message/ask/field m 3 message/ask/body))
(define (:message/ask/metadata m) (message/ask/field m 4 message/ask/metadata))
(define (:message/ask/reply    m) (message/ask/field m 5 message/ask/reply))
(define (:message/ask/echo     m) (message/ask/field m 6 message/ask/echo))

(define (message/tell? x)
  (and
   (tuple? x)
   (= (tuple/length x) 6)
   (list/prefix? MOTILE/MESSAGE/TELL (tuple/ref x 0))))

(define-syntax-rule (message/tell/field m i field)
  (if (message/tell? m)
      (tuple/ref m i)
      (error field "not a message/tell ~s" m)))

(define (:message/tell/status   m) (message/tell/field m 1 'message/tell/status))
(define (:message/tell/reason   m) (message/tell/field m 1 'message/tell/reason))
(define (:message/tell/body     m) (message/tell/field m 1 'message/tell/body))
(define (:message/tell/metadata m) (message/tell/field m 1 'message/tell/metadata))
(define (:message/tell/echo     m) (message/tell/field m 1 'message/tell/echo))

(define message/tell/new
  (case-lambda
    ((status reason echo)
     (tuple MOTILE/MESSAGE/TELL status reason :no-body: :no-metadata: echo))
    ((status reason body metadata echo)
     (tuple MOTILE/MESSAGE/TELL status reason body      metadata      echo))))

;; A message is either an ask or a tell.
(define (message/any? x)
  (and
   (tuple? x)
   (or
    (and
     (= (tuple/length x) 7)
     (list/prefix? MOTILE/MESSAGE/ASK (tuple/ref x 0)))
    (and
     (= (tuple/length x) 6)
     (list/prefix? MOTILE/MESSAGE/TELL (tuple/ref x 0))))))

;; An URL or URI is a tuple #(flavor scheme authority path query) where:
;;  flavor - always the list (motile message uri)
;;  scheme -    a symbol consisting of a letter  followed by any combination of letters,
;;               digits, and the plus ("+"), period ("."), or hyphen ("-") characters
;;  authority - one of a thread handle or  (domain . port) pair where domain is either
;;               a domain name or IP address and port is an IP port number
;;  path      - a (possibly empty) list of symbols
;;  query     - an association list of (key . value) pairs

(define message/uri/new
  (case-lambda
    ((scheme authority)
     (tuple MOTILE/MESSAGE/URI scheme authority null :no-query:))
    ((scheme authority path)
     (tuple MOTILE/MESSAGE/URI scheme authority path :no-query:))
    ((scheme authority path query)
     (tuple MOTILE/MESSAGE/URI scheme authority path query))))

(define (message/uri? x)
  (and
   (tuple? x)
   (= (tuple/length x) 5)
   (list/prefix? MOTILE/MESSAGE/URI (tuple/ref x 0))))


(define-syntax-rule (message/uri/field u i field)
  (if (message/uri? u)
      (tuple/ref u i)
      (error field "not a message/uri ~s" u)))

(define (:message/uri/scheme    u) (message/uri/field u 1 'message/uri/scheme))
(define (:message/uri/authority u) (message/uri/field u 2 'message/uri/authority))
(define (:message/uri/path      u) (message/uri/field u 3 'message/uri/path))
(define (:message/uri/query     u) (message/uri/field u 4 'message/uri/query))