#lang racket

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

(require (only-in "../../../Motile/persistent/tuple.rkt" tuple tuple? tuple/length tuple/ref))


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
 :no-reply:)

;; Upper level of Accomplice messaging.

;(define :method/get:     'get)
;(define :method/post:    'post)
;(define :method/put:     'put)
;(define :method/delete:  'delete)
;(define :method/remote:  'remote)
;(define :method/response 'response)
;(define :method/spawn:   'spawn)

(define :no-body:     #f)
(define :no-echo:     #f)
(define :no-metadata: null)
(define :no-query:    null)
(define :no-reply:    #f)

;(define :ask-id:  '33bd9138-f42d-4da2-b400-cc6aea1b402f)
;(define :tell-id: '92763926-abdf-4dc5-9c22-d02fcfe4b516)

(define MISCHIEF/MESSAGE/ASK  '(mischief message ask))
(define MISCHIEF/MESSAGE/TELL '(mischief message tell))
(define MISCHIEF/MESSAGE/URI  '(mischief message uri))

;; An "ask" message is a 7-tuple with the structure #(flavor method url body metadata reply echo) where:
;;  flavor   - always the list (mischief message ask)
;;  method   - a symbol, one of GET, POST, PUT, DELETE, REMOTE, or SPAWN
;;  url      - an url denoting the target thread of the message
;;  body     - the payload of the request
;;  metadata - metadata regarding the body and/or the request as a whole
;;  reply    - an url denoting the thread to which a reply should be directed
;;  echo     - arbitrary context provided by sender to be echoed in the response


;; A "tell" message is a response to an "ask" and is represented as a 6-tuple 
;; with the structure #(flavor status reason body metadata echo) where:
;;  flavor   - always the list (mischief message tell)
;;  status   - a numeric code (positive integer) denoting the nature of the response
;;  reason   - a human-readable string expanding on the status code
;;  body     - the payload of the response
;;  metadata - metadata regarding the body and/or the response as a whole
;;  echo     - the echo of the ask to which this tell corresponds

(define message/ask/new
  (case-lambda
    ((method url)
     (tuple MISCHIEF/MESSAGE/ASK method url :no-body: :no-metadata: :no-reply: :no-echo:))
    ((method url body metadata)
     (tuple MISCHIEF/MESSAGE/ASK method url body      metadata      :no-reply: :no-echo:))
    ((method url body metadata reply echo)
     (tuple MISCHIEF/MESSAGE/ASK method url body      metadata      reply      echo))))


;; Returns #t iff prefix, a list of symbols, is a prefix of, or equal to, actual.
(define (list/prefix? prefix actual)
  (let loop ((prefix prefix) (actual actual))
    (cond
      ((null? prefix) #t)
      ((null? actual)  #f)
      ((eq? (car prefix) (car actual))
       (loop (cdr prefix) (cdr actual)))
      (else #f))))

(define (message/ask? x)
  (and
   (tuple? x)
   (= (tuple/length x) 7)
   (list/prefix? MISCHIEF/MESSAGE/ASK (tuple/ref x 0))))

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
   (list/prefix? MISCHIEF/MESSAGE/TELL (tuple/ref x 0))))

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
     (tuple MISCHIEF/MESSAGE/TELL status reason :no-body: :no-metadata: echo))
    ((status reason body metadata echo)
     (tuple MISCHIEF/MESSAGE/TELL status reason body      metadata      echo))))

;(define (ask/new
;         method url
;         #:body (body :no-body:)
;         #:metadata (metadata :no-metadata:)
;         #:reply (reply :no-reply:)
;         #:echo (echo :no-echo:))
;  (vector-immutable :ask-id: method url body metadata reply echo))
;(define-syntax-rule (:ask/method   x) (vector-ref x 1)) ; Method designator.
;(define-syntax-rule (:ask/url      x) (vector-ref x 2)) ; http/url structure.
;(define-syntax-rule (:ask/body     x) (vector-ref x 3)) ; Payload.
;(define-syntax-rule (:ask/metadata x) (vector-ref x 4)) ; Metadata regarding the request and/or payload.
;(define-syntax-rule (:ask/reply    x) (vector-ref x 5)) ; Point to which response (if any) should be directed.
;(define-syntax-rule (:ask/echo     x) (vector-ref x 6)) ; Arbitrary context provided by requestor to be echoed in response.

;(define (tell/new status reason body metadata echo)
;(define (tell/new
;         code reason
;         #:body (body :no-body:)
;         #:metadata (metadata :no-metadata:)
;         #:echo (echo :no-echo:))
;  (vector-immutable :tell-id: code reason body metadata echo))
;(define-syntax-rule (:tell/code     x) (vector-ref x 1)) ; Positive integer status code.
;(define-syntax-rule (:tell/reason   x) (vector-ref x 2)) ; Brief reason phrase for status code.
;(define-syntax-rule (:tell/body     x) (vector-ref x 3)) ; Payload.
;(define-syntax-rule (:tell/metadata x) (vector-ref x 4)) ; Metadata regarding the response and/or payload.
;(define-syntax-rule (:tell/echo     x) (vector-ref x 5)) ; Arbitrary context from requestor echoed in response.

;(define (message/ask? x)
;  (and (vector? x) (= (vector-length x) 7) (eq? (vector-ref x 0) :ask-id:)))

;(define (message/tell? x)
;  (and (vector? x) (= (vector-length x) 6) (eq? (vector-ref x 0) :tell-id:)))

;(define alpha (response/new 'foobar "/a/b/c" (list 1  2 3) :no-metadata: (cons 33 "who cares?") "echo"))

;; A message is either an ask or a tell.
(define (message/any? x)
  (and
   (tuple? x)
   (or
    (and
     (= (tuple/length x) 7)
     (list/prefix? MISCHIEF/MESSAGE/ASK (tuple/ref x 0)))
    (and
     (= (tuple/length x) 6)
     (list/prefix? MISCHIEF/MESSAGE/TELL (tuple/ref x 0))))))



;; An URL or URI is a tuple #(flavor scheme authority path query) where:
;;  flavor - always the list (mischief message uri)
;;  scheme -    a symbol consisting of a letter  followed by any combination of letters,
;;               digits, and the plus ("+"), period ("."), or hyphen ("-") characters
;;  authority - one of a thread handle or  (domain . port) pair where domain is either
;;               a domain name or IP address and port is an IP port number
;;  path      - a (possibly empty) list of symbols
;;  query     - an association list of (key . value) pairs

(define message/uri/new
  (case-lambda
    ((scheme authority)
     (tuple MISCHIEF/MESSAGE/URI scheme authority null :no-query:))
    ((scheme authority path)
     (tuple MISCHIEF/MESSAGE/URI scheme authority path :no-query:))
    ((scheme authority path query)
     (tuple MISCHIEF/MESSAGE/URI scheme authority path query))))

(define (message/uri? x)
  (and
   (tuple? x)
   (= (tuple/length x) 5)
   (list/prefix? MISCHIEF/MESSAGE/URI (tuple/ref x 0))))


(define-syntax-rule (message/uri/field u i field)
  (if (message/uri? u)
      (tuple/ref u i)
      (error field "not a message/uri ~s" u)))

(define (:message/uri/scheme    u) (message/uri/field u 1 'message/uri/scheme))
(define (:message/uri/authority u) (message/uri/field u 2 'message/uri/authority))
(define (:message/uri/path      u) (message/uri/field u 3 'message/uri/path))
(define (:message/uri/query     u) (message/uri/field u 4 'message/uri/query))

;(define (message/any? x)
;  (and (vector? x)
;       (or
;        (and (= (vector-length x) 7)
;             (eq? (vector-ref x 0) :ask-id:))
;        (or (= (vector-length x) 7)
;            (eq? (vector-ref x 0) :tell-id:)))))

; ask = #(id method uri body metadata reply echo)
;(define-match-expander ask/method
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ pattern _ _ _ _ _)]))
;
;(define-match-expander ask/url
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ _ pattern _ _ _ _)]))
;    
;(define-match-expander ask/body
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ _ _ pattern _ _ _)]))
;
;(define-match-expander ask/metadata
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ _ _ _ pattern _ _)]))
;
;(define-match-expander ask/reply
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ _ _ _ _ pattern _)]))
;
;(define-match-expander ask/echo
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ _ _ _ _ _ pattern)]))
;
;;; tell = #(id code reason body metadata echo)
;(define-match-expander tell/code
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ pattern _ _ _ _)]))
;
;(define-match-expander tell/reason
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ _ pattern _ _ _)]))     
;
;(define-match-expander tell/body
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ _ _ pattern _ _)]))
;
;(define-match-expander tell/metadata
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ _ _ _ pattern _)]))
;
;(define-match-expander tell/echo
;  (syntax-rules ()
;    [(_ pattern)
;     (vector _ _ _ _ _ pattern)]))



;(define (! to body . arguments)
;  (match
;   arguments
;   (()                       (message/! to body :no-metadata: :no-reply: :no-echo:))
;   ((,metadata)              (message/! to body metadata      :no-reply: :no-echo:))
;   ((,metadata ,reply)       (message/! to body metadata      reply      :no-echo:))
;   ((,metadata ,reply ,echo) (message/! to body metadata      reply      echo))))

;; Used for the common case in the reply to a transaction in which:
;; (1) The reply terminates the transaction
;; (2) the intent of the transaction is wholly contained within or directly
;;     implied to the receiver by the echo (context)
;; Usage: (!! to body) or (!! to body echo).
;; Any additional arguments beyond the echo are ignored and not transmitted.
;(define (!! to body . arguments)
;  (if (null? arguments)
;      (message/! to body  :no-metadata: :no-reply: :no-echo:)
;      (message/! to body  :no-metadata: :no-reply: (car arguments))))
;
;(define (message/! to body metadata reply echo)
;  (cond
;   ((pair? to) ; (mailbox . path)
;    (let* ((mailbox (car to))
;	   (path    (cdr to))
;	   (m       (message/new path body metadata reply echo)))
;      (mailbox/send! mailbox m)))
;
;   ((string? to) #f) ; Unimplemented. Here to is a text URL like "http://www.example.com/a/b/c".
;
;   (else #f))) ; Need to implement for <java.net.URI>.
;
;(define (message/test/01)
;  (let ((alpha (make <mailbox>))
;	(beta  (make <mailbox>))
;	(m     #f))
;    (! (@ alpha 'ping) (list 1 2 3) :no-metadata: (@ beta 'pong) 'foobar)
;    (? alpha)))
