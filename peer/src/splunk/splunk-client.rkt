#! /usr/bin/env racket
#lang racket

(require "util.rkt"
         "../util/xpath.rkt"
         (planet neil/json-parsing:1:=1))

(provide make-splunk-client
         splunk-search)

(define login-url-path "/services/auth/login")
(define search-jobs-path "/services/search/jobs")

;; Client setup: make a client instance only after logging in
;; and receiving a session key
(define (make-splunk-client service-base-url username password)
  (define login-url (string-append service-base-url login-url-path))
  (define (login sc service-base username password)
    (let-values ([(errorcode header-port body-port) 
                  (POST-wwwform sc login-url `((username . ,username) (password . ,password)))])
      (match errorcode
        ['ok 
         (let* ([response (output-port->xexpr body-port)])
           (match (simple-xpath* '(sessionKey) response)
             [(? string? a) a]
             [_ (error (format "Couldn't login to splunk at ~a: (probably) incorrect credentials" 
                               login-url))]))]
        [_ (error (format "Couldn't login to splunk at ~a: error was ~a"
                          login-url))])))
  (define h (new-handle))
  (let* ([sc (splunk-client h service-base-url #f)]
         [session-key (login sc service-base-url username password)])
    (set-splunk-client-session-key! sc session-key)
    (reset-client/except-auth sc)
    sc))

;; takes the result of a simple-xpath*/list selection on
;; an s:dict and looks through the keys for a given name
(define (find-in-s:dict-by-key-name key elems)
  (let ([res (filter (λ (entry)
                       (match entry
                         [(list 's:key (list (list 'name (? (curry string=? key) k))) a) #t]
                         [_ #f]))
                     elems)])
    (if (not (empty? res)) (first res) #f)))

(define (splunk-search sc term)
  (define search-url (string-append (splunk-client-base-url sc) search-jobs-path))
  (define sk (splunk-client-session-key sc))
  
  ;; we have to keep polling the job base URL until the results are in
  ;; unfortunately, Splunk server does not return useful headers in that
  ;; vein, so we have to keep requesting the whole /services/search/jobs/<jobid>
  ;; page and parsing the whole thing until the "isDone" key is equal to "1"
  ;; to ease the burden of doing this a little, sleep a little bit between
  ;; requests and only try some number of times before giving up
  
  ;; the curl auth header is already attached to this when it is called,
  ;; and before that it was reset, so we don't need to reset between calls
  (define (poll-for-results sid tries)
    (cond
      [(= tries 0) (error "Gave up on polling for results: taking too long")]
      [else 
       (let-values
           ([(errorcode header-port body-port)
             (GET sc (string-append search-url "/" sid))])
         (match errorcode
           ['ok (let* ([response (output-port->xexpr body-port)]
                       [is-done (string->number 
                                 (third
                                  (find-in-s:dict-by-key-name 
                                   "isDone"
                                   (simple-xpath*/list '(s:dict) response))))])
                  (if (= is-done 1)
                      (get-final-results sid)
                      (begin
                        (sleep 1)
                        (poll-for-results sid (sub1 tries)))))]))]))
  
  (define (get-final-results sid)
    (let-values ([(errorcode header-port body-port)
                  (GET sc (string-append search-url "/" sid "/results?output_mode=json"))])
      (match errorcode
        ['ok (let ([response (get-output-bytes body-port)])
               (reset-client/except-auth sc)
               (json->sjson (open-input-bytes response)))])))
  
  (let-values ([(errorcode header-port body-port) 
                (POST-wwwform sc search-url `((search . ,term)))])
    (reset-client/except-auth sc)
    (match errorcode
      ['ok 
       (let ([response (output-port->xexpr body-port)])
         (let ([sid 
                (match (simple-xpath* '(sid) response)
                  [(? string? a) a]
                  [_ (error "Couldn't start splunk job")])])
           (values sid (poll-for-results sid 10))))]
      [_ (error "Error requesting splunk job")])))