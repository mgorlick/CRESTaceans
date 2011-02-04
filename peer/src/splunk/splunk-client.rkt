#lang racket

(require "util.rkt"
         "../util/xpath.rkt"
         (planet neil/json-parsing:1:=1))

(provide make-splunk-client
         start-search
         check-on-search
         get-search-results)

(define login-url-path "/services/auth/login")
(define search-jobs-path "/services/search/jobs")

;; Client setup: make a client instance only after logging in
;; and receiving a session key
(define (make-splunk-client service-base-url username password)
  (define login-url (string-append service-base-url login-url-path))
  (define (login sc service-base username password)
    (let-values ([(errorcode header-port body-port) 
                  (POST-wwwform sc login-url `((username . ,username) (password . ,password)))])
      (if (eq? errorcode 'ok)
          (let* ([response (output-port->xexpr body-port)])
            (match (simple-xpath* '(sessionKey) response)
              [(? string? a) a]
              [_ (error "Couldn't login to splunk: (probably) incorrect credentials. URL =" 
                                login-url)]))
          (error "Couldn't login to splunk at ~a: error was ~a" login-url errorcode))))
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

;; start a search job, returning the job ID so that the client
;; can check up on that job again later
;; we don't force a synchronous structure on waiting for job results
;; because programmers may want to start several jobs and wait on them all
;; simultaneously: let the library consumer handle it at a higher level
(define (start-search sc term)
  (define search-url (string-append (splunk-client-base-url sc) search-jobs-path))
  (define sk (splunk-client-session-key sc))
  (let-values ([(errorcode header-port body-port) (POST-wwwform sc search-url `((search . ,term)))])
    (reset-client/except-auth sc)
    (match errorcode
      ['ok  (let* ([response (output-port->xexpr body-port)]
                   [sid (match (simple-xpath* '(sid) response)
                          [(? string? a) a]
                          [_ (error "Couldn't start splunk job, raw response: " response)])])
              sid)]
      [_ (error "Error requesting splunk job: ~a" errorcode)])))

;; we have to keep polling the job base URL until the results are in.
;; unfortunately, Splunk server does not return useful headers in that
;; regard, so we have to keep requesting the whole /services/search/jobs/<jobid>
;; page and parsing the whole thing until the "isDone" key is equal to "1"
(define (check-on-search sc sid)
  (define job-url (string-append (splunk-client-base-url sc) search-jobs-path "/" sid))
  (define sk (splunk-client-session-key sc))
  (reset-client/except-auth sc)
  (let-values
      ([(errorcode header-port body-port) (GET sc job-url)])
    (match errorcode
      ['ok (let* ([response (output-port->xexpr body-port)]
                  [is-done (third (find-in-s:dict-by-key-name 
                                   "isDone" (simple-xpath*/list '(s:dict) response)))])
             (string=? is-done "1"))]
      [_ (error "Error checking on search results: " errorcode)])))

;; once the search results are actually marked done we can retrieve them
;; here we use JSON instead of XML because it's the one resource type that
;; actually generates JSON (all the others report it as an invalid resource)
(define (get-search-results sc sid)
  (define job-results-url (string-append (splunk-client-base-url sc)
                                         search-jobs-path "/" sid "/results?output_mode=json"))
  (define sk (splunk-client-session-key sc))
  (let-values ([(errorcode header-port body-port) (GET sc job-results-url)])
    (reset-client/except-auth sc)
    (match errorcode
      ['ok (let ([response (get-output-bytes body-port)])
             (reset-client/except-auth sc)
             (json->sjson (open-input-bytes response)))]
      [_ (error "Error retrieving or decodind search results: " errorcode)])))