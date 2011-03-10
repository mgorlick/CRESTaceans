#lang racket

(require "util.rkt"
         "xpath.rkt")
(provide make-splunk-client
         start-search
         check-on-search
         get-search-results)

(define login-url-path "/services/auth/login")
(define search-jobs-path "/services/search/jobs")

;; query:
;; ([f : GET | POST-wwwform | ...] splunk-client args-to-f ...) (output-port -> any) string
(define-syntax (query stx)
  (syntax-case stx ()
    [(_ (request-method sc e1 ...) converter errstr)
     #'(let-values ([(errorcode header-port body-port) (request-method sc e1 ...)])
         (reset-client/except-auth sc)
         (match errorcode
           ['ok (converter body-port)]
           [_ (error errstr errorcode)]))]))

;; Client setup: make a client instance only after logging in
;; and receiving a session key
(define (make-splunk-client service-base-url username password)
  (define login-url (string-append service-base-url login-url-path))
  (define (login sc service-base username password)
    (let* ([response (query 
                      (POST-wwwform sc login-url `((username . ,username) (password . ,password)))
                      output-port->xexpr "Couldn't log into splunk: ")])
      (match (simple-xpath* '(sessionKey) response)
        [(? string? a) a]
        [_ (error "Couldn't login to splunk: (probably) incorrect credentials. URL =" 
                  login-url)])))
  (let* ([sc (splunk-client (new-handle) service-base-url #f)]
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
                         [_ #f])) elems)])
    (and (not (empty? res)) (first res))))

;; start a search job, returning the job ID so that the client
;; can check up on that job again later
;; we don't force a synchronous structure on waiting for job results
;; because programmers may want to start several jobs and wait on them all
;; simultaneously: let the library consumer handle it at a higher level
(define (start-search sc term)
  (define search-url (string-append (splunk-client-base-url sc) search-jobs-path))
  (define sk (splunk-client-session-key sc))
  (let* ([response (query (POST-wwwform sc search-url `((search . ,term)))
                          output-port->xexpr "Error starting splunk search: ")])
    (match (simple-xpath* '(sid) response)
      [(? string? a) a]
      [_ (error "Couldn't start splunk job, raw response: " response)])))

;; we have to keep polling the job base URL until the results are in.
;; unfortunately, Splunk server does not return useful headers in that
;; regard, so we have to keep requesting the whole /services/search/jobs/<jobid>
;; page and parsing the whole thing until the "isDone" key is equal to "1"
(define (check-on-search sc sid)
  (define job-url (string-append (splunk-client-base-url sc) search-jobs-path "/" sid))
  (define sk (splunk-client-session-key sc))
  (let* ([response (query (GET sc job-url) output-port->xexpr "Error checking on search results: ")]
         [is-done (find-in-s:dict-by-key-name "isDone" (simple-xpath*/list '(s:dict) response))])
    (string=? (third is-done) "1")))

;; once the search results are actually marked done we can retrieve them
;; here we use JSON instead of XML because it's the one resource type that
;; actually generates JSON (all the others report it as an invalid resource)
(define (get-search-results sc sid)
  (define job-results-url (string-append (splunk-client-base-url sc) search-jobs-path
                                         "/" sid "/results?output_mode=json"))
  (define sk (splunk-client-session-key sc))
  (query (GET sc job-results-url) output-port->sjson "Error retrieving search results: "))