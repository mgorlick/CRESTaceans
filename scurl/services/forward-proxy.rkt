#lang racket
  (require "depends.rkt"
           "program.rkt"
           
           "../peer-validation/host-id.rkt"
           "../peer-validation/scurl.rkt")
  
  (provide
   (rename-out [remove-program remove-fproxy-program])
   
   add-fproxy-program
   run-fproxy-programs)
  
  ; Create the logger for the certification module.
  (define logger (make-logger 'forward-proxy-logger services-parent-logger))
  
  ; Uses the given values to create a program and add it to the given program list.
  ; The filter and exclude values all default to false which will disable them.
  ; Returns the id of added program and a new program-list which contains the new
  ; list of registered programs.
  ;
  ; (listof program?)
  ; (or string? url? scurl?)
  ; #:prefix string?
  ; #:filter string?
  ; #:exclude string?
  ; -> (values id program-list)
  (define (add-fproxy-program programs url #:prefix [prefix #f] #:filter [filter #f] #:exclude [exclude #f])
    ; Compile the filter and exclude into regular expressions.
    (letrec ((prog-url (cond
                         [(string? url) (string->url url)]
                         [(url? url) url]
                         [(scurl? url) url]
                         [else
                          (debug logger "The given url was not a valid url string, url or scurl.")
                          #f]))
             (str-prefix (if (string? prefix) prefix #f))
             (reg-filter (if (string? filter) (regexp filter) #f))
             (reg-exclude (if (string? exclude) (regexp exclude) #f))
             (id (program-list-next-id programs))
             (prog (program id str-prefix reg-filter reg-exclude prog-url)))
      ; Everything checks out, make a program and add it to the list.
      (program-list (append (program-list-programs programs) (list prog)) (add1 id))))
  
  ; Runs the given scurl against the given forward proxy program list.  If a scurl is found then it
  ; is returned, otherwise a value of false is returned.
  ; 
  ; program-list? (or string? url? scurl?) -> (or boolean? scurl?)
  (define (run-fproxy-programs fproxy-prog-list url)
    (letrec ((name (cond
                     [(string? url) url]
                     [(url? url) (url->string url)]
                     [(scurl? url) (url->string (scurl-url url))]
                     [else
                      (debug logger "Given url is not a string, url or scurl.  Not running the forward proxy programs and returning false.")
                      #f]))
             (run-fproxy-program
              (lambda (prog)
                (letrec (; The argument to run-program isn't used here because all procedures in
                         ; in a cert-program are either urls or scurls.
                         (output-list (run-program prog name name))
                         (prog-out (first output-list))
                         (output (last output-list)))
                  (cond
                    ; Not run, return false.
                    [(equal? 'not-run output) #f]
                    ; Returned an url or a scurl, let's return that.
                    [(or (url? output) (scurl? output)) output]
                    ; Don't know what was returned, return false.
                    [else #f])))))
      ; Invoke the run-program function for each program in the given list.  Returns the
      ; first non false value.
      (if (string? name)
          (ormap run-fproxy-program (program-list-programs fproxy-prog-list))
          #f)))
  

