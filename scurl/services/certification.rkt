#lang racket
  
  (require "depends.rkt"
           "program.rkt"
           
           "../peer-validation/scurl.rkt")
  
  (provide
   (rename-out
    [add-program add-cert-program]
    [remove-program remove-cert-program])
   
   run-cert-programs)
  
  ; Create the logger for the certification module.
  (define logger (make-logger 'certification-logger services-parent-logger))
  
  ; Runs the given url, scurl or string against the given certification program list.  If a scurl is found then it
  ; is returned, otherwise a value of false is returned.
  ; 
  ; program-list? (or string? url? scurl?) -> (or boolean? scurl?)
  (define (run-cert-programs cert-prog-list url)
    (letrec ((name (cond
                     [(string? url) url]
                     [(url? url) (url->string url)]
                     [(scurl? url) (url->string (scurl-url url))]
                     [else
                      (debug logger "Given url is not a string, url or scurl.  Not running the certification programs and returning false.")
                      #f]))
             (run-cert-program
              (lambda (prog)
                (letrec (; If the program is run the argument should be the name with the prefix stripped.
                         (arg (strip-prefix name prog))
                         (output-list (run-program prog name arg))
                         (prog-out (first output-list))
                         (output (last output-list)))
                      (if (scurl? output)
                          ; The procedure returned a scurl
                          output
                          ; The procedure was either not run or returned some other unknown value, return false.
                          #f)))))
      ; Invoke the run-program function for each program in the given list.  Returns the
      ; first non false value.
      (if (string? name)
          (ormap run-cert-program (program-list-programs cert-prog-list))
          #f)))

