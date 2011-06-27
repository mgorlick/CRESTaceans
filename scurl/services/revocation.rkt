#lang racket
  
  (require "depends.rkt"
           "program.rkt"
           
           "../peer-revocation/revo-cert.rkt"
           "../peer-validation/host-id.rkt"
           "../peer-validation/scurl.rkt")
  
  (provide
   (rename-out [remove-program remove-revo-program])
   (struct-out revo-program)
   revo-program=?
           
   add-revo-program
   run-revo-programs)
  
  ; Create the logger for the revocation module.
  (define logger (make-logger 'revocation-logger services-parent-logger))
  
  ; The block flag denotes whether host-id blocking is turned on or off.
  (struct revo-program program (block))
  
  ; Returns true when both items are revo-program's with the same state.
  ;
  ; any any -> boolean?
  (define (revo-program=? rp1 rp2)
    (and (revo-program? rp1)
         (revo-program? rp2)
         (equal? (revo-program-block rp1)
                 (revo-program-block rp2))
         (program=? rp1 rp2)))
  
  ; Uses the given values to create a revo-program and add it to the given program list.
  ; The block, filter and exclude values all default to false which will disable them.
  ; Returns the id of added program and a new program-list which contains the new
  ; list of registered programs.
  ;
  ; (listof program?)
  ; (or procedure? any)
  ; #:block boolean?
  ; #:filter (or boolean? string?)
  ; #:exclude (or boolean? string?)
  ; -> (values id program-list)
  (define (add-revo-program programs procedure #:block [block #f] #:filter [filter #f] #:exclude [exclude #f])
    ; Compile the filter and exclude into regular expressions.
    (letrec ((prefix #f)
             (reg-filter (if (boolean? filter) filter (regexp filter)))
             (reg-exclude (if (boolean? exclude) exclude (regexp exclude)))
             (id (program-list-next-id programs))
             (prog (revo-program id prefix reg-filter reg-exclude procedure block)))
      ; Everything checks out, make a program and add it to the list.
      (program-list (append (program-list-programs programs) (list prog)) (add1 id))))
  
  ; Runs the given scurl against the given program-list.  If a valid revocation certificate
  ; is found than that is returned and should signal the revocation of the scurl.  If an
  ; exception occurs while running a procedure than a value of false will be returned and
  ; finally, if the procedure runs without complications, but doesn't return a revo-cert then
  ; the value of the program-block attribute will be returned.
  ;
  ; program-list? scurl? -> (or revo-cert? boolean?)
  (define (run-revo-programs revo-prog-list scurl)
    (letrec (; If the given scurl is not a scurl then we pass an empty string as the host-id.
             (arg (if (scurl? scurl) (host-id-bytes->host-id-string (scurl-host-id scurl)) ""))
             (name (if (scurl? scurl) (url->string (scurl-url scurl)) (url->string scurl)))
             (run-revo-program
              (lambda (prog)
                ; Invoke the run-revo-prog function for each revo-prog in the given list.  Returns the
                ; first non false value.
                (letrec ((output-list (run-program prog name arg))
                         (prog-out (first output-list))
                         (output (last output-list)))
                  (cond
                    ; If not run, return false.
                    [(equal? 'not-run output) #f]
                    ; Is this a valid revo-cert?
                    [(revo-cert? output)
                     ; The procedure ran succesfully and returned a revocation certificate,
                     ; make sure the scurl in the revocation certificate is for the requested
                     ; scurl and that it is correctly signed.
                     (if (and
                          (scurl=? scurl (revo-cert-scurl output))
                          (valid-revo-cert? output))
                         ; The certificate is valid, let's return it.
                         output
                         (begin
                           (debug logger "The procedure returned an invalid revo-cert.")
                           #f))]
                    ; The procedure was run, did it run without raising an exception?
                    [(and
                      (not (exn? output))
                      (revo-program? prog))
                     ; The procedure ran successfully, but did not return a revocation certificate.
                     ; Let's return the block attribute.
                     (revo-program-block prog)]
                    ; The program was run and it threw an exception, return false.
                    [else
                     #f])))))
      ; Invoke the run-program function for each program in the given list.  Returns the
      ; first non false value.
      (ormap run-revo-program (program-list-programs revo-prog-list))))

