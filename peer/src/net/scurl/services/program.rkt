#lang racket
  
  (require "depends.rkt"
           
           "../peer-validation/host-id.rkt"
           "../peer-validation/scurl.rkt")
  
  (provide
   (struct-out program)
   program=?
   
   (struct-out program-list)
   new-program-list
   
   add-program
   remove-program
   run-program
   
   ; The functions below are provided for unit testing.
   match-prefix?
   strip-prefix
   match-filter?
   match-exclude?)
  
  ; Create the logger for the program module.
  (define logger (make-logger 'program-logger services-parent-logger))
  
  ; A program is the storage utility used for the revocation, certification, forward proxy and reverse proxy
  ; modules.
  ; The id is a numberic value that is used to remove the program.
  ; The filter and exclude are regular expressions that are matched against the full scurl.
  ; The procedure is either a function which is run or a value to be returned when the pattern
  ; matching is successfull.
  ; id - exact-positive-integer?
  ; block - boolean?
  ; filter - (or boolean? regexp?)
  ; exclude - (or boolean? regexp?)
  ; proc - procedure?
  (struct program (id prefix filter exclude proc))
  
  ; Returns true when the two given items are both programs with the same state.
  ;
  ; any any -> boolean?
  (define (program=? p1 p2)
    (and
     (program? p1)
     (program? p2)
     (= (program-id p1)
        (program-id p2))
     (equal? (program-prefix p1)
             (program-prefix p2))
     (equal? (program-filter p1)
             (program-filter p2))
     (equal? (program-exclude p1)
             (program-exclude p2))
     (equal? (program-proc p1)
             (program-proc p2))))
  
  ; Provides a structure that encapsulates all registered programs and the next
  ; identifier used when adding a revocation program.
  ; programs - listof program?
  ; next-id - exact-positive-integer?
  (struct program-list (programs next-id))
  
  ; Creates and returns a new program list with no registered programs.
  ; -> program-list
  (define (new-program-list)
    (program-list '() 1))
  
  ; Uses the given values to create a program and add it to the given program list.
  ; The filter and exclude values all default to false which will disable them.
  ; Returns the id of added program and a new program-list which contains the new
  ; list of registered programs.
  ;
  ; (listof program?)
  ; (or procedure? any)
  ; #:prefix string?
  ; #:filter string?
  ; #:exclude string?
  ; -> (values id program-list)
  (define (add-program programs procedure #:prefix [prefix #f] #:filter [filter #f] #:exclude [exclude #f])
    ; Compile the filter and exclude into regular expressions.
    (letrec ((str-prefix (if (string? prefix) prefix #f))
             (reg-filter (if (string? filter) (regexp filter) #f))
             (reg-exclude (if (string? exclude) (regexp exclude) #f))
             (id (program-list-next-id programs))
             (prog (program id str-prefix reg-filter reg-exclude procedure)))
      ; Everything checks out, make a program and add it to the list.
      (program-list (append (program-list-programs programs) (list prog)) (add1 id))))
  
  ; Removes the program with the given id.
  ;
  ; exact-positive-integer? program-list? -> program-list?
  (define (remove-program id programs)
    (program-list
     ; Remove the program that is associated with the given id.
     (remove
      id
      (program-list-programs programs)
      (lambda (id program) (= id (program-id program))))
     ; Keep the old next-id, once we assign an id it is gone forever.
     (program-list-next-id programs)))
  
  ; Attempts to match the given name against the prefix regular expression in the given
  ; program
  ;
  ; string? program? -> boolean?
  (define (match-prefix? name program)
    (let ((prefix (program-prefix program)))
      ; If the filter is a boolean, let it pass, otherwise try to match it.
      (if (not (string? prefix))
          ; No prefix, so automatically passes.
          #t
          ; See if the prefix matches the start of the name string.
          (and
           (> (string-length name)
              (string-length prefix))
           (string=?
            (string-downcase (substring name 0 (string-length prefix)))
            (string-downcase prefix))))))
  
  ; Returns the name argument without the matched prefix.  If the program
  ; doesn't contain a prefix then the name will be returned and if the
  ; name argument is shorter than the prefix an empty string will be returned.
  ;
  ; string? cert-program? -> string?
  (define (strip-prefix name program)
    (let ((prefix (program-prefix program)))
      (if (not (string? prefix))
          ; No prefix, so return name.
          name
          ; Strip the length of prefix from the beginning of name.
          (if (> (string-length name)
                 (string-length prefix))
              (substring name (string-length prefix))
              ; Don't have enough to do a substring, shouldn't ever be here.
              ""))))
  
  ; Attempts to match the given name against the filter regular expression in the given
  ; program
  ;
  ; string? program? -> boolean?
  (define (match-filter? name program)
    (let ((filter (program-filter program)))
      ; If the filter is a boolean, let it pass, otherwise try to match it.
      (if (regexp? filter)
          (regexp-match-exact? filter name)
          ; No filter, so automatically passes.
          #t)))
  
  ; Attempts to match the given name against the exclude regular expression in the given
  ; program.
  ;
  ; string? program? -> boolean?
  (define (match-exclude? name program)
    (let ((exclude (program-exclude program)))
      (if (regexp? exclude)
          (regexp-match-exact? exclude name)
          ; No exclude, so make sure it doesn't match.
          #f)))
  
  ; Attempts to run the given program against the given name.  If the given program
  ; contains a procedure then the procedure will be run, otherwise the procedure
  ; value will be returned.  If a procedure is run the third parameter arg will be
  ; passed to the procedure.
  ;
  ; program? string? any -> any
  (define (run-program prog name arg)
    ; Match the prefix.
    (if (not (match-prefix? name prog))
        ; There was a prefix and it did not match, return false.
        (list prog 'not-run)
        ; The prefix did match, strip the prefix from name and continue.
        (let ((name (strip-prefix name prog)))
          ; Ensure that the filter matches.
          (if (not (match-filter? name prog))
              ; The filter did not match, return false.
              (list prog 'not-run)
              ; The filter did match.  Check the exclude.
              (if (match-exclude? name prog)
                  ; Matched on the exclude, return false.
                  (list prog 'not-run)
                  ; The exclude did not match, Run the procedure.
                  (let (; Grab the procedure to run.
                        (proc (program-proc prog)))
                    
                    ; If the stored procedure is a procedure then run it and return the result, otherwise
                    ; just return the procedure.
                    (if (not (procedure? proc))
                        ; Not a procedure, just return whatever it is.
                        (list prog proc)
                        ; Let's run the procedure with the host-id as the argument.
                        (let ((output (with-handlers ((exn?
                                                       (lambda (e)
                                                         (debug logger
                                                                (string-append
                                                                 "Exception occured while running the program ("
                                                                 (number->string (program-id prog))
                                                                 ") procedure.")
                                                                e)
                                                         ; Return the exception that occurred.
                                                         e)))
                                        ; Run and return the procedures result.
                                        (proc arg))))
                          (list prog output)))))))))

