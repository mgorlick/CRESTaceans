#lang racket/base

(require racket/contract racket/list)
  
  (require "depends.rkt"
           "program.rkt"
           
           "../peer-validation/scurl.rkt")
  
  (provide
   (rename-out [add-program add-rproxy-program]
               [remove-program remove-rproxy-program])
   
   run-rproxy-programs)
  
  ; Create the logger for the certification module.
  (define logger (make-logger 'reverse-proxy-logger services-parent-logger))
  
  ; Runs the given rproxy list against the given scurl using the msg as the argument to the
  ; procedure for each applied program.  If a procedure is run and returns anything other than
  ; false then it will be assumed that the message was handled by the reverse proxy handler.
  ; 
  ; program-list? scurl? any -> (or boolean? any)
  (define (run-rproxy-programs rproxy-prog-list scurl msg)
    (letrec ((name (url->string (scurl-url scurl)))
             (run-rproxy-program
              (lambda (prog)
                (letrec ((output-list (run-program prog name msg))
                         (prog-out (first output-list))
                         (output (last output-list)))
                  (cond
                    ; Not run, return false.
                    [(equal? 'not-run output) #f]
                    ; If the output is false then it wasn't handled and the ormap will continue
                    ; to run, anything else will cause the ormap to complete and will signal
                    ; that the message has been handled.
                    [else output])))))
      ; Invoke the run-program function for each program in the given list.  Returns the
      ; first non false value.
      (ormap run-rproxy-program (program-list-programs rproxy-prog-list))))

