#lang racket
  (provide (all-defined-out))
  
  (define (fatal logger string-expr [data null])
    (logger-log logger 'fatal string-expr data))
  
  (define (error logger string-expr [data null])
    (logger-log logger 'error string-expr data))

  (define (warning logger string-expr [data null])
    (logger-log logger 'warning string-expr data))
  
  (define (info logger string-expr [data null])
    (logger-log logger 'info string-expr data))
  
  (define (debug logger string-expr [data null])
    (logger-log logger 'debug string-expr data))
  
  (define (logger-log logger level string-expr [data null])
    (when (and (logger? logger)
               (string? string-expr)
               (log-level? logger level))
      (let ((str (if (null? data)
                     string-expr
                     (let ((o (open-output-string)))
                       (write data o)
                       (string-append
                        string-expr
                        (get-output-string o))))))
        (log-message logger level str
                     (current-continuation-marks)))))

