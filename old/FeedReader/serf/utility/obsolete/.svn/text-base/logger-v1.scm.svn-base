(import s2j)
(import generic-procedures)
(import type-system)

(require-library 'sisc/libs/srfi/srfi-13)
(import srfi-13) ; String library.

(define-java-class <java-apache-logger>                |org.apache.log4j.Logger|)
(define-java-class <java-apache-logger-level>          |org.apache.log4j.Level|)
(define-java-class <java-apache-logger-pattern-layout> |org.apache.log4j.PatternLayout|)
(define-java-class <java-apache-logger-file-appender>  |org.apache.log4j.FileAppender|)

(define %APACHE-LOGGER       (java-null <java-apache-logger>))
(define %APACHE-LOGGER-LEVEL (java-null <java-apache-logger-level>))

(define SERF:LOGGER:FORMAT "%-5p %d{ISO8601} %m%n") ; LEVEL ISO8601-date-time message line-separator
(define SERF:LOGGER:LAYOUT (java-new <java-apache-logger-pattern-layout> (->jstring SERF:LOGGER:FORMAT)))

(define-generic-java-methods
  (java-logger-level! |setLevel|)
  (java-root-logger   |getRootLogger|)
  (java-get-logger    |getLogger|)
  (java-add-appender! |addAppender|)
  (java-trace         |trace|)
  (java-debug         |debug|)
  (java-info          |info|)
  (java-warn          |warn|)
  (java-error         |error|)
  (java-fatal         |fatal|))

(define-generic-java-field-accessors
  (:off   |OFF|)
  (:trace |TRACE|)
  (:debug |DEBUG|)
  (:info  |INFO|)
  (:warn  |WARN|)
  (:error |ERROR|)
  (:fatal |FATAL|)
  (:all   |ALL|))

;; Returns the root logger.
(define (serf-root-logger) (java-root-logger %APACHE-LOGGER))
;; Returns the logger denoted by string or symbol s.
(define (serf-logger s) (java-get-logger %APACHE-LOGGER (->jstring s)))

;; Logging levels.
(define SERF:LOGGER:OFF   (:off   %APACHE-LOGGER-LEVEL))
(define SERF:LOGGER:TRACE (:trace %APACHE-LOGGER-LEVEL))
(define SERF:LOGGER:DEBUG (:debug %APACHE-LOGGER-LEVEL))
(define SERF:LOGGER:AUDIT (:info  %APACHE-LOGGER-LEVEL)) ; A synonym for coarse-grained logging.
(define SERF:LOGGER:INFO  (:info  %APACHE-LOGGER-LEVEL)) ; Coarse-grained logging.
(define SERF:LOGGER:WARN  (:warn  %APACHE-LOGGER-LEVEL))
(define SERF:LOGGER:ERROR (:error %APACHE-LOGGER-LEVEL))
(define SERF:LOGGER:FATAL (:fatal %APACHE-LOGGER-LEVEL))
(define SERF:LOGGER:ALL   (:all   %APACHE-LOGGER-LEVEL))

;; Map a symbol (off, trace, debug, audit, warn, error, fatal, all) to a log4j logger level.
;; Note that audit is a synonym for info.
(define (to-logger-level s)
  (case s
    ((off)   SERF:LOGGER:OFF)
    ((trace) SERF:LOGGER:TRACE)
    ((debug) SERF:LOGGER:DEBUG)
    ((audit info) SERF:LOGGER:INFO)
    ((warn)  SERF:LOGGER:WARN)
    ((error) SERF:LOGGER:WARN)
    ((fatal) SERF:LOGGER:FATAL)
    ((all)   SERF:LOGGER:ALL)
    (else    SERF:LOGGER:DEBUG))) ; Default if we don't recognize the level s.
     
;; Set the given logger to the level specified by symbol s (one of
;; off, trace, debug, info, warn, error, fatal, or all).
(define (serf-logger-level! logger s)
  (java-logger-level! logger (to-logger-level s)))

;; In the routines below an authority is a cons cell (host . port) where host is
;; either a DNS name (string) or an IP address (string) and port is a natural number.
;; For example ("www.foo.org" . 3322) or ("192.168.12.95" . 8181).

;; Translate simple values to a printable string.
(define (always-string x)
  (cond
   ((string? x) x)
   ((symbol? x) (symbol->string x))
   ((number? x) (number->string x))
   ((boolean? x) (if x "true" "false"))
   (else #f)))

;; Simple constructor for an authority "object".
;; Returns an authority on success and #f on failure.
(define (make-authority host port)
  (let ((h (string-downcase (always-string host)))) ; Normal form for DNS host names.
    (and (integer? port) (>= port 0) (cons h port))))

(define (make-authority-as-string host port)
  (let ((h (always-string host))
	(p (and (integer? port) (>= port 0))))
    (if (and h p)
	(string-append (string-downcase h) ":" (number->string p))
	#f)))

;; Translate an authority ("www.foo.org" . 3322) to a string "www.foo.org:3322".
(define (authority-to-string authority)
  (let ((host (always-string (car authority)))  ; Insurance.
	(port (always-string (cdr authority)))) ; Insurance again.
    (string-append host ":" port)))

;; Translates a string "www.foo.org:3322" to an authority ("www.foo.org" . 3322)
(define (string-to-authority s)
  (let* ((i (string-index-right (string-trim-both s) #\:)) ; Find the ":" in host:port.
	 (port (and i (substring s (+ i 1) (string-length s)))) ; Extract the port.
	 (host (and i (substring s  0 i))))                     ; Extract the host.
    (if (and port host) (make-authority port (string->number port)))))

;; Return the logger for the given authority.
(define (authority-to-logger authority)
  (serf-logger (authority-to-string authority)))

;; Serf adopts the convention of one log file per authority with all threads
;; executing on behalf of the authority logging into the same file.
;; Create a log4j file appender for the given authority.
;; The file name will be host:port for authority (host . port).
(define (authority-to-file-appender authority)
   (java-new
    <java-apache-logger-file-appender>
    SERF:LOGGER:LAYOUT
    (->jstring (authority-to-string authority))))

;; Make a shared logger for the given authority.
(define (make-logger-for-authority authority)
  (let ((logger (authority-to-logger authority))
	(file-appender (authority-to-file-appender authority)))
    (java-add-appender! logger file-appender)
    (java-logger-level! logger SERF:LOGGER:AUDIT)
    logger))

;; The individual logging functions by level.
(define (serf-trace logger message)
  (and logger (java-trace logger (->jstring message)) #t))
(define (serf-debug logger message)
  (and logger (java-debug logger (->jstring message)) #t))
(define (serf-info logger message)
  (and logger (java-info logger (->jstring message)) #t))
(define (serf-warn logger message)
  (and logger (java-warn logger (->jstring message)) #t))
(define (serf-error logger message)
  (and logger (java-error logger (->jstring message)) #t))
(define (serf-fatal logger message)
  (and logger (java-fatal logger (->jstring message)) #t))
(define serf-audit serf-info) ; Synonym.


