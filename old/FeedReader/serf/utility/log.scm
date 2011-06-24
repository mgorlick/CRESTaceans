;; Copyright 2009 Michael M. Gorlick

;; Apache logging classes.
(define-java-class <java-apache-logger>                |org.apache.log4j.Logger|)
(define-java-class <java-apache-logger-level>          |org.apache.log4j.Level|)
(define-java-class <java-apache-logger-pattern-layout> |org.apache.log4j.PatternLayout|)
(define-java-class <java-apache-logger-file-appender>  |org.apache.log4j.FileAppender|)

;; Null pointers.
(define :java-null-apache-logger:)
(define :java-null-apache-logger-level:)

;; Internal constants.
(define :logger-format: "%-5p %d{ISO8601} %m%n") ; LEVEL ISO8601-date-time message line-separator
(define :logger-layout:) ; Default layout for all logger instances.

;; Java methods.
(define-generic-java-method to-level)
(define-generic-java-method get-level)
(define-generic-java-method set-level)
(define-generic-java-method get-root-logger)
(define-generic-java-method get-logger)
(define-generic-java-method add-appender)
(define-generic-java-method trace)
(define-generic-java-method debug)
(define-generic-java-method info)
(define-generic-java-method warn)
(define-generic-java-method error)
(define-generic-java-method fatal)
(define-generic-java-method hash-code)

;; Returns a logger instance that logs into the file named by filename.
;; id: a string or symbol to uniquely identfy the logger instance
;; filename: a string or symbol naming the log file for this logger instance
;; By Serf convention, the filename is host:port, for example,
;; "crest.ics.uci.edu:1951" or |www.example.org:9821|, or "198.162.15.119:1969".
;; By default the log level is set to info.
(define (log/new id filename)
  (let ((instance (log/get id)) ; Log is created if necessary.
	(file-appender
	 (java-new <java-apache-logger-file-appender> :logger-layout: (->jstring filename))))
    (add-appender instance file-appender)
    (log/level! instance 'info)
    instance))

;; Returns the level of the log as a symbol (one of [off, trace, debug, warn, error, fatal, all])
;; if the log level has been set and #f otherwise.
(define (log/level log)
  (let ((n (get-level log))) ; get-level returns an org.apache.log4j.Level instance.
    (if (java-null? n) #f (string->symbol (string-downcase (java-to-string n))))))

;; Set the log to the level specified where s is either
;; a symbol [off, trace, debug, info, warn, error, fatal, all]
;; or a string ["off", "trace", "debug", "info", "warn", "error", "fatal", "all"].
(define (log/level! log level)
  (set-level
   log
   ; to-level maps a string or symbol to an org.apache.log4j.Level instance.
   (to-level :java-null-apache-logger-level: (->jstring level)))
  log)

;; Returns the root log.
(define (log/root) (get-root-logger :java-null-apache-logger:))

;; Returns the log denoted by string or symbol s, creating it if necessary.
(define (log/get s) (get-logger :java-null-apache-logger: (->jstring s)))
    
;; The individual logging functions by level.
(define (log/trace log message)
  (and log (trace log (->jstring message)) #t))

(define (log/debug log message)
  (and log (debug log (->jstring message)) #t))

(define (log/info log message)
  (and log (info log (->jstring message)) #t))

(define (log/warn log message)
  (and (warn log (->jstring message)) #t))

(define (log/error log message)
  (and (error log (->jstring message)) #t))

(define (log/fatal log message)
  (and log (fatal log (->jstring message)) #t))

(define log/audit) ; Synonym for log/info.

;; Translate simple values to a printable string.
; (define (always-string x)
;   (cond
;    ((string? x) x)
;    ((symbol? x) (symbol->string x))
;    ((number? x) (number->string x))
;    ((boolean? x) (if x "true" "false"))
;    (else "<unknown>")))

;; Returns a substring that is, at most, the first m characters of s.
(define (string-at-most s m)
  (let ((n (string-length s)))
    (if (<= n m) s (substring s 0 m))))

;; Remove the package name from the string representation of a Java object instance o.
;; For example, org.apache.http.protocol.SyncBasicHttpContext@afe17b => SyncBasicHttpContext@afe17b.
;; Returns the abbreviation as a Scheme string.
(define (snip-package-name o)
  (let* ((s (java-to-string o))
	 (i (string-index-right s #\.)))
    (if i (substring s (+ i 1) (string-length s)) s)))

;; Returns an integer that is (more or less) a unique id for a Java object x.
(define (java-object-to-id x) (->number (hash-code x)))

(set! :java-null-apache-logger:       (java-null <java-apache-logger>))
(set! :java-null-apache-logger-level: (java-null <java-apache-logger-level>))
(set! :logger-layout: (java-new <java-apache-logger-pattern-layout> (->jstring :logger-format:)))
(set! log/audit log/info) ; Synonym.
