;; Copyright 2009 Michael M. Gorlick

(define (now/utc) (current-time time-utc))

(define (now/monotonic) (current-time time-monotonic))

;; "2009-07-09T14:30:51Z" for example.
;; DEPRECATED. Use utc/iso8601 instead.
(define (utc-to-iso8601 t)
  (date->string (time-utc->date t 0) "~4"))

(define (utc/iso8601 t)
  (date->string (time-utc->date t 0) "~4"))

;; DEPRECATED. Use utc/string instead.
(define (utc-to-string t) (utc-to-iso8601 t))

(define (utc/string t) (utc/iso8601 t))

;; Convert m in milliseconds to a SRFI-19 time-duration.
(define (milliseconds-to-duration m)
  (let ((seconds (quotient m 1000))
	(nanoseconds (* (remainder m 1000) 1000000)))
    (make-time time-duration nanoseconds seconds)))

;; Convert a SRFI-19 time duration d to integer milliseconds.
(define (duration-to-milliseconds d)
  (+ (* (time-second d) 1000) (quotient (time-nanosecond d) 1000000)))


