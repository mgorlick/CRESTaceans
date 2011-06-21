(import threading)

(define *MUTEX* (mutex/new))

(define (log message)
  (mutex/lock! *MUTEX*)
  (write message)
  (newline)
  (newline)
  (mutex/unlock! *MUTEX*))

(define (queue-test-1)
  (let* ((q1 (make <queue-1-n>))

	 (thunk1
	  (lambda ()
	    (let* ((start (%now))
		   (outcome (take! q1 5000 'nothing-found)) ; Wait 5 seconds total.
		   (end (%now))
		   (elapsed (duration-to-milliseconds (time-difference end start))))
	      (log (list 'thunk1 'outcome: outcome 'elapsed: elapsed)))))

	 (thunk2
	  (lambda ()
	    (let* ((start (%now))
		   (junk (snooze 1000))
		   (outcome (put! q1 999))
		   (end (%now))
		   (elapsed (duration-to-milliseconds (time-difference end start))))
	      (log (list 'thunk2 'outcome: outcome 'elapsed: elapsed)))))

	 (f1 (make <fiber> thunk1))
	 (f2 (make <fiber> thunk2)))
    (start f1)
    (start f2)))

(define (queue-test-2)
  (let* ((q1 (make <queue-1-n>))
	 (filter (lambda (x) (even? x)))

	 (thunk1
	  (lambda ()
	    (let* ((start (%now))
		   (outcome (take! q1 filter 5000 'nothing-found)) ; Wait 5 seconds total.
		   (end (%now))
		   (elapsed (duration-to-milliseconds (time-difference end start))))
	      (log (list 'thunk1 'outcome: outcome 'elapsed: elapsed))
	      (log (list 'thunk1 'q1: (queue-to-list q1))))))

	 (thunk2
	  (lambda ()
	    (let loop ((i 1))
	      (let* ((start (%now))
		     (junk (snooze (* i 100)))
		     (outcome (put! q1 999))
		     (end (%now))
		     (elapsed (duration-to-milliseconds (time-difference end start))))
		(log (list 'thunk2 'outcome: outcome 'elapsed: elapsed))
		(if (< i 6) (loop (+ i 1)))))))

	 (f1 (make <fiber> thunk1))
	 (f2 (make <fiber> thunk2)))
    (start f1)
    (start f2)))


(define (queue-test-3)
  (let* ((q1 (make <queue-1-n>))
	 (filter (lambda (x) (even? x)))

	 (thunk1
	  (lambda ()
	    (let* ((start (%now))
		   (outcome (take! q1 filter 5000 'nothing-found)) ; Wait 5 seconds total.
		   (end (%now))
		   (elapsed (duration-to-milliseconds (time-difference end start))))
	      (log (list 'thunk1 'outcome: outcome 'elapsed: elapsed))
	      (log (list 'thunk1 'q1: (queue-to-list q1))))))

	 (thunk2
	  (lambda ()
	    (let loop ((i 1))
	      (let* ((start (%now))
		     (junk (snooze (* i 100)))
		     (outcome (put! q1 (if (< i 5) 999 (* i 100))))
		     (end (%now))
		     (elapsed (duration-to-milliseconds (time-difference end start))))
		(log (list 'thunk2 'outcome: outcome 'elapsed: elapsed))
		(if (< i 6)
		    (loop (+ i 1))
		    (log (list 'thunk1 'q1: (queue-to-list q1))))))))

	 (f1 (make <fiber> thunk1))
	 (f2 (make <fiber> thunk2)))
    (start f1)
    (start f2)))
