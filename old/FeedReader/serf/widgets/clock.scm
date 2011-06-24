(define (thunk/clock)
  (define :tick: 3000) ; Period between ticks in milliseconds.

  (let ((links '())
	(n 0))

    (let loop ((m (? (this-mailbox :tick: 'tick))))
      (match
       (message/path+body m)

       (#(link/create ,mailbox)
	(set! links (cons mailbox links))
	(! mailbox (@ mailbox 'clock/tick) n))

       (tick
	(for-each
	 (lambda (link) (! (@ link 'clock/tick) n))
	 links)
	(set! n (+ n 1)))

       (,_ignore #f))
      (loop (? (this-mailbox :tick: 'tick))))))


