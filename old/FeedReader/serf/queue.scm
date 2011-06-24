;; Copyright 2009 Michael M. Gorlick

(define-nongenerative-record-type <queue-posting> |563130a3-0678-4a9d-b521-57289b05a62d|
  (make-queue-posting before after posting)
  <queue-posting>?
  (before  :before  :before!)
  (after   :after   :after!)
  (posting :posting :posting!))

(define-generic :head)
(define-generic :head!)
(define-generic :tail)
(define-generic :tail!)
(define-generic :cursor)
(define-generic :cursor!)
(define-generic :capacity)
(define-generic :capacity!)
(define-generic :occupancy)
(define-generic :occupancy!)
(define-generic :more)
(define-generic :more!)
(define-generic :busy)
(define-generic :busy!)
(define-generic :store)
(define-generic :store!)

;; Queue methods.
(define-generic queue/new)
(define-generic queue/empty?)
(define-generic queue/nonempty?)
(define-generic queue/full?)
(define-generic queue/capacity)
(define-generic queue/occupancy)
;(define-generic queue/push!)
(define-generic queue/put!)
(define-generic queue/take!)
(define-generic queue/rewind!)
(define-generic queue/advance!)
(define-generic queue/peek)
(define-generic queue/extract!)
(define-generic queue/to-list)

;; General-purpose queue (not thread-safe).
(define-nongenerative-class (<queue>) |e19ed8de-1766-4302-9569-78ee75b5424d|
  (head :head :head!)  ; Queue head.
  (tail :tail :tail!)  ; Queue tail.
  (cursor :cursor :cursor!) ; Reading cursor.
  (capacity :capacity :capacity!) ; Maximum capacity of queue (#f => unbounded capacity).
  (occupancy :occupancy :occupancy!)) ; Number of items in queue.

;; General-purpose thread-safe queue.
(define-nongenerative-class (<thread-safe-queue>) |912e231b-2b61-4dc1-9a30-f749a109ee66|
  (store :store :store!) ; <queue> instance.
  (busy :busy :busy!)    ; Mutex.
  (more :more :more!))   ; Condition variable.

;; Utility routines.
(define (%empty? q)
  (not (:head q)))

;; Returns #t if queue is nonempty and #f otherwise.
(define (%nonempty? q)
  (and (:head q) #t))

;; Returns #t if the queue is full and #f otherwise.
(define (%full? q)
  (and
   (:capacity q)
   (>= (:occupancy q) (:capacity q))))

(define (%put! q x)
  (cond
   ((%full? q) #f) ; Queue is full.

   ((:tail q) ; Queue is nonempty. Add to tail.
    (:after! (:tail q) (make-queue-posting (:tail q) #f x))
    (:tail! q (:after (:tail q)))
    (:occupancy! q (+ (:occupancy q) 1))
    #t)

   (else ; Queue is empty. Add to head.
    (:head! q (make-queue-posting #f #f x))
    (:tail! q (:head q))
    (:occupancy! q (+ (:occupancy q) 1))
    #t)))

;; Remove and return the oldest (first) item in the queue.
;; If the queue is empty return the given default value.
(define (%take! q default)
  (%rewind! q)
  (if (%advance! q)
      (%extract! q)
      default))

;; Rewind the queue cursor.
(define (%rewind! q)
  (:cursor! q #f))

;; Advance the cursor to the next item in the queue.
;; Return #t if the cursor advanced and #f otherwise.
(define (%advance! q)
  (cond
   ((boolean? (:cursor q)) ; Cursor was rewound.
    (:cursor! q (:head q)) ; Set cursor to first item (if any).
    (and (:cursor q) #t))

   ((eq? (:cursor q) (:tail q)) ; Cursor pointing to last <queue-posting>.
    #f)

   (else ; Cursor somewhere in the middle of the queue. Advance cursor.
    (:cursor! q (:after (:cursor q)))
    #t)))

;; Return the item referenced by the cursor without disturbing its position in the queue.
;; If the cursor is not positioned at an item then the void value is returned.
(define (%peek q)
  (if (:cursor q)
      (:posting (:cursor q))))

;; Remove and return the item referenced by the cursor.
;; If the cursor is not positioned at a posting then the void value is returned.
(define (%extract! q)
  (if (:cursor q)
      (let* ((posting   (:cursor q))       ; The posting of interest.
	     (prior     (:before posting)) ; The posting immediately before the posting of interest.
	     (successor (:after posting))) ; The posting immediately after the posting of interest.
	(cond
	 ((and (boolean? prior) (boolean? successor))
	  ; The posting is the only one in the queue.
	  (:head! q #f)
	  (:tail! q #f))

	 ((boolean? prior)
	  ; The posting of interest is first in the queue.
	  (:head!   q successor)
	  (:before! successor #f)
	  (:after!  posting #f))

	 ((boolean? successor)
	  ; The message of interest is last in the queue.
	  (:tail!  q prior)
	  (:after! prior #f)
	  (:before! posting #f))

	 (else
	  ; The message is somewhere in the middle of the queue.
	  (:after!  prior successor)
	  (:before! successor prior)
	  (:before! posting #f)
	  (:after!  posting  #f)))

	(:cursor! q #f) ; Rewind the cursor.
	(:occupancy! q (- (:occupancy q) 1)) ; Decrement the backlog count.
	(:posting posting))))

;; Remove and return the first item i in the queue for which (f i) is true.
;; If no such item i exists then return default.
(define (%take-with-filter! q f default)
  (%rewind! q)
  (let loop ()
    (if (%advance! q)
	(if (f (%peek q))
	    (%extract! q)
	    (loop))
	default)))

;; Return a list containing the queue items in arrival order.
(define (%queue-to-list q)
  (%rewind! q)
  (let loop ((items '()))
    (if (%advance! q)
	(loop (cons (%peek q) items))
	(reverse items))))



;; Outline
;;   rewind the queue cursor
;;   try take and filter with no rewind
;;   if success then return item
;;   if failure then wait

;; q - queue-1-n of interest
;; f - predicate
;; used - time used so far as a time-duration
;; timeout - total time allotted for extraction from queue expressed as a time-duration
;; default - value returned on failure (no item satisfying predicate f found in timeout time)
;; Either returns item from q satisfying predicate f or default. In both cases will return no later
;; than timeout time-duration from call.
(define (%subtake! q f used timeout default)
  ; Debugging
  ;(write (list '%subtake! 'used: (duration-to-milliseconds used) 'timeout: (duration-to-milliseconds timeout)))
  ;(newline) (newline)

  (if (time>? used timeout) ; Have we consumed all of the time allocated by timeout?

      default ; The total amount of time that we have waited exceeds the timeout.

      (let* ((unused (time-difference timeout used))    ; Calculate the remaining (unused) wait time.
	     (d      (duration-to-milliseconds unused)) ; Convert from time-duration to milliseconds.
	     (start (now/monotonic)))

	; Debugging.
	;(write (list '%subtake! 'unused: d))
	;(newline) (newline)
	;(write (list '%subtake! 'backlog: (backlog q))) (newline)(newline)

	(if (> d 0)
	    (cond
	     ((mutex/unlock! (:busy q) (:more q) d) ; Wait for something tasty to arrive.
	      ;(write (list '%subtake! 'emerged-from-wait)) (newline) (newline)
	      (mutex/lock! (:busy q)) ; One or more items were added to the queue.

	      (let loop ()
		(if (%advance! q)
		    (if (f (%peek q))
			(%extract! q) ; Return the item that satisfied predicate f.
			(loop))     ; Advance to the next item (if any) and try again.

		    ; Oops. Nothing satisfied the filter and we have exhausted the queue so try waiting again.
		    (%subtake! q f (add-duration! (time-difference (now/monotonic) start) used) timeout default))))

	     (else default)) ; We waited for the entire "unused" period. By construction we exceeded the timeout.

	    default)))) ; No more time to wait.

;; NOTE: Timeouts in Termite (recv ...) violate the law of least surprise since
;; the (recv ...) should EITHER find a matching message in no more than timeout seconds (expressed as a real).
;; or return. However careful reading of the code for Termite (see recv.scm) shows that if messages
;; repeatedly arrive before the timeout expires but are not matched by (recv ...) then the (recv ...)
;; will just wait again for timeout seconds. This could easily lead to either indefinite blocking
;; (as long as non-matching messages continue to arrive) or eventual success but whose total elapsed time
;; exceeds the timeout.

;; Constructors for <queue> and <thread-safe-queue>.
;; Note that the generic procedure is implicitly declared by the (define-nongenerative-class ...) above.
(add-methods
 initialize
 (list
  ; Unbounded queue.
  (method
   ((<queue> q))
   (:head! q #f)
   (:tail! q #f)
   (:cursor! q #f)
   (:capacity! q #f) ; Infinite capacity.
   (:occupancy! q 0))  ; Empty.

  ; Bounded queue with maximum capacity n > 0.
  (method
   ((<queue> q) (<number> n))
   (:head! q #f)
   (:tail! q #f)
   (:cursor! q #f)
   (:capacity! q n)   ; Capacity = n >=0.
   (:occupancy! q 0)) ; Empty.

  ; Unbounded capacity 1:n (single reader/multiple writers) thread-safe queue.
  (method
   ((<thread-safe-queue> q))
   (:store! q (make <queue>))
   (:busy!  q (mutex/new))
   (:more!  q (condvar/new)))

  ; Bounded capacity n > 0 (single reader/multiple writers) thread-safe queue.
  (method
   ((<thread-safe-queue> q) (<number> n))
   (:store! q (make <queue> n))
   (:busy!  q (mutex/new))
   (:more!  q (condvar/new)))))

(add-methods
 queue/new
 (list
  (method
   ((<boolean> safe))
   (if safe (make <thread-safe-queue>) (make <queue>)))

  (method
   ((<boolean> safe) (<number> n))
   (if safe (make <thread-safe-queue> n) (make <queue> n)))))

;; Returns #t if the queue is empty and #f if items are enqueued.
(add-methods
 queue/empty?
 (list
  (method ((<queue> q)) (%empty? q))

  (method
   ((<thread-safe-queue> q))
   (mutex/lock! (:busy q))
   (let ((outcome (%empty? (:store q))))
     (mutex/unlock! (:busy q))
     outcome))))

;; Returns #t if items are enqueued and #f if the queue is empty.
(add-methods
 queue/nonempty?
 (list
  (method ((<queue> q)) (%nonempty? q))

  (method
   ((<thread-safe-queue> q))
   (mutex/lock! (:busy q))
   (let ((outcome (%nonempty? (:store q))))
     (mutex/unlock! (:busy q))
     outcome))))

;; Returns #t if the backlog of waiting messages equals the queue capacity
;; and #f if the backlog is less than the queue capacity.
(add-methods
 queue/full?
 (list
  (method ((<queue> q)) (%full? q))

  (method
   ((<thread-safe-queue> q))
   (mutex/lock! (:busy q))
   (let ((outcome (%full? (:store q))))
     (mutex/unlock! (:busy q))
     outcome))))

;; Returns the n > 0 capacity of a queue. Returns #f if the queue is unbounded.
(add-methods
 queue/capacity
 (list
  (method ((<queue> q)) (:capacity q))
  (method ((<thread-safe-queue> q)) (:capacity (:store q)))))

;; Returns the number of elements in the queue.
(add-methods
 queue/occupancy
 (list
  (method ((<queue> q)) (:occupancy q))

  (method
   ((<thread-safe-queue> q))
   (mutex/lock! (:busy q))
   (let ((outcome (:occupancy (:store q))))
     (mutex/unlock! (:busy q))
     outcome))))

;; Enqueue item x. Returns #t if successful (sufficient capacity)
;; and #f otherwise (insufficient capacity).
(add-methods
 queue/put!
 (list
  (method ((<queue> q) (<value> x)) (%put! q x))

  (method
   ((<thread-safe-queue> q) (<value> x))
   (mutex/lock! (:busy q))
   (let ((ok (%put! (:store q) x)))
     (if ok (condvar/notify (:more q)))
     (mutex/unlock! (:busy q))
     ok))))

;; Extract and return the oldest item in the queue.
;; If the queue is empty return the given default value.
(add-methods
 queue/take!
 (list

  (method ((<queue> q) (<value> default)) (%take! q default))

  ; Nonblocking extraction from queue.
  (method
   ((<thread-safe-queue> q) (<value> default))
   (mutex/lock! (:busy q))
   (let ((outcome (%take! (:store q) default)))
     (mutex/unlock! (:busy q))
     outcome))

  ; Blocking extraction from queue.
  (method
   ((<thread-safe-queue> q))
   (let loop ((s (:store q)))
     (mutex/lock! (:busy q))
     (%rewind! s)
     (cond
      ((%advance! s)
       (let ((item (%extract! s)))
	 (mutex/unlock! (:busy q))
	 item))
      ((mutex/unlock! (:busy q) (:more q)) ; Block until something shows up.
       (loop s)))))

  ; Blocking extraction from queue with timeout.
  ; Block waiting, at most timeout milliseconds, for an item from the queue and return.
  ; If the timeout expires before an item becomes available return the default.
  (method
   ((<thread-safe-queue> q) (<value> default) (<number> timeout))
   (let loop ((s (:store q)))
     (mutex/lock! (:busy q))
     (%rewind! s)
     (cond
      ((%advance! s)
       (let ((item (%extract! s)))
	 (mutex/unlock! (:busy q))
	 item))
      ((mutex/unlock! (:busy q) (:more q) timeout)
       (loop s))
      (else default))))

  ; Remove and return the first item in arrival order that satisfies predicate f.
  ; Blocks if necessary until such an item is found.
  (method
   ((<thread-safe-queue> q) (<procedure> f))
   (mutex/synchronize

    (:busy q)

    (lambda ()
      (%rewind! (:store q))
      (let loop ((s (:store q)))
	(cond
	 ((%advance! s)
	  (if (f (%peek s)) (%extract! s) (loop)))
	 (else
	  (mutex/unlock! (:busy q) (:more q)) ; Blocking wait for something else to arrive.
	  (mutex/lock! (:busy q)) ; Reacquire the lock.
	  (loop s)))))))

  ; Remove and return the first item in arrival order that satisfies predicate f.
  ; If no such item exists return the default value.
  (method
   ((<queue> q) (<procedure> f) (<value> default))
    (%take-with-filter! q f default))

  ; Remove and return the first item in arrival order that satisfies predicate f.
  ; If no such item exists return the default value.
  (method
   ((<thread-safe-queue> q) (<procedure> f) (<value> default))
   (mutex/sychronize

    (:busy q)

    (lambda () (%take-with-filter! (:store q) f default))))

  ; Remove and return the first item in arrival order that satisfies predicate f.
  ; If no such item exists then wait no more than timeout milliseconds for such an item to appear.
  ; If the timeout expires then return the default value.
  (method
   ((<thread-safe-queue> q) (<procedure> f) (<value> default) (<number> timeout))
   (mutex/synchronize

    (:busy q)

    (lambda ()
      (%rewind! (:store q))
      (%subtake! (:store q) f (make-time time-duration 0 0) (milliseconds-to-duration timeout) default))))))

;; Rewind the queue cursor.
(add-methods
 queue/rewind!
 (list
  (method ((<queue> q)) (%rewind! q))

  (method
   ((<thread-safe-queue> q))
    (mutex/lock! (:busy q))
    (%rewind! (:store q))
    (mutex/unlock! (:busy q)))))

;; Advance the queue cursor.
(add-method
 queue/advance!
 (method ((<queue> q)) (%advance! q)))

;; Peek at the head of the queue.
(add-method
 queue/peek
 (method ((<queue> q)) (%peek q)))

;; Extract the queue item referenced by the cursor.
(add-method
 queue/extract!
 (method ((<queue> q)) (%extract! q)))

;; Return the contents of the queue as a list with items in arrival order.
(add-methods
 queue/to-list
 (list
  (method ((<queue> q)) (%queue-to-list q))

  (method
   ((<thread-safe-queue> q))
   (mutex/lock! (:busy q))
   (let ((outcome (%queue-to-list (:store q))))
     (mutex/unlock! (:busy q))
     outcome))))




