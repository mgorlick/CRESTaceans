(require-library 'sisc/libs/srfi/srfi-19)

(import generic-procedures)
(import oo)
(import record)
(import srfi-19) ; Time/date library.
(import threading)
(import type-system)

(define-nongenerative-record-type <queue-posting> |563130a3-0678-4a9d-b521-57289b05a62d|
  (make-queue-posting before after posting)
  <queue-posting>?
  (before  :before  :before!)
  (after   :after   :after!)
  (posting :posting :posting!))

(define-generics
  :head :head!
  :tail :tail!
  :cursor :cursor!
  :capacity :capacity!
  :backlog :backlog!)

;; General-purpose queue (NOT thread-safe!).
(define-class (<queue>)
  (head :head :head!)  ; Queue head.
  (tail :tail :tail!)  ; Queue tail.
  (cursor :cursor :cursor!) ; Reading cursor.
  (capacity :capacity :capacity!) ; Maximum capacity of queue (#f => unbounded capacity).
  (occupancy :backlog :backlog!)) ; Number of items in queue.

;; Unbounded queue.
(define-method (initialize (<queue> q))
  (:head! q #f)
  (:tail! q #f)
  (:cursor! q #f)
  (:capacity! q #f) ; Infinite capacity.
  (:backlog! q 0))  ; Empty.

;; Bounded mailbox with maximum capacity n > 0.
(define-method (initialize (<queue> q) (<number> n))
  (:head! q #f)
  (:tail! q #f)
  (:cursor! q #f)
  (:capacity! q n) ; Capacity = n >=0.
  (:backlog! q 0)) ; Empty.

(define-generics
  empty? nonempty? full? backlog put! take!
  rewind! advance! peek extract! queue-to-list)

(define (%empty? q)
  (not (:head q)))

(define (%nonempty? q)
  (and (:head q) #t))

(define (%full? q)
  (and
   (:capacity q)
   (>= (:backlog q) (:capacity q))))

(define (%backlog q) (:backlog q))

(define (%put! q x)
  (cond
   ((%full? q) #f) ; Queue is full.

   ((:tail q) ; Queue is nonempty. Add to tail.
    (:after! (:tail q) (make-queue-posting (:tail q) #f x))
    (:tail! q (:after (:tail q)))
    (:backlog! q (+ (:backlog q) 1))
    #t)

   (else ; Queue is empty. Add to head.
    (:head! q (make-queue-posting #f #f x))
    (:tail! q (:head q))
    (:backlog! q (+ (:backlog q) 1))
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
	(:backlog! q (- (:backlog q) 1)) ; Decrement the backlog count.
	(:posting posting))))

(define (%put! q x)
  (cond
   ((%full? q) #f) ; Queue is full.

   ((:tail q) ; Queue is nonempty. Add to tail.
    (:after! (:tail q) (make-queue-posting (:tail q) #f x))
    (:tail! q (:after (:tail q)))
    (:backlog! q (+ (:backlog q) 1))
    #t)

   (else ; Queue is empty. Add to head.
    (:head! q (make-queue-posting #f #f x))
    (:tail! q (:head q))
    (:backlog! q (+ (:backlog q) 1))
    #t)))

;; Returns #t if the queue is empty and #f if items are enqueued.
(define-method (empty? (<queue> q)) (%empty? q))

;; Returns #t if items are enqueued and #f if the queue is empty.
(define-method (nonempty? (<queue> q)) (%nonempty? q))

;; Returns #t if the backlog of waiting messages equals the queue capacity
;; and #f if the backlog is less than the queue capacity.
(define-method (full? (<queue> q)) (%full? q))

;; Returns the number of elements in the queue.
(define-method (backlog (<queue> q)) (:backlog q))

;; Enqueue item x. Returns #t if successful (sufficient capacity) and #f otherwise (insufficient capacity).
(define-method (put! (<queue> q) (<value> x)) (%put! q x))

;; Remove and return the oldest (first) item in the queue.
;; If the queue is empty return the given default value.
(define-method (take! (<queue> q) (<value> default)) (%take! q default))

(define (%take-with-filter! q f default)
  (%rewind! q)
  (let loop ()
    (if (%advance! q)
	(if (f (%peek q))
	    (%extract! q)
	    (loop))
	default)))

;; Remove and return the oldest item in the queue that satisfies predicate f.
;; If no such item exists then return the given default value.
(define-method (take! (<queue> q) (<procedure> f) (<value> default))
  (%take-with-filter! q f default))

;; Rewind the queue cursor.
(define-method (rewind! (<queue> q)) (%rewind! q))

;; Advance the cursor to the next item in the queue.
;; Return #t if the cursor advanced and #f otherwise.
(define-method (advance! (<queue> q)) (%advance! q))

;; Return the item referenced by the cursor without disturbing its position in the queue.
;; If the cursor is not positioned at an item then the void value is returned.
(define-method (peek (<queue> q)) (%peek q))

;; Remove and return the item referenced by the cursor.
;; If the cursor is not positioned at a posting then the void value is returned.
(define-method (extract! (<queue> q)) (%extract! q))

(define-generics
  :more :more!
  :busy :busy!)

;; 1:n (SINGLE reader MULTIPLE writers) thread-safe queue.
(define-class (<queue-1-n> <queue>)
  (busy :busy :busy!)   ; Mutex.
  (more :more :more!))  ; Condition variable.

(define-method (initialize (next: next) (<queue-1-n> q))
  (next q)
  (:busy! q (mutex/new))
  (:more! q (condvar/new)))

(define-method (initialize (next: next) (<queue-1-n> q) (<number> n))
  (next q n)
  (:busy! q (mutex/new))
  (:more! q (condvar/new)))

(define-method (empty? (<queue-1-n> q))
  (mutex/lock! (:busy q))
  (let ((outcome (%empty? q)))
    (mutex/unlock! (:busy q))
    outcome))

(define-method (nonempty? (<queue-1-n> q))
  (mutex/lock! (:busy q))
  (let ((outcome (%nonempty? q)))
    (mutex/unlock! (:busy q))
    outcome))

(define-method (full? (<queue-1-n> q))
  (mutex/lock! (:busy q))
  (let ((outcome (%full? q)))
    (mutex/unlock! (:busy q))
    outcome))

(define-method (backlog (<queue-1-n> q))
  (mutex/lock! (:busy q))
  (let ((outcome (%backlog q)))
    (mutex/unlock! (:busy q))
    outcome))

(define-method (put! (<queue-1-n> q) (<value> x))
  (mutex/lock! (:busy q))
  (let ((ok (%put! q x)))
    (if ok (condvar/notify (:more q)))
    (mutex/unlock! (:busy q))
    ok))

;; Nonblocking read of queue. Extract and return the oldest item.
;; If no item is waiting then return the default value.
(define-method (take! (<queue-1-n> q) (<value> default))
  (mutex/lock! (:busy q))
  (let ((outcome (%take! q default)))
    (mutex/unlock! (:busy q))
    outcome))

; Blocking read of queue.
(define-method (take! (<queue-1-n> q))
  (let loop ()
    (mutex/lock! (:busy q))
    (%rewind! q)
    (cond
     ((%advance! q)
      (let ((item (%extract! q)))
	(mutex/unlock! (:busy q))
	item))
     ((mutex/unlock! (:busy q) (:more q)) ; Block until something shows up. Safe since we assume single reader.
      (loop)))))

;; Block waiting, at most timeout milliseconds, for an item to remove from the head of the queue and return.
;; If the timeout expires before an item appears return the default.
(define-method (take! (<queue-1-n> q) (<number> timeout) (<value> default))
  (let loop ()
    (mutex/lock! (:busy q))
    (%rewind! q)
    (cond
     ((%advance! q)
      (let ((item (%extract! q)))
	(mutex/unlock! (:busy q))
	item))
     ((mutex/unlock! (:busy q) (:more q) timeout)
      (loop))
     (else default))))

;; Remove and return the first item in arrival order that satisfies predicate f.
;; Blocks if necessary until such an item is found.
(define-method (take! (<queue-1-n> q) (<procedure> f))
  (mutex/synchronize

   (:busy q)

   (lambda ()
     (%rewind! q)
     (let loop ()
       (cond
	((%advance! q)
	 (if (f (%peek q)) (%extract! q) (loop)))
	(else
	 (mutex/unlock! (:busy q) (:more q)) ; Blocking wait for something else to arrive.
	 (mutex/lock! (:busy q)) ; Reacquire the lock.
	 (loop)))))))

;; Remove and return the first item in arrival order that satisfies predicate f.
;; If no such item exists return the default value.
(define-method (take! (<queue-1-n> q) (<procedure> f) (<value> default))
  (mutex/sychronize

   (:busy q)

   (lambda () (%take-with-filter! q f default))))
	 
;; Convert m in milliseconds to a SRFI-19 time-duration.
(define (milliseconds-to-duration m)
  (let ((seconds (quotient m 1000))
	(nanoseconds (* (remainder m 1000) 1000000)))
    (make-time time-duration nanoseconds seconds)))

;; Convert a SRFI-19 time duration d to integer milliseconds.
(define (duration-to-milliseconds d)
  (+ (* (time-second d) 1000) (quotient (time-nanosecond d) 1000000)))

(define (%now) (current-time time-monotonic))

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
	     (start (%now)))

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
		    (%subtake! q f (add-duration! (time-difference (%now) start) used) timeout default))))

	     (else default)) ; We waited for the entire "unused" period. By construction we exceeded the timeout.

	    default)))) ; No more time to wait.

(define-method (take! (<queue-1-n> q) (<procedure> f) (<number> timeout) (<value> default))
  (mutex/synchronize

   (:busy q)

   (lambda ()
     (%rewind! q)
     (%subtake! q f (make-time time-duration 0 0) (milliseconds-to-duration timeout) default))))
		   
;; NOTE: Timeouts in Termite (recv ...) violate the law of least surprise since
;; the (recv ...) should EITHER find a matching message in no more than timeout seconds (expressed as a real).
;; or return. However careful reading of the code for Termite (see recv.scm) shows that if messages
;; repeatedly arrive before the timeout expires but are not matched by (recv ...) then the (recv ...)
;; will just wait again for timeout seconds. This could easily lead to either indefinite blocking
;; (as long as non-matching messages continue to arrive) or eventual success but whose total elapsed time
;; exceeds the timeout.

(define-method (rewind! (<queue-1-n> q)) (%rewind! q))

(define-method (advance! (<queue-1-n> q))
  (mutex/lock! (:busy q))
  (let ((outcome (%advance! q)))
    (mutex/unlock! (:busy q))
    outcome))

(define-method (peek (<queue-1-n> q)) (%peek q))

(define-method (extract! (<queue-1-n> q))
  (mutex/lock! (:busy q))
  (let ((outcome (%extract! q)))
    (mutex/unlock! (:busy q))
    outcome))

(define (%queue-to-list q)
  (%rewind! q)
  (let loop ((items '()))
    (if (%advance! q)
	(loop (cons (%peek q) items))
	(reverse items))))

(define-method (queue-to-list (<queue> q)) (%queue-to-list q))

(define-method (queue-to-list (<queue-1-n> q))
  (mutex/lock! (:busy q))
  (let ((outcome (%queue-to-list q)))
    (mutex/unlock! (:busy q))
    outcome))


