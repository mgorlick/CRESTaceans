#lang racket/base

(require racket/dict
         racket/contract
         racket/match
         racket/class
         racket/function
         unstable/contract
         data/queue
         "msg.rkt"
         "parse-app-protocol.rkt")

(provide channel%)

(define-syntax-rule (inc! x adder)
  (set! x (adder x)))

(struct frame-temp (type payload))

;; merge a frame-temp with the new payload, returning a new frame-temp
(define/contract (merge-frames ft newpl)
  (frame-temp? bytes? . -> . frame-temp?)
  (match-let ([(frame-temp type oldpl) ft])
    (frame-temp type (bytes-append oldpl newpl))))

(define/contract (frame-temp->response ft rpk)
  (frame-temp? bytes? . -> . response?)
  (match-let ([(frame-temp type payload) ft])
    (payload->response type rpk payload)))

(define channel%
  (class object%
    [init channelno
          this-pk
          remote-pk]
    
    (define/contract this-public-key    bytes? this-pk)
    (define/contract remote-public-key  bytes? remote-pk)
    (define/contract channel-number     exact-nonnegative-integer? channelno)
    
    ;; sequence numbers / frames
    (define/contract next-expected-seqno   uint32/c 0)
    (define/contract last-frame-continues? boolean? #f)
    (define/contract last-seen-type        bytes? #"")
    
    ;; message numbers / completed messages
    ; input from remote
    (define/contract last-seen-msgno uint31/c top/u31)
    
    ; received and queued-for-delivery messages
    (define/contract last-rpy-enqueued-no uint31/c top/u31)
    (define/contract last-ans-enqueued-no uint31/c top/u31)
    (define/contract last-msg-enqueued-no uint31/c top/u31)
    (define/contract last-err-enqueued-no uint31/c top/u31)
    
    ; output to remote
    (define/contract largest-written-msg-no uint31/c 0)
    (define/contract largest-written-rpy-no uint31/c 0)
    (define/contract largest-written-ans-no uint31/c 0)
    (define/contract largest-written-err-no uint31/c 0)
    (define/contract largest-written-nul-no uint31/c 0)
    
    (define/contract pending-frames ; msgno -> in-progress concatenated frames
      (dict/c uint31/c frame-temp?) (make-hash))
    (define/contract message-queue queue? (make-queue))
    (define/contract message-queue-lock semaphore? (make-semaphore 1))
    
    (super-new)
    
    (define/public (get-this-pk)
      this-public-key)
    
    (define/public (get-remote-pk)
      remote-public-key)
    
    ;; note that most of these error-case catching functions assume
    ;; a serialized nature of message writing
    ;; which is to say, all frames of message A are written before
    ;; any frame of message B is written
    
    ; 2.2.1.1 errors clause 4
    ; this doesn't make sense:
    ; suppose I send message 5
    ; you never reply to message 5, purposefully
    ; then 2^32-1 messages later we hit message 5 again
    ; spec implies that it should be rejected
    ;(define (msg-not-used-already? msgno)
    ;  #t)
    
    ; 2.2.1.1 errors clause 5
    ;; prevent receiving a reply from a reply that was already received
    (define (didnt-already-get-reply? type msgno)
      (match type
        [#"ERR" (> msgno last-err-enqueued-no)]
        ;[#"ANS" (> msgno last-ans-enqueued-no)] this constraint doesn't make sense with ANS/NUL, more than one response is legal
        [#"RPY" (> msgno last-rpy-enqueued-no)]
        [else #t]))
    
    ; 2.2.1.1 errors clause 6
    (define (sent-msg-no? msgno)
      (<= msgno largest-written-msg-no))
    
    ; 2.2.1.1 errors clause 7
    ;; prevents inequivalent-by-type messages from sharing a message number
    ;; over a single iteration of the message number space
    (define (type-code-ok? type msgno)
      (if (equal? msgno last-seen-msgno)
          (equal? type last-seen-type)
          #t))
    
    ; 2.2.1.1 errors clause 8
    ;; prevents non-ANS/NUL messages from being delivered inside an ANS sequence
    (define (in-ans-mode? msgno)
      (if (equal? last-seen-type #"ANS")
          (equal? last-seen-msgno msgno)
          #t))
    
    ; 2.2.1.1 errors clause 9
    ;; prevents continued frames from appending with different message frames
    (define (continuation-ok? msgno)
      (if last-frame-continues?
          (equal? msgno last-seen-msgno)
          #t))
    
    ; 2.2.1.1 errors clause 10
    ;; prevents sequence number desynchronization
    (define (valid-seqno? seqno payload-size)
      (=/u32 seqno next-expected-seqno))
    
    (define (update-expected-seqno! len)
      (set! next-expected-seqno (+/u32 next-expected-seqno len)))
    
    (define (process/continue msgno seqno payload type)
      (process-frame msgno seqno payload type (curry dict-set! pending-frames msgno) #t))
    
    (define (process/finish msgno seqno payload type)
      (process-frame msgno seqno payload type enqueue-complete-frame #f))
    
    (define (process-frame msgno seqno payload type handlefun continue?)
      (begin (if (dict-has-key? pending-frames msgno)
                 (handlefun (merge-frames (dict-ref pending-frames msgno) payload))
                 (handlefun (frame-temp type payload)))
             (set! last-seen-msgno msgno)
             (set! last-frame-continues? continue?)
             (set! last-seen-type type)
             (update-expected-seqno! (bytes-length payload))))
    
    ;; call when ready to deliver a complete message to user space
    ;; takes a frame-temp and turns it into a real response
    (define/contract (enqueue-complete-frame ft)
      (frame-temp? . -> . void)
      (let ([response (frame-temp->response ft remote-public-key)])
        (semaphore-wait message-queue-lock)
        (enqueue! message-queue response)
        (printf "enqueued: ~s~n" response)
        (semaphore-post message-queue-lock)))
    
    (define/public (new-ans-frame msgno seqno ansno more? payload) ; error clauses: 6, 7, 9, 10
      (cond
        [(and (valid-seqno? seqno (bytes-length payload))
              (continuation-ok? msgno)
              (type-code-ok? #"ANS" msgno)
              (sent-msg-no? msgno))
         (if more?
             (process/continue msgno seqno payload #"ANS")
             (process/finish msgno seqno payload #"ANS"))]
        [else
         (raise-frame-warning (format
                               "invalid ans frame (valid seqno? ~a; continuation ok? ~a; type code ok? ~a; sent msg no? ~a)"
                               (valid-seqno? seqno (bytes-length payload))
                               (continuation-ok? msgno)
                               (type-code-ok? #"ANS" msgno)
                               (sent-msg-no? msgno)))]))
    
    (define/public (new-nul-frame msgno seqno)
      (cond
        [(valid-seqno? seqno 0)
         (if (and (sent-msg-no? msgno) (in-ans-mode? msgno))
             (begin
               ; enqueue any partially-finished ANS message in the pending frame dict?
               (set! last-seen-msgno msgno)
               (set! last-frame-continues? #f)
               (set! last-seen-type #"NUL"))
             (raise-frame-warning
              (format "NUL not ok (sent the msgno? ~a; in ans mode? ~a)" (sent-msg-no? msgno) (in-ans-mode?))))]
        [else (raise-frame-warning
               (format "invalid sequence number on a nul frame (got seq ~a, expected ~a)" seqno next-expected-seqno))]))
    
    (define/public (new-msg-frame msgno seqno more? payload)
      (cond
        [(and (valid-seqno? seqno (bytes-length payload))
              (type-code-ok? #"MSG" msgno)
              (continuation-ok? msgno))
         (if more?
             (process/continue msgno seqno payload #"MSG")
             (process/finish msgno seqno payload #"MSG"))]
        [else (raise-frame-warning
               (format "invalid msg frame (seq ok? ~a; type ok? ~a; continuation ok? ~a)"
                       (valid-seqno? seqno (bytes-length payload))
                       (type-code-ok? #"MSG" msgno)
                       (continuation-ok? msgno)))]))
    
    (define/public (new-rpy-frame msgno seqno more? payload)
      'x)
    
    (define/public (new-err-frame msgno seqno more? payload)
      'x)))