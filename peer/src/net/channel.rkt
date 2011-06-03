#lang racket/base

(require racket/dict
         racket/contract
         racket/tcp
         racket/match
         racket/class
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
    
    ; 2.2.1.1 errors clause 4
    ;(define (msg-not-used-already? msgno)
    ;  #t)
    
    ; 2.2.1.1 errors clause 5
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
    (define (type-code-ok? type msgno)
      (if (equal? msgno last-seen-msgno)
          (equal? type last-seen-type)
          #t))
    
    ; 2.2.1.1 errors clause 8, slightly weaker enforcement than spec
    (define (nul-ok?)
      (equal? last-seen-type #"ANS"))
    
    ; 2.2.1.1 errors clause 9
    (define (continuation-ok? msgno)
      (if last-frame-continues?
          (equal? msgno last-seen-msgno)
          #t))
    
    ; 2.2.1.1 errors clause 10
    (define (valid-seqno? seqno payload-size)
      (=/u32 seqno next-expected-seqno))
    
    (define/public (new-ans-frame msgno seqno ansno more? payload) ; error clauses: 6, 7, 9, 10
      (printf "new ANS frame~n")
      (cond
        [(and (valid-seqno? seqno (bytes-length payload))
              (continuation-ok? msgno)
              (type-code-ok? #"ANS" msgno)
              (sent-msg-no? msgno))
         (printf "frame is OK~n")
         (if more?
             (new-ans-frame/continued msgno seqno ansno payload)
             (new-ans-frame/finish msgno seqno ansno payload))]
        [else
         (raise-frame-warning (format
                               "invalid ans frame (valid seqno? ~a; continuation ok? ~a; type code ok? ~a; sent msg no? ~a)"
                               (valid-seqno? seqno (bytes-length payload))
                               (continuation-ok? msgno)
                               (type-code-ok? #"ANS" msgno)
                               (sent-msg-no? msgno)))]))
    
    ; an ANS with a #"*" continuation indicator
    (define/contract (new-ans-frame/continued msgno seqno ansno payload)
      (uint31/c uint32/c uint31/c bytes? . -> . void)
      (if (dict-has-key? pending-frames msgno)
          (dict-set! pending-frames msgno (merge-frames (dict-ref pending-frames msgno) payload))
          (dict-set! pending-frames msgno (frame-temp #"ANS" payload)))
      (set! last-seen-msgno msgno)
      (set! last-frame-continues? #t)
      (set! last-seen-type #"ANS")
      (set! next-expected-seqno (+ next-expected-seqno (bytes-length payload))))
    
    ; an ANS with a #"." continuation indicator
    (define/contract (new-ans-frame/finish msgno seqno ansno payload)
      (uint31/c uint32/c uint31/c bytes? . -> . void)
      (if (dict-has-key? pending-frames msgno)
          (enqueue-complete-frame (merge-frames (dict-ref pending-frames msgno) payload))
          (enqueue-complete-frame (frame-temp #"ANS" payload)))
      (set! last-seen-msgno msgno)
      (set! last-frame-continues? #f)
      (set! last-seen-type #"ANS")
      (set! next-expected-seqno (+ next-expected-seqno (bytes-length payload))))
    
    ;; call when ready to deliver a complete message to user space
    ;; takes a frame-temp and turns it into a real response
    (define/contract (enqueue-complete-frame ft)
      (frame-temp? . -> . void)
      (let ([response (frame-temp->response ft remote-public-key)])
        (semaphore-wait message-queue-lock)
        (enqueue! message-queue response)
        (printf "enqueued: ~a~n" response)
        (semaphore-post message-queue-lock)))
    
    (define/public (new-nul-frame msgno seqno)
      (cond
        [(valid-seqno? seqno 0)
         (if (and (sent-msg-no? msgno) (nul-ok?))
             (begin
               (set! last-seen-msgno msgno)
               (set! last-frame-continues? #f)
               (set! last-seen-type #"NUL")
               ; enqueue any partially-finished ANS message in the pending frame dict?
               )
             (raise-frame-warning "NUL not ok"))]
        [else (raise-frame-warning "invalid sequence number on a nul frame")]))
    
    (define/public (new-msg-frame msgno seqno more? payload)
      'y)
    
    (define/public (new-rpy-frame msgno seqno more? payload)
      'x)
    
    (define/public (new-err-frame msgno seqno more? payload)
      'x)))