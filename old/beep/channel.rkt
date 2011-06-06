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

(struct frame-temp (type ; bytes?
                    payload ; (listof bytes?)
                    ))

;; merge a frame-temp with the new payload, returning a new frame-temp
(define/contract (merge-frames ft newpl)
  (frame-temp? bytes? . -> . frame-temp?)
  (match-let ([(frame-temp type payloads) ft])
    (frame-temp type (cons newpl payloads))))

(define/contract (frame-temp->response ft rpk)
  (frame-temp? bytes? . -> . response?)
  (match-let ([(frame-temp type payloads) ft])
    (payload->response type rpk (apply bytes-append (reverse payloads)))))

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
    (define/contract last-seen-remote-type symbol? 'neverseen)
    (define/contract last-seen-remote-msgno uint31/c top/u31)
    
    ;; message numbers / completed messages
    ; output to remote
    (define/contract largest-written-msg-no uint31/c 0)
    
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
    (define/contract (fresh-reply? msgno)
      (uint31/c . -> . boolean?)
      ; XXX fixme
      ; find a way to implement this that doesn't require tracking
      ; all message numbers for which any reply has been received...
      #t)
    
    ; 2.2.1.1 errors clause 6
    (define/contract (sent-msg-no? msgno)
      (uint31/c . -> . boolean?)
      ;(<= msgno largest-written-msg-no))
      ; XXX fixme rollover
      #t)
    
    ; 2.2.1.1 errors clause 7
    ;; prevents inequivalent-by-type messages from sharing a message number
    ;; over a single iteration of the message number space
    (define/contract (type-code-ok? type msgno)
      (symbol? uint31/c . -> . boolean?)
      (if (equal? msgno last-seen-remote-msgno)
          (equal? type last-seen-remote-type)
          #t))
    
    ; 2.2.1.1 errors clause 8
    ;; prevents non-ANS/NUL messages from being delivered inside an ANS sequence
    (define/contract (in-ans-mode? msgno)
      (uint31/c . -> . boolean?)
      (if (equal? last-seen-remote-type 'ans)
          (equal? last-seen-remote-msgno msgno)
          #t))
    
    ; 2.2.1.1 errors clause 9
    ;; prevents continued frames from appending with different message frames
    (define/contract (continuation-ok? msgno)
      (uint31/c . -> . boolean?)
      (if last-frame-continues?
          (equal? msgno last-seen-remote-msgno)
          #t))
    
    ; 2.2.1.1 errors clause 10
    ;; prevents sequence number desynchronization
    (define/contract (valid-seqno? seqno payload-size)
      (uint31/c uint31/c . -> . boolean?)
      (=/u32 seqno next-expected-seqno))
    
    (define/contract (update-expected-seqno! len)
      (uint32/c . -> . void)
      (set! next-expected-seqno (+/u32 next-expected-seqno len)))
    
    ;; call when ready to deliver a complete message to user space
    ;; takes a frame-temp and turns it into a real response
    (define/contract (enqueue-complete-frame ft)
      (frame-temp? . -> . void)
      (let ([response (frame-temp->response ft remote-public-key)])
        (semaphore-wait message-queue-lock)
        (enqueue! message-queue response)
        (printf "enqueued: ~s~n" response)
        (semaphore-post message-queue-lock)))
    
    ;; -----------------------------
    ;; new message received handlers
    ;; -----------------------------
    
    (define/public (new-msg-frame msgno seqno more? payload)
      ;      (cond
      ;        [(and (valid-seqno? seqno (bytes-length payload))
      ;              (type-code-ok? #"MSG" msgno)
      ;              (continuation-ok? msgno))
      ;         (if more?
      ;             (process/continue msgno seqno payload #"MSG")
      ;             (process/finish msgno seqno payload #"MSG"))]
      ;        [else (raise-frame-warning
      ;               (format "invalid msg frame (seq ok? ~a; type ok? ~a; continuation ok? ~a)"
      ;                       (valid-seqno? seqno (bytes-length payload))
      ;                       (type-code-ok? #"MSG" msgno)
      ;                       (continuation-ok? msgno)))]))
      'x)
    
    ;; -----------------------------
    ;; reply frame received handlers
    ;; -----------------------------
    
    (define/contract (response/process/continue msgno seqno payload type)
      (uint31/c uint32/c bytes? symbol? . -> . void)
      (response/process-frame msgno seqno payload type (curry dict-set! pending-frames msgno) #t))
    
    (define/contract (response/process/finish msgno seqno payload type)
      (uint31/c uint32/c bytes? symbol? . -> . void)
      (response/process-frame msgno seqno payload type enqueue-complete-frame #f))
    
    (define/contract (response/process-frame msgno seqno payload type handlefun continue?)
      (uint31/c uint32/c bytes? symbol? (frame-temp? . -> . void) boolean? . -> . void)
      (if (dict-has-key? pending-frames msgno)
          (handlefun (merge-frames (dict-ref pending-frames msgno) payload))
          (handlefun (frame-temp type (list payload))))
      (set! last-seen-remote-msgno msgno)
      (set! last-seen-remote-type type)
      (set! last-frame-continues? continue?)
      (update-expected-seqno! (bytes-length payload)))
    
    (define/public (new-err-frame msgno seqno more? payload)
      (cond
        [(and (fresh-reply? msgno)
              (sent-msg-no? msgno)
              (type-code-ok? 'err msgno)
              (valid-seqno? seqno (bytes-length payload))
              (continuation-ok? msgno))
         (if more?
             (response/process/continue msgno seqno payload 'err)
             (response/process/finish msgno seqno payload 'err))]
        [else (raise-frame-warning
               (format "invalid err frame (reply ok? ~a; seq ok? ~a; type ok? ~a; continuation ok? ~a; msgno ok? ~a)"
                       (fresh-reply? msgno)
                       (valid-seqno? seqno (bytes-length payload))
                       (type-code-ok? 'err msgno)
                       (continuation-ok? msgno)
                       (sent-msg-no? msgno)))]))
    
    (define/public (new-rpy-frame msgno seqno more? payload)
      (printf "processing rpy frame~n")
      (cond
        [(and (fresh-reply? msgno)
              (sent-msg-no? msgno)
              (type-code-ok? 'rpy msgno)
              (valid-seqno? seqno (bytes-length payload))
              (continuation-ok? msgno))
         (printf "RPY OK~n")
         (if more?
             (response/process/continue msgno seqno payload 'rpy)
             (response/process/finish msgno seqno payload 'rpy))]
        [else (raise-frame-warning
               (format "invalid rpy frame (reply ok? ~a; seq ok? ~a; type ok? ~a; continuation ok? ~a; msgno ok? ~a)"
                       (fresh-reply? 'rpy msgno)
                       (valid-seqno? seqno (bytes-length payload))
                       (type-code-ok? 'rpy msgno)
                       (continuation-ok? msgno)              
                       (sent-msg-no? msgno)))]))
    
    (define/public (new-ans-frame msgno seqno ansno more? payload)
      (cond
        [(and (valid-seqno? seqno (bytes-length payload))
              (continuation-ok? msgno)
              (type-code-ok? 'ans msgno)
              (sent-msg-no? msgno))
         (if more?
             (response/process/continue msgno seqno payload 'ans)
             (response/process/finish msgno seqno payload 'ans))]
        [else
         (raise-frame-warning
          (format "invalid ans frame (valid seqno? ~a; continuation ok? ~a; type code ok? ~a; sent msg no? ~a)"
                  (valid-seqno? seqno (bytes-length payload))
                  (continuation-ok? msgno)
                  (type-code-ok? 'ans msgno)
                  (sent-msg-no? msgno)))]))
    
    (define/public (new-nul-frame msgno seqno)
      (cond
        [(valid-seqno? seqno 0)
         (if (and (sent-msg-no? msgno) (in-ans-mode? msgno))
             (begin
               ; enqueue any partially-finished ANS message in the pending frame dict?
               (set! last-seen-remote-msgno msgno)
               (set! last-frame-continues? #f)
               (set! last-seen-remote-type 'nul))
             (raise-frame-warning
              (format "NUL not ok (sent the msgno? ~a; in ans mode? ~a)"
                      (sent-msg-no? msgno)
                      (in-ans-mode?))))]
        [else (raise-frame-warning
               (format "invalid sequence number on a nul frame (got seq ~a, expected ~a)"
                       seqno
                       next-expected-seqno))]))))