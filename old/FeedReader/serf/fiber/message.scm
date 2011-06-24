;; Copyright 2009 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Upper level of Serf messaging.

(define-nongenerative-record-type <serf/message> |d88ff7db-5833-4512-a88c-8127d533d24|
  (message/new path body metadata reply echo)
  serf/message?
  (path     :message/path)     ; Path in destination.
  (body     :message/body)     ; Payload
  (metadata :message/metadata) ; Additional information about payload and message.
  (reply    :message/reply)    ; Reply address one of: (mailbox . path), URI object, or URL as string.
  (echo     :message/echo))    ; Information supplied by sender to be echoed back in reply (if any).

(define (@ mailbox path)
  (cons mailbox path))

(define :no-reply:    #f)
(define :no-metadata: '())
(define :no-echo:     #f)

(define (message/path+body m)
  (and (serf/message? m)
       (vector (:message/path m) (:message/body m))))

(define (message/path+body+metadata m)
  (and (serf/message? m)
       (vector (:message/path m) (:message/body m) (:message/metadata m))))

(define (message/path+body+reply m)
  (and (serf/message? m)
       (vector (:message/path m) (:message/body m) (:message/reply m))))

(define (message/path+body+echo m)
  (and (serf/message? m)
       (vector (:message/path m) (:message/body m) (:message/echo m))))

(define (message/all m)
  (and (serf/message? m)
       (vector (:message/path m) (:message/body m) (:message/metadata m) (:message/reply m) (:message/echo m))))


(define (! to body . arguments)
  (match
   arguments
   (()                       (message/! to body :no-metadata: :no-reply: :no-echo:))
   ((,metadata)              (message/! to body metadata      :no-reply: :no-echo:))
   ((,metadata ,reply)       (message/! to body metadata      reply      :no-echo:))
   ((,metadata ,reply ,echo) (message/! to body metadata      reply      echo))))

(define (message/! to body metadata reply echo)
  (cond
   ((pair? to) ; (mailbox . path)
    (let* ((mailbox (car to))
	   (path    (cdr to))
	   (m       (message/new path body metadata reply echo)))
      (mailbox/send! mailbox m)))

   ((string? to) #f) ; Unimplemented. Here to is a text URL like "http://www.example.com/a/b/c".

   (else #f))) ; Need to implement for <java.net.URI>.

(define (? . arguments)
  (match
   arguments

   ((,mailbox)
    (mailbox/receive! mailbox))

   ((,mailbox ,timeout ,default)
    (guard (integer? timeout) (>= timeout 0))
    (mailbox/receive! mailbox timeout default))))
      
(define (message/test/01)
  (let ((alpha (make <mailbox>))
	(beta  (make <mailbox>))
	(m     #f))
    (! (@ alpha 'ping) (list 1 2 3) :no-metadata: (@ beta 'pong) 'foobar)
    (? alpha)))
