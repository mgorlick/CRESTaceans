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

(define audit           (make-generic-method))
(define dispatch        (make-generic-method))
(define collect         (make-generic-method))
(define sham/register   (make-generic-method))
(define sham/unregister (make-generic-method))
(define sham/port       (make-generic-method))

(define <serf/sham>
  (let ()
    (define-java-class <java-sham-wrapper>         |ShamWrapper|)
    (define-java-class <java-sham-handler-wrapper> |ShamHandlerWrapper|)
    (define-generic-java-field-accessor :queue    |shamQueue|) ; ShamWrapper
    (define-generic-java-field-accessor :request  |req|)  ; ShamHandlerWrapper
    (define-generic-java-field-accessor :response |resp|) ; ShamHandlerWrapper
    (define-generic-java-field-accessor :context  |obj|)  ; ShamHandlerWrapper
    (define-generic-java-method take)
    (define-generic-java-method completed)

    (lambda ()
      (let* ((self          (make-object))
             (origin-server "Serf/Sham 0.1")
             (done          (make <mailbox>))
             (logger        #f)
             (host          #f)
             (port          #f)
             (root          #f) ; Document root for serving static files.
             (registry      (object/forge <sham/matcher>))
             (id            1) ; Unique id for each request/response pair.
             (pending       (make-hashtable eqv?)) ; Map from id to ShamHandlerWrapper instance. All pending request/response pairs.
             (dispatcher    #f) ; Thread running the dispatch loop.
             (collector     #f) ; Thread running the collector loop.
             (bottom        #f))

        (make-method!
         self instantiate
         (lambda (self $host $port $root $logger)
           (set! host $host)
           (set! port $port)
           (set! logger $logger)
           (set! bottom (java-new <java-sham-wrapper> (->jstring host) (->jint port) (->jstring $root) (->jstring origin-server) logger))
           (set! dispatcher
                 (fiber/start
                  (fiber/new (lambda () (dispatch self)))))
           (set! collector
                 (fiber/start
                  (fiber/new (lambda () (collect self)))))))


                                        ; Logging for sham.
        (make-method!
         self audit
         (lambda (self method message)
           (log/audit logger (format "serf/sham.~a: ~a" method message))))

                                        ; Register a responder for a portion of the URL space.
                                        ; pattern: Scheme string of the form "*<uri>", "<uri>*" or "*".
                                        ; responder: mailbox to which request/response pair is sent
        (make-method!
         self sham/register
         (lambda (self pattern responder) ; 
           (matcher/register registry pattern responder)
           #t))

                                        ; Unregister a responder for a portion of the URL space.
                                        ; pattern: Scheme string of the form "*<uri>", "<uri>*" or "*".
        (make-method!
         self sham/unregister
         (lambda (self pattern)
           (matcher/unregister registry pattern)
           #t))

        (make-method!
         self dispatch
         (lambda (self)
           (audit self "dispatch" "start")
           (let loop ((wrapper (take (:queue bottom))))
             (let* ((req   (:request wrapper))
                    (resp  (:response wrapper))
                    (alpha     (object/forge <http/request>  req))
                    (beta      (object/forge <http/response> resp))
                    (responder (matcher/match registry (http/request/uri* alpha))))
               (audit self "dispatch" (format "method:~a path:~a id:~d" (http/request/method alpha) (http/request/uri alpha) id))
               (hashtable/put! pending id (cons wrapper (now/monotonic))) ; Wrapper + timestamp.
               (if responder
                   (begin
                     ; Echo is id.
                     (case (string->symbol (http/request/method alpha))
                        ((|GET|) (! (@ responder '/http/get) (vector #f (uri/new (http/request/uri alpha)) alpha beta) :no-metadata: (@ done '/sham/response) id))
                        ((|POST| |PUT|) (! (@ responder '/http/post) (vector #f (uri/new (http/request/uri alpha)) (http/entity/string (http/request/entity alpha)))) (! (@ done '/sham/response) #f :no-metadata: #f id))
                        (else
                          (http/response/status! beta 501)
                          (http/response/reason! beta "Method Not Implemented")
                          (http/response/entity! beta "501 Method Not Implemented")
                          (! (@ done '/sham/response) #f :no-metadata: #f id)
                         ))
                     )
                   (begin
                     (audit self "dispatch" (format "method:~a path:~a not found" (http/request/method alpha) (http/request/uri alpha)))
                     (http/response/status! beta 404)
                     (http/response/reason! beta "Not Found")
                     (http/response/entity! beta "404 Not Found. What did you expect?")
                     (! (@ done '/sham/response) #f :no-metadata: #f id))
                     ))
             (set! id (+ id 1))
             (loop (take (:queue bottom))))))

        (make-method!
         self collect
         (lambda (self)
           (audit self "collect" "start")
           (let loop ((m (? done)))
             ;(audit self "collect" (format "collect: path:~a echo:~a" (:message/path m)(:message/echo m)))
             (match
              (message/path+body+echo m)

              (#(/sham/response ,_ignore ,id) ; Whoever was the responder for the request/response pair for that id is finished.
               (guard (integer? id) (positive? id))
               (let ((pair (hashtable/get pending id))) ; (wrapper . timestamp)
                 (if pair (completed (car pair)))
                 (audit self "collect" (format "collect id:~d duration:~d"
                                     id
                                     (duration-to-milliseconds (time-difference (now/monotonic) (cdr pair)))))
                 (hashtable/remove! pending id)
               ))

              (,_ignore #f))
             (loop (? done)))))

        (make-method!
         self sham/port
         (lambda (self) port))

        self))))


(define (sham/test/01)
  (let* ((log     (log/new "serf:8080" "serf:8080"))    ; Create a log for this Sham instance.
	 (sham    (object/forge <serf/sham> 8080 log #f)) ; Create an instance of Sham.
	 (counter 1))
					
    (define (responder sham from uri request response)
      (http/response/entity! response (format "Hello! Nice to hear from you again [~d]." counter))
      (set! counter (+ counter 1)))

    (sham/register sham "/*" responder) ; Register the responder with the Sham instance.
    sham))

(define (sham/test/02)
  (let* ((log     (log/new "serf:8080" "serf:8080"))    ; Create a log for this Sham instance.
	 (sham    (object/forge <serf/sham> 8080 log
				"/Users/mgorlick/Projects/sisc-1.16.6/serf/")) ; Create an instance of Sham.
	 (counter 1))
					
    (define (responder sham from uri request response)
      (http/response/entity! response (format "Hello! Nice to hear from you again [~d]." counter))
      (set! counter (+ counter 1)))

    (sham/register sham "/*" responder) ; Register the responder with the Sham instance.
    sham))

