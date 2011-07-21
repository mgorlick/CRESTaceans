#! /usr/bin/env racket
#lang racket/base

(require "environs.rkt" "misc.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         "../../peer/src/api/message.rkt"
         racket/match
         racket/function
         unstable/function)

(define *LISTENING-ON* *LOCALHOST*)
(define *LOCALPORT* 1235)

(define curls=>threads (make-hash)) ; dispatch on the actual running 
(define curls=>motiles (make-hash)) ; save motiles for retransmission later.....xxx fixme
(define curls=>metadata (make-hash)) ; save metadata for retransmission
(define curls=>replycurls (make-hash)) ; save corresponding reply curls for retransmission

(define (handle-spawn curl body benv metadata reply)
  (hash-set! curls=>threads curl (spawn (start-program body benv)))
  (hash-set! curls=>motiles curl body)
  (hash-set! curls=>metadata curl metadata)
  (hash-set! curls=>replycurls curl reply)
  (printf "a new actor installed at ~s~n" curl))

;; primitive for sending from the Motile level
(define ask/send*
  (case-lambda
    [(method url body) (ask/send request-thread method url body)]
    [(method url body metadata) (ask/send request-thread method url body #:metadata metadata)]))
(define MULTIMEDIA-BASE* (++ MULTIMEDIA-BASE (list (define/global/N 'ask/send* ask/send*))))
(define VIDEO-ENCODE* (++ VIDEO-ENCODE (list (define/global/N 'ask/send* ask/send*))))
(define AUDIO-ENCODE* (++ AUDIO-ENCODE (list (define/global/N 'ask/send* ask/send*))))
(define VIDEO-DECODE* (++ VIDEO-DECODE (list (define/global/N 'ask/send* ask/send*))))
(define AUDIO-DECODE* (++ AUDIO-DECODE (list (define/global/N 'ask/send* ask/send*))))

(define (metadata->benv metadata)
  (match metadata
    ['(("accepts" . "video/webm")) VIDEO-DECODE*]
    ['(("accepts" . "audio/speex")) AUDIO-DECODE*]
    ['(("produces" . "video/webm")) VIDEO-ENCODE*]
    ['(("produces" . "audio/speex")) AUDIO-ENCODE*]
    [_ MULTIMEDIA-BASE*]))

(define handler
  (spawn
   (let loop ()
     (define v (thread-receive))
     (match v
       [(ask "SPAWN" (? (is? root-curl) u) body metadata reply echo)
        (define curl (make-curl (uuid)))
        (handle-spawn curl body (metadata->benv metadata) metadata reply)
        (ask/send request-thread "POST" reply `(AddCURL ,curl) #:metadata '(("is-a" . "curl")))]
       
       [(ask "POST" u body metadata reply echo)
        ;(printf "dispatch to ~a~n" u)
        (if ((conjoin thread? thread-running?) (hash-ref curls=>threads u #f))
            (thread-send (hash-ref curls=>threads u) (start-program body (metadata->benv metadata)))
            (printf "error: not a thread or not running: ~a~n" (hash-ref curls=>threads u #f)))]
       [else (printf "Message not recognized: ~a~n" else)])
     (loop))))

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k))
(define request-thread (run-tcp-peer *LISTENING-ON* *LOCALPORT* this-scurl handler))
(define make-curl (curry message/uri/new #f (cons *LISTENING-ON* *LOCALPORT*)))
(define root-curl (make-curl "/"))
(printf "listening on ~s~n" root-curl)

(define (cp u host port)
  (ask/send request-thread
            "SPAWN"
            (message/uri/new #f (cons (symbol->string host) port) "/")
            (hash-ref curls=>motiles u)
            #:metadata (hash-ref curls=>metadata u)
            #:reply (hash-ref curls=>replycurls u)
            #:compile? #f))

(let interpreter ()
  (printf "Enter command...~n")
  (define cmd (read))
  (with-handlers ([exn:fail? (λ (e) (printf "~a~n" (exn-message e)) #f)])
    (match cmd
      [(list 'mv uuid host port key)
       ;; look up the actor named by the uuid locally. then transmit its original decompiled
       ;; source representation to the new island and name the originating reply url as
       ;; the destination for the new CURL notification
       (define u (make-curl uuid))
       (cp u host port)
       (ask/send request-thread "POST" (hash-ref curls=>replycurls u) `(RemoveCURL ,u))]
      
      [(list 'cp uuid host port key)
       (define u (make-curl uuid))
       (cp u host port)]
      [a (printf "Command not recognized: ~s~n" a)]))
  (interpreter))