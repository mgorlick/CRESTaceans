#! /usr/bin/env racket
#lang racket/base

(require "misc.rkt"
         "motiles.rkt"
         "environs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match
         racket/function
         racket/list)
(provide (all-defined-out))

; argsassoc: string [call: string -> any] [no-val: any] [default: any] -> any
; separates the provided command line arguments of the form:
;             --key1=val1 --key2=val2 --key3
; if the provided key name is present and has a value, returns `call' applied to that value.
; if the provided key name is present but has no value, returns `default'.
; if the provided key name is not present, returns `no-val'.
(define CLargs (map (curry regexp-split #rx"=") (vector->list (current-command-line-arguments))))
(define (argsassoc key #:call [call (λ (x) x)] #:no-val [no-val #f] #:default [default #t])
  (let ([entry (assoc key CLargs)])
    (if entry
        (if (> (length entry) 1)
            (call (second entry))
            default)
        no-val)))

; these hash tables hold necessary values for tracking local state
; and if necessary moving it to another island.
(define curls=>threads (make-hash)) ; dispatch on the actual running 
(define curls=>motiles (make-hash)) ; save COMPILED motiles for retransmission later
(define curls=>metadata (make-hash)) ; save metadata for retransmission
(define curls=>replycurls (make-hash)) ; save corresponding reply curls for retransmission

; contains-any?: returns a non-false value iff any argument beyond the first is present in the first.
; list any -> any
(define (contains-any? mlst . vs)
  (and (list? mlst) (ormap (curryr member mlst) vs)))

; handle-spawn: message/uri? any any message/uri? -> void
; when a spawn comes in, assign it a binding environment and set of standard arguments
; based on the metadata attached. then keep track of it in the above hashes
(define (handle-spawn curl body metadata reply)
  (define benv (metadata->benv metadata))
  (define args
    (cond [(contains-any? metadata
                          '("accepts" . "video/webm") 
                          '("accepts" . "audio/speex")
                          '("is" . "gui")
                          '("produces" . "video/webm")
                          '("produces" . "audio/speex"))
           (list reply)]
          [else '()]))
  (parameterize ([current-curl curl])
    (hash-set! curls=>threads curl (spawn (start-program body benv args)))
    (hash-set! curls=>motiles curl body)
    (hash-set! curls=>metadata curl metadata)
    (hash-set! curls=>replycurls curl reply)
    (printf "a new actor installed at ~n\t~s~n" curl)))

(define (valid-actor? u)
  (define t (hash-ref curls=>threads u #f))
  (and (thread? t) (thread-running? t)))

; the "actor" at 'root': hardcoded to receive all messages and then dispatches them as necessary 
(define handler
  (spawn
   (let loop ()
     (define v (thread-receive))
     (match v
       [(ask "SPAWN" (? (is? root-curl) u) body metadata reply echo)
        (define curl (make-curl (uuid)))
        (handle-spawn curl body metadata reply)]
       
       [(ask _ u body metadata reply _)
        (if (valid-actor? u)
            (thread-send (hash-ref curls=>threads u) 
                         (cons (start-program body (metadata->benv metadata)) v))
            (printf "error: not a thread or not running: ~a / directed to: ~a~n" (hash-ref curls=>threads u #f) u))]
       
       [else (printf "Message not recognized: ~a~n" else)])
     (loop))))

(define (metadata->benv metadata)
  (cond
    [(contains-any? metadata '("accepts" . "video/webm"))
     VIDEO-DECODE*]
    [(contains-any? metadata '("accepts" . "audio/speex"))
     AUDIO-DECODE*]
    [(contains-any? metadata '("produces" . "video/webm"))
     VIDEO-ENCODE*]
    [(contains-any? metadata '("produces" . "audio/speex"))
     AUDIO-ENCODE*]
    [(contains-any? metadata '("is" . "gui"))
     GUI*]
    [else
     MULTIMEDIA-BASE*]))

;; primitive for sending from the Motile level
(define (ask/send* method u body metadata)
  (cond [(hash-has-key? curls=>threads u)
         (thread-send (hash-ref curls=>threads u)
                      (cons (motile/start (motile/compile body) (metadata->benv metadata))
                            (message/ask/new method u (motile/compile body) metadata)))]
        [(message/uri? u)
         (ask/send request-thread method u body #:metadata metadata #:compile? (not (procedure? body)))]))

(define current-gui-curl
  (let ([c #f] [sema (make-semaphore 0)])
    (case-lambda
      [() (semaphore-wait sema) (semaphore-post sema) c]
      [(f) (set! c f)
           (semaphore-post sema)])))

(define get-current-gui-curl (procedure-reduce-arity current-gui-curl 0))
(define set-current-gui-curl! (procedure-reduce-arity current-gui-curl 1))

(define (respawn u host port)
  (ask/send request-thread "SPAWN"
            (message/uri/new #f (cons host port) "/")
            (hash-ref curls=>motiles u)
            #:metadata (hash-ref curls=>metadata u)
            #:reply (hash-ref curls=>replycurls u)
            #:compile? #f))

(define (cp u host port)
  (respawn u host port)
  (ask/send* "POST" u `(CP ,host ,port) #f))

(define (mv u host port)
  (respawn u host port)
  (ask/send* "POST" u `(Quit/MV ,host ,port) #f)
  (ask/send* "POST" (hash-ref curls=>replycurls u) `(RemoveCURL ,u) #f))

(define *LISTENING-ON* (argsassoc "--host" #:no-val *LOCALHOST*))
(define *LOCALPORT* (argsassoc "--port" #:no-val 5000 #:call string->number))

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k))
(define request-thread (run-tcp-peer *LISTENING-ON* *LOCALPORT* this-scurl handler))

(define make-curl (curry message/uri/new #f (cons *LISTENING-ON* *LOCALPORT*)))
(define root-curl (make-curl "/"))
(define current-curl (make-parameter root-curl))
(printf "listening on ~s~n" root-curl)

(define MULTIMEDIA-BASE* 
  (++ MULTIMEDIA-BASE (global-defines current-curl ask/send*)))
(define VIDEO-ENCODE* 
  (++ VIDEO-ENCODE    (global-defines current-curl ask/send*)))
(define AUDIO-ENCODE* 
  (++ AUDIO-ENCODE    (global-defines current-curl ask/send*)))
(define VIDEO-DECODE* 
  (++ VIDEO-DECODE    (global-defines current-curl ask/send* get-current-gui-curl)))
(define GUI* 
  (++ GUI             (global-defines current-curl ask/send* get-current-gui-curl set-current-gui-curl! cp mv)))
(define AUDIO-DECODE* 
  (++ AUDIO-DECODE    (global-defines current-curl ask/send*)))

(define (interpreter)
  (printf "Enter command...~n")
  (define cmd (read))
  (with-handlers ([exn:fail? (λ (e) (printf "~a~n" (exn-message e)) #f)])
    (match cmd
      
      [`(cp ,uuid ,host ,port)
       (define u (make-curl uuid))
       (cp u (symbol->string host) port)]
      
      [`(mv ,uuid ,host ,port)
       (define u (make-curl uuid))
       (mv u (symbol->string host) port)]
      
      [a (printf "Command not recognized: ~s~n" a)]))
  (interpreter))

(define remoteroot
  (remote-curl-root #f (argsassoc "--rhost" #:no-val *LOCALHOST*)
                    (argsassoc "--rport"#:no-val 1235 #:call string->number)))

(cond [(argsassoc "--video")
       (define relay-curl (make-curl (uuid)))
       (handle-spawn relay-curl relayer (metadata) root-curl)
       (handle-spawn (make-curl (uuid)) (video-reader/encoder (argsassoc "--video" #:default "/dev/video0")
                                                              1280 720) (metadata produces/webm) relay-curl)
       
       (ask/send request-thread "SPAWN" remoteroot command-center-gui
                 #:metadata (metadata is/gui) #:reply root-curl)
       (ask/send request-thread "SPAWN" remoteroot (video-decoder/gui 1280 720)
                 #:metadata (metadata accepts/webm) #:reply relay-curl)])

(cond [(argsassoc "--audio")
       (define relay-curl (make-curl (uuid)))
       (handle-spawn relay-curl relayer (metadata type/speex) root-curl)
       (handle-spawn (make-curl (uuid)) (audio-reader/speex-encoder 3 relay-curl) (metadata produces/speex) root-curl)
       
       (ask/send request-thread "SPAWN" remoteroot (speex-decoder 640)
                 #:metadata (metadata accepts/speex) #:reply relay-curl)])

(interpreter)

(no-return)