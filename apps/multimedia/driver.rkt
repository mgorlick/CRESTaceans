#! /usr/bin/env racket
#lang racket/base

(require "misc.rkt"
         "motiles.rkt"
         "environs.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match
         racket/function
         unstable/function
         racket/list)
(provide (all-defined-out))

(define CLargs (map (curry regexp-split #rx"=") (vector->list (current-command-line-arguments))))

(define (argsassoc key #:no-val [no-val '()] #:call [call (λ (x) x)] #:default [default #f])
  (let ([entry (assoc key CLargs)])
    (if entry
        (if (> (length entry) 1)
            (call (second entry))
            no-val)
        default)))

(define *LISTENING-ON* (argsassoc "--host" #:default *LOCALHOST*))
(define *LOCALPORT* (argsassoc "--port" #:default 5000 #:call string->number))

(define top-thread (current-thread))

(define curls=>threads (make-hash)) ; dispatch on the actual running 
(define curls=>motiles (make-hash)) ; save COMPILED motiles for retransmission later
(define curls=>metadata (make-hash)) ; save metadata for retransmission
(define curls=>replycurls (make-hash)) ; save corresponding reply curls for retransmission

(define (handle-spawn curl body metadata reply)
  (define benv (metadata->benv metadata))
  (define args (match metadata
                 [(or '(("accepts" . "video/webm"))
                      '(("accepts" . "audio/speex"))
                      '(("is" . "gui")))
                  (list curl reply)]
                 [(or '(("produces" . "video/webm"))
                      '(("produces" . "audio/speex")))
                  (list reply)]
                 [_ '()]))
  (hash-set! curls=>threads curl (spawn (start-program body benv args)))
  (hash-set! curls=>motiles curl body)
  (hash-set! curls=>metadata curl metadata)
  (hash-set! curls=>replycurls curl reply)
  (printf "a new actor installed at ~n\t~s~n" curl))

(define (metadata->benv metadata)
  (match metadata
    ['(("accepts" . "video/webm")) VIDEO-DECODE*]
    ['(("accepts" . "audio/speex")) AUDIO-DECODE*]
    ['(("produces" . "video/webm")) VIDEO-ENCODE*]
    ['(("produces" . "audio/speex")) AUDIO-ENCODE*]
    ['(("is" . "gui")) GUI*]
    [_ MULTIMEDIA-BASE*]))

;; primitive for sending from the Motile level
(define (ask/send* method u body metadata)
  (cond [(hash-has-key? curls=>threads u)
         (thread-send (hash-ref curls=>threads u)
                      (motile/start (motile/compile body) (metadata->benv metadata)))]
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

(define additions (global-defines ask/send* current-gui-curl))
(define MULTIMEDIA-BASE* (++ MULTIMEDIA-BASE (global-defines ask/send*)))
(define VIDEO-ENCODE* (++ VIDEO-ENCODE (global-defines ask/send*)))
(define AUDIO-ENCODE* (++ AUDIO-ENCODE (global-defines ask/send*)))
(define VIDEO-DECODE* (++ VIDEO-DECODE (global-defines ask/send* get-current-gui-curl)))
(define GUI* (++ GUI (global-defines ask/send* get-current-gui-curl set-current-gui-curl!)))
(define AUDIO-DECODE* (++ AUDIO-DECODE (global-defines ask/send*)))

(define handler
  (spawn
   (let loop ()
     (define v (thread-receive))
     (match v
       [(ask "SPAWN" (? (is? root-curl) u) body metadata reply echo)
        (define curl (make-curl (uuid)))
        (handle-spawn curl body metadata reply)]
      
       [(ask "POST" u body metadata reply echo)
        (if ((conjoin thread? thread-running?) (hash-ref curls=>threads u #f))
            (thread-send (hash-ref curls=>threads u) (start-program body (metadata->benv metadata)))
            (printf "error: not a thread or not running: ~a / directed to: ~a~n" (hash-ref curls=>threads u #f) u))]
       
       [else (printf "Message not recognized: ~a~n" else)])
     (loop))))

(define k (generate-key/defaults))
(define this-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k))
(define request-thread (run-tcp-peer *LISTENING-ON* *LOCALPORT* this-scurl handler))
(define make-curl (curry message/uri/new #f (cons *LISTENING-ON* *LOCALPORT*)))
(define root-curl (make-curl "/"))
(printf "listening on ~s~n" root-curl)

(define (cp u host port)
  (ask/send request-thread "SPAWN"
            (message/uri/new #f (cons (symbol->string host) port) "/")
            (hash-ref curls=>motiles u)
            #:metadata (hash-ref curls=>metadata u)
            #:reply (hash-ref curls=>replycurls u)
            #:compile? #f))

(define (interpreter)
  (printf "Enter command...~n")
  (define cmd (read))
  (with-handlers ([exn:fail? (λ (e) (printf "~a~n" (exn-message e)) #f)])
    (match cmd
      
      [`(cp ,uuid ,host ,port)
       (define u (make-curl uuid))
       (cp u host port)]
      
      [`(mv ,uuid ,host ,port)
       (define u (make-curl uuid))
       (cp u host port)
       (ask/send request-thread "POST" (hash-ref curls=>replycurls u) `(RemoveCURL ,u))]
      
      [a (printf "Command not recognized: ~s~n" a)]))
  (interpreter))

(define remoteroot
  (remote-curl-root #f (argsassoc "--rhost" #:default *LOCALHOST*)
                    (argsassoc "--rport"#:default 1235 #:call string->number)))

(cond [(argsassoc "--video")
       (define relay-curl (make-curl (uuid)))
       (handle-spawn relay-curl (relayer (metadata type/webm)) (metadata type/webm) root-curl)
       (handle-spawn (make-curl (uuid)) (video-reader/encoder "/dev/video0" 1280 720) (metadata produces/webm) relay-curl)
       
       (ask/send request-thread "SPAWN" remoteroot command-center-gui
                 #:metadata (metadata is/gui) #:reply root-curl)       
       (ask/send request-thread "SPAWN" remoteroot (video-decoder/gui 1280 720) 
                 #:metadata (metadata accepts/webm) #:reply relay-curl)])

(cond [(argsassoc "--audio")
       (define relay-curl (make-curl (uuid)))
       (handle-spawn relay-curl (relayer (metadata type/speex)) (metadata type/speex) root-curl)
       
       (handle-spawn (make-curl (uuid)) (audio-reader/speex-encoder 3 relay-curl) (metadata produces/speex) root-curl)
       
       (ask/send request-thread "SPAWN" remoteroot (speex-decoder 640)
                 #:metadata (metadata accepts/speex) #:reply relay-curl)])

(spawn (interpreter))

(no-return)