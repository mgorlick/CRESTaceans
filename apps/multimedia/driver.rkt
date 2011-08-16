#! /usr/bin/env racket
#lang racket/base

(require "misc.rkt"
         "motiles.rkt"
         "environs.rkt"
         "gui.rkt"
         "../../peer/src/net/tcp-peer.rkt"
         "../../peer/src/api/compilation.rkt"
         racket/match
         racket/function
         racket/list)
(provide (all-defined-out))

;; -------
;; Helpers
;; -------

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

; contains-any?: returns a non-false value iff any argument beyond the first is present in the first.
; list any -> any
(define (contains-any? mlst . vs)
  (and (list? mlst) (ormap (curryr member mlst) vs)))

(define *LISTENING-ON* (argsassoc "--host" #:no-val *LOCALHOST*))
(define *LOCALPORT* (argsassoc "--port" #:no-val 5000 #:call string->number))

(define (go!)
  ;; ----------
  ;; Clan state
  ;; ----------
  
  ; the "chieftain" at 'root': hardcoded to receive all messages and then dispatches them as necessary 
  (define (handler)
    (spawn
     (let loop ()
       (define v (thread-receive))
       (match v
         [(ask "SPAWN" (? (is? root-curl) u) body metadata reply echo)
          (define curl (make-curl (uuid)))
          (handle-spawn curl body metadata reply)]
         
         [(ask method u body metadata reply echo)
          (if (valid-actor? u)
              (thread-send (hash-ref curls=>threads u) 
                           (cons (start-program body (metadata->benv metadata)) v))
              (printf "error: not a thread or not running: ~a / directed to: ~a~n" 
                      (hash-ref curls=>threads u #f) u))]
         
         [else (printf "Message not recognized: ~a~n" else)])
       (loop))))
  
  ;; set up the listening server.
  (define k (generate-key/defaults))
  (define this-scurl (generate-scurl/defaults *LISTENING-ON* *LOCALPORT* #:key k))
  (define request-thread (run-tcp-peer *LISTENING-ON* *LOCALPORT* this-scurl (handler)))
  
  ; functions to use for manufacturing new CURLs naming actors on THIS ISLAND.
  ; root-curl identifies the chieftain.
  (define make-curl (curry message/uri/new #f (cons *LISTENING-ON* *LOCALPORT*)))
  (define root-curl (make-curl "/"))
  (printf "listening on ~s~n" root-curl)
  
  ; these hash tables hold necessary values for tracking local state
  ; and if necessary moving it to another island.
  (define curls=>threads (make-hash)) ; dispatch on the actual running 
  (define curls=>motiles (make-hash)) ; save COMPILED motiles for retransmission later
  (define curls=>metadata (make-hash)) ; save metadata for retransmission
  (define curls=>replycurls (make-hash)) ; save corresponding reply curls for retransmission
  
  ; a stored actor thread reference is valid for receiving messages if it's running.
  (define (valid-actor? u)
    (define t (hash-ref curls=>threads u #f))
    (and (thread? t) (thread-running? t)))
  
  ; metadata->benv: assign a binding environment to a spawned actor based on the metadata tagged
  (define (metadata->benv metadata)
    (cond
      [(contains-any? metadata accepts/webm) VIDEO-DECODE*]
      [(contains-any? metadata accepts/speex) AUDIO-DECODE*]
      [(contains-any? metadata produces/webm) VIDEO-ENCODE*]
      [(contains-any? metadata produces/speex) AUDIO-ENCODE*]
      [(contains-any? metadata is/gui) GUI*]
      [else MULTIMEDIA-BASE*]))
  
  ; handle-spawn: message/uri? any any message/uri? -> void
  ; when a spawn comes in, assign it a binding environment and set of standard arguments
  ; based on the metadata attached. then keep track of it in the above hashes
  (define (handle-spawn curl body metadata reply)
    (define benv (metadata->benv metadata))
    (define args
      (cond [(contains-any? metadata 
                            accepts/webm produces/webm 
                            accepts/speex produces/speex is/gui)
             (list reply)]
            [else null]))
    (parameterize ([current-curl curl])
      (hash-set! curls=>threads curl (spawn (displayln "Starting") (start-program body benv args)))
      (hash-set! curls=>motiles curl body)
      (hash-set! curls=>metadata curl metadata)
      (hash-set! curls=>replycurls curl reply)
      (printf "a new actor installed at ~n\t~s (~a)~n" curl metadata)))
  
  ;; ----------
  ;; State-specific functions provided to actors (and chieftain)
  ;; ----------
  
  ; primitive for sending from the Motile level. 
  ; thread-to-thread intra-island messaging
  ; looks the same as island-to-island messaging does.
  (define (ask/send* method u body metadata)
    (cond [(hash-has-key? curls=>threads u)
           (thread-send (hash-ref curls=>threads u)
                        (cond [(procedure? body)
                               (cons body
                                     (message/ask/new method u body metadata))]
                              [else             
                               (cons (motile/call (motile/compile body) (metadata->benv metadata))
                                     (message/ask/new method u (motile/compile body) metadata))])
                        #f)]
          [(message/uri? u)
           (ask/send request-thread method u body 
                     #:metadata metadata #:compile? (not (procedure? body)))]
          [else (displayln "Unknown delivery target:")
                (displayln u)]))
  
  ; some actors can ask for the curl identifying themselves.
  (define current-curl (make-parameter root-curl))
  
  ; allow an actor to get the current gui curl, set it, neither, or both, depending on binding environment.
  ; `current-gui-curl' itself shouldn't be added to a binding environment.
  (define get-current-gui-curl (procedure-reduce-arity current-gui-curl 0))
  (define set-current-gui-curl! (procedure-reduce-arity current-gui-curl 1))
  (define clear-current-gui-curl! (λ () (current-gui-curl #f)))
  
  ; a key piece of the ad hoc protocol we've set up:
  ; when a SPAWN message comes in, name the original reply-to CURL
  ; as the new reply-to recipient for the SPAWN relay. also package the original metadata.
  (define (respawn u host port)
    (ask/send request-thread "SPAWN"
              (message/uri/new #f (cons host port) "/")
              (hash-ref curls=>motiles u)
              #:metadata (hash-ref curls=>metadata u)
              #:reply (hash-ref curls=>replycurls u)
              #:compile? #f))
  
  ;; the binding environs we'll use to parameterize actors spawned on this island
  (define MULTIMEDIA-BASE* 
    (++ MULTIMEDIA-BASE (global-defines current-curl ask/send*)))
  (define VIDEO-ENCODE* 
    (++ VIDEO-ENCODE    (global-defines current-curl ask/send*)))
  (define AUDIO-ENCODE* 
    (++ AUDIO-ENCODE    (global-defines current-curl ask/send*)))
  (define VIDEO-DECODE* 
    (++ VIDEO-DECODE    (global-defines current-curl ask/send*)))
  (define GUI* 
    (++ GUI             (global-defines current-curl ask/send* respawn)))
  (define AUDIO-DECODE* 
    (++ AUDIO-DECODE    (global-defines current-curl ask/send*)))
  
  ;; ---------
  ;; Finally, spawn any initial actors if the command-line arguments request them
  ;; ---------
  
  (define remoteroot
    (remote-curl-root #f (argsassoc "--rhost" #:no-val *LOCALHOST*)
                      (argsassoc "--rport"#:no-val 1235 #:call string->number)))
  
  (cond [(argsassoc "--video")       
         
         (unless (argsassoc "--no-gui")
           (ask/send request-thread "SPAWN" remoteroot command-center-gui
                     #:metadata (metadata is/gui) #:reply root-curl))
         
         (define relay-curl (make-curl (uuid)))
         (handle-spawn relay-curl relayer (metadata) root-curl)
         (handle-spawn (make-curl (uuid)) 
                       (video-reader/encoder (argsassoc "--video" #:default "/dev/video0") 1280 720) 
                       (metadata produces/webm) relay-curl)
         
         (ask/send request-thread "SPAWN" remoteroot video-decoder/single
                   #:metadata (metadata accepts/webm) #:reply relay-curl)])
  
  (cond [(argsassoc "--audio")
         (define relay-curl (make-curl (uuid)))
         (handle-spawn relay-curl relayer (metadata type/speex) root-curl)
         (handle-spawn (make-curl (uuid)) (audio-reader/speex-encoder 3 relay-curl) 
                       (metadata produces/speex) root-curl)
         
         (ask/send request-thread "SPAWN" remoteroot (speex-decoder 640)
                   #:metadata (metadata accepts/speex) #:reply relay-curl)])
  
  (no-return))

(require profile
         profile/render-text)

(profile-thunk go!
               #:threads #t
               #:delay 1/1000
               #:render (λ (data)
                          (render data #:hide-self 1/10000 #:hide-subs 1/10000)))