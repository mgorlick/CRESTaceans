#! /usr/bin/env racket
#lang racket/base

(require "motiles.rkt"
         "environs.rkt"
         "../../peer/src/api/api.rkt"
         racket/match
         racket/function)
(provide (all-defined-out))

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
         [(ask "SPAWN" _ body metadata reply echo)
          (define curl (make-curl (uuid)))
          (handle-spawn curl body metadata reply)
          (ask/send request-thread "POST" reply curl
                    #:metadata (make-metadata `(is . curl)
                                              `(in-response-to . ,echo)))]
         
         [(ask _ (? (is? root-curl) u) _ _ _ _)
          'foo]
         
         [(ask method u body metadata reply echo)
          (cond [(valid-actor? u)
                 (thread-send (hash-ref curls=>threads u) 
                              (cons (start-program body (metadata->benv metadata)) v))]
                [else (printf "error: not a thread or not running: ~a at ~a / directed to: ~a~n" 
                              method (hash-ref curls=>threads u #f) u)])]
         
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
  
  ; clean-up-spawn: when a spawn dies remove its entries from the registry
  (define (clean-up-spawn curl)
    (hash-remove! curls=>threads curl)
    (hash-remove! curls=>motiles curl)
    (hash-remove! curls=>metadata curl)
    (hash-remove! curls=>replycurls curl))
  
  ; handle-spawn: message/uri? any any message/uri? -> void
  ; when a spawn comes in, assign it a binding environment and set of standard arguments
  ; based on the metadata attached. then keep track of it in the above hashes
  (define (handle-spawn curl body metadata reply)
    (define benv (metadata->benv metadata))
    (define args
      (cond [(assoc "spawnargs" metadata)
             (displayln "Spawner: selecting custom metadata-specified call convention")
             (cdr (assoc "spawnargs" metadata))]
            [(contains-any? metadata 
                            accepts/webm produces/webm accepts/speex produces/speex
                            is/gui)
             (displayln "Spawner: selecting reply call convention")
             (list reply)]
            [else
             (displayln "Spawner: selecting null call convention")
             null]))
    (printf "USING THESE ARGS: ~a~n" args)
    (hash-set! curls=>threads curl (spawn 
                                    (parameterize ([current-curl curl])
                                      (with-handlers ([exn:fail? (λ (e) (printf "Spawn died [~a]: ~a~n"
                                                                                metadata e))])
                                        (start-program body benv args))
                                      (printf "Spawn quitting cleanly [~a]~n" metadata)
                                      (clean-up-spawn curl))))
    (hash-set! curls=>motiles curl body)
    (hash-set! curls=>metadata curl metadata)
    (hash-set! curls=>replycurls curl reply)
    (printf "a new actor installed at ~n\t~s~n\t[metadata: ~a]~n" curl metadata))
  
  ;; ----------
  ;; State-specific functions provided to actors (and chieftain)
  ;; ----------
  
  ; primitive for sending from the Motile level. 
  ; thread-to-thread intra-island messaging
  ; looks the same as island-to-island messaging does.
  (define (ask/send* method u body [metadata :no-metadata:] [rpy (current-curl)])
    (cond [(hash-has-key? curls=>threads u)
           ;; precompiled motile procedures don't need to be recompiled, but
           ;; other values (e.g., literals, data structures, ....) do need to be recompiled and called
           ;; (to escape the enclosing thunk).
           (define to-send (if (procedure? body)
                               body
                               (motile/call (motile/compile body) (metadata->benv metadata))))
           ;; similarly, the attached copy of the payload only needs to be recompiled
           ;; if it's not already a compiled procedure.
           (define to-attach (if (procedure? body)
                                 body
                                 (motile/compile body)))
           ;; for our purposes, reply address always == current curl is good enough.
           (thread-send (hash-ref curls=>threads u)
                        (cons to-send (message/ask/new method u  to-attach metadata rpy :no-echo:))
                        #f)]
          [(message/uri? u)
           ; it's going off-Island.
           (ask/send request-thread method u body 
                     #:metadata metadata #:compile? (not (procedure? body)) #:reply rpy)]
          [else (displayln "Unknown delivery target:")
                (displayln u)]))
  
  ; a key piece of the ad hoc protocol we've set up:
  ; when a SPAWN message comes in, name the original reply-to CURL
  ; as the new reply-to recipient for the SPAWN relay.
  ; also package the original metadata if no metadata is used to override it.
  (define (respawn-self host port 
                        [meta (hash-ref curls=>metadata (current-curl))]
                        [rpy (hash-ref curls=>replycurls (current-curl))])
    (ask/send request-thread "SPAWN" (message/uri/new #f (cons host port) "/")
              (hash-ref curls=>motiles (current-curl))
              #:metadata meta
              #:reply rpy
              #:compile? #f ; never compile. by definition a spawned and stored Motile is already compiled
              ))
  
  ; some actors can ask for the curl identifying themselves.
  (define current-curl (make-parameter root-curl))
  
  ; some actors can ask for the root chieftain.
  ; giving this URL out is a nontrivial decision since it means that 
  ; anyone who can send messages may also send a SPAWN or REMOTE to the chieftain.
  (define (get-root-curl)
    root-curl)
  
  ;; the binding environs we'll use to parameterize actors spawned on this island
  (define MULTIMEDIA-BASE* 
    (++ MULTIMEDIA-BASE (global-defines current-curl ask/send*)))
  (define VIDEO-ENCODE* 
    (++ VIDEO-ENCODE    (global-defines current-curl ask/send* respawn-self)))
  (define AUDIO-ENCODE* 
    (++ AUDIO-ENCODE    (global-defines current-curl ask/send* respawn-self)))
  (define VIDEO-DECODE* 
    (++ VIDEO-DECODE    (global-defines current-curl ask/send* respawn-self get-root-curl)))
  (define GUI* 
    (++ GUI             (global-defines current-curl ask/send* respawn-self get-root-curl)))
  (define AUDIO-DECODE* 
    (++ AUDIO-DECODE    (global-defines current-curl ask/send* respawn-self get-root-curl)))
  
  ;; ---------
  ;; Finally, spawn any initial actors if the command-line arguments request them
  ;; ---------
  
  (thread
   (λ ()
     (let loop ()
       (define cmd (read))
       (match cmd
         [`(cp ,uuid ,h ,p)
          (define to-respawn (make-curl uuid))
          (ask/send* "POST" to-respawn `(CP ,(symbol->string h) ,p))]
         [`(mv ,uuid ,h ,p)
          (define to-respawn (make-curl uuid))
          (ask/send* "DELETE" to-respawn `(Quit/MV ,(symbol->string h) ,p))])
       (loop))))
  
  (define remoteroot
    (remote-curl-root #f (argsassoc "--rhost" #:no-val *LOCALHOST*)
                      (argsassoc "--rport" #:no-val 1235 #:call string->number)))
  
  (cond [(argsassoc "--video")
         (define device (argsassoc "--video" #:default "/dev/video0"))
         (define bang! (big-bang remoteroot device 800 600 root-curl))
         
         (handle-spawn (make-curl (uuid)) command-center-gui (make-metadata is/gui) root-curl)
         (handle-spawn (make-curl (uuid)) bang! (make-metadata) root-curl)])
  
  (no-return))

(go!)