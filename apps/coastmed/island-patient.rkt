#lang racket/base

(require COAST
         "utils.rkt"
         "bindings-extensions.rkt")

(provide PUBLIC-CURL)


(define ISLAND-ADDRESS 
  (island/address/new #"www.johndoe.com" #"127.0.0.2" 5001))
(define-values (ROOT ROOT-LOCATIVE PUBLIC-LOCATIVE PUBLIC-CURL COMM-thd)
  (listen-on/make-root ISLAND-ADDRESS))
(displayln (format "Island starting: ~s~n" (this/island)))

; Set the authority for the ROOT actor.
(locative/cons/authority! ROOT-LOCATIVE (list ROOT))
(locative/curl/authority! ROOT-LOCATIVE (list ROOT))

(actor/jumpstart ROOT 
                 (λ ()
                   
                   ;;spawn actor ArequestEHR
                   (let-values ([(ArequestEHR LrequestEHR @Arequest) (spawn-actor 'ArequestEHR ROOT (list ROOT) (list ROOT))])
                     
                     ; defines the binding environment βrequestEHR for actor Arequest
                     (let ((βrequestEHR (pairs/environ (environ/merge BASELINE MESSAGES/RECEIVE) (global-defines format))))
                       
                       ;jumpstart actor A_ArequestEHR with binding environment B_hello
                       (eval-actor ArequestEHR execute βrequestEHR))
                     (displayln "actor ArequestEHR spawned"))
                   
                   ;;spawn actor Akeychain
                   (define-values (Akeychain Lkeychain @Akeychain) (spawn-actor 'Akeychain ROOT (list ROOT) (list ROOT)))
                   
                   ; defines the binding environment βkeychain for actor Akeychain
                   (let ([βkeychain (pairs/environ (environ/merge BASELINE MESSAGES/RECEIVE) (global-defines format curl?))])
                     
                     ;jumpstart actor A_ArequestEHR with binding environment Bkeychain
                     (eval-actor Akeychain executeKeychain βkeychain))
                   (displayln "actor Akeychain spawned")
                   
                   ; executes procedure "execute" to allow root to receive messages
                   (motile/call execute (pairs/environ (environ/merge BASELINE MESSAGES/RECEIVE) (global-defines format curl? curl/send @Akeychain)))))



(motile/procedure execute
                  (lambda()
                    (let loop ((message (thread-receive)))
                      (cond
                        ((delivery? message); The message is #(curl thunk).
                         (let ((contents (delivery/contents-sent message)))
                           (cond
                             ((procedure? contents)
                              (contents))
                             ((curl? contents)
                              (display "GOT HERE")
                              (curl/send @Akeychain contents))                             
                             (else (displayln "message discarded due to unknown contents")))
                           (loop (thread-receive)))
                         (else 
                          (displayln "message discarded due to unknown contents"))
                         (loop (thread-receive)))))))



(motile/procedure executeKeychain
                  (lambda()
                    (let ((keychain hash/eq/null)) ;;;keychain datastructure
                      (let loop ((message (thread-receive)))
                        (cond
                          ((curl? message)
                           (displayln "YES CONTENT IS A CURL")
                           (let ((metadata (curl/get-meta contents)))
                             (hash/cons keychain (hash/ref metadata 'keychain #f) contents)
                             (displayln "Patient receiving curl and storing in its keychain")))
                          (else (displayln "message discarded due to unknown contents")))
                        (loop (thread-receive))))))