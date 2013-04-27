#lang racket/base

(require COAST
         "utils.rkt"
         "binding_env/Bpatient.rkt"
         "binding_env/bindings-extensions.rkt")

(provide PUBLIC-CURL)


(define ISLAND-ADDRESS 
  (island/address/new #"www.johndoe.com" #"127.0.0.2" 5001))
(define-values (ROOT ROOT-LOCATIVE PUBLIC-LOCATIVE PUBLIC-CURL COMM-thd)
  (listen-on/make-root ISLAND-ADDRESS))
(displayln (format "Island starting: ~s~n" (this/island)))

; Set the authority for the ROOT actor.
(locative/cons/authority! ROOT-LOCATIVE (list ROOT))
(locative/curl/authority! ROOT-LOCATIVE (list ROOT))


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
                              (curl/send @Akeychain contents))                             
                             (else (displayln "message discarded due to unknown contents")))
                           (loop (thread-receive)))
                         (else 
                          (displayln "message discarded due to unknown contents"))
                         (loop (thread-receive)))))))



(motile/procedure executeRequestEHR
                  (lambda()
                    
                    (sleep 2)
                    
                    ;creates a curl for this actor based on it's orginal locative
                    (let* ((@me-metadata (hash/cons hash/eq/null 'reply displayln))
                           (@me (curl/new/any (locative/cons/any (this/locative) 2.76e110 1 #t #t) null @me-metadata)))
                      
                      ;requesting Akeychain for the service CURL of the EHR system at Iehr
                      (and (curl/send @Akeychain (lambda() 
                                              (let ((@EHR (retrieve-curl 'hospitalEHR)))
                                                (curl/send @EHR 
                                                           (lambda() (curl/send @me 
                                                                                (get-my-record @EHR)))))))
                           (displayln "\nActor ArequestEHR requesting service curl from actor Akeychain (island www.johndoe.com) and using the curl to retrieve personal electronic health record from hospital service. Response is:")))
                    
                    
                    (let loop ((message (thread-receive)))
                      (cond
                        ((delivery? message); The message is #(curl thunk)
                         (let ((contents (delivery/contents-sent message))
                               (metadata (curl/get-meta (delivery/curl-used message))))
                           (cond
                             ((hash/contains? metadata 'reply)
                              (apply (hash/ref metadata 'reply #f) 
                                     (list contents)))                            
                             (else (displayln "message discarded due to unknown contents")))
                           (loop (thread-receive)))
                         (else 
                          (displayln "message discarded due to unknown contents"))
                         (loop (thread-receive)))))))



(motile/procedure executeKeychain
                  (lambda()
                    (let loop ((message (thread-receive)))
                      (let ((contents (delivery/contents-sent message)))
                        (cond
                          ((curl? contents)
                           (let* ((metadata (curl/get-meta contents))
                                  (key (hash/ref metadata 'keychain #f)))
                             (and (store-curl key contents)
                                  (displayln (format "Patient John Doe receiving curl and storing in its keychain with key ~a \n" key)))))
                          ((procedure? contents)
                           (contents))
                          (else (displayln "message discarded due to unknown contents")))
                        (loop (thread-receive))))))


(actor/jumpstart ROOT 
                 (λ ()
                   (PROMISSARY ROOT)
                   
                   ;;spawn actor Akeychain
                   (define-values (Akeychain Lkeychain @Akeychain) (spawn-actor 'Akeychain ROOT (list ROOT) (list ROOT)))
                   ;jumpstart actor A_ArequestEHR with binding environment Bkeychain
                   (eval-actor Akeychain executeKeychain βkeychain)
                   (displayln "actor Akeychain spawned at island www.johndoe.com\n")
                   
                   ;;spawn actor ArequestEHR
                   (let-values ([(ArequestEHR LrequestEHR @Arequest) (spawn-actor 'ArequestEHR ROOT (list ROOT) (list ROOT))])
                     ;jumpstart actor A_ArequestEHR with binding environment βrequestEHR
                     (eval-actor ArequestEHR executeRequestEHR 
                                 (pairs/environ βrequestEHR
                                                (global-defines @Akeychain curl/new/any locative/cons/any this/locative))))
                   (displayln "actor ArequestEHR spawned at island www.johndoe.com\n")
                   
                   ; executes procedure "execute" to allow root to receive messages
                   (motile/call execute (pairs/environ 
                                         (environs/merge BASELINE MESSAGES/SEND MESSAGES/RECEIVE)
                                         (global-defines format curl? @Akeychain)))))



