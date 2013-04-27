#lang racket/base

(require COAST
         "utils.rkt"
         "binding_env/Bresearcher.rkt"
         "binding_env/bindings-extensions.rkt")

(provide PUBLIC-CURL)


(define ISLAND-ADDRESS 
  (island/address/new #"www.uci.edu/clinicalResearch" #"127.0.0.2" 5003))
(define-values (ROOT ROOT-LOCATIVE PUBLIC-LOCATIVE PUBLIC-CURL COMM-thd)
  (listen-on/make-root ISLAND-ADDRESS))
(displayln (format "Island starting: ~s~n" (this/island)))

; Set the authority for the ROOT actor.
(locative/cons/authority! ROOT-LOCATIVE (list ROOT))
(locative/curl/authority! ROOT-LOCATIVE (list ROOT))


(motile/procedure 
 execute
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



(motile/procedure 
 executeRequestEHR
 (lambda()
   
   (sleep 6)
   
   ;creates a curl for this actor based on it's orginal locative
   (let* ((@me-metadata (hash/cons hash/eq/null 'reply displayln))
          (loc-exp-date (expiration/date->milliseconds 31 12 2020 11 59 0))
          (@me (curl/new/any (locative/cons/any (this/locative) loc-exp-date 10 #t #t) null @me-metadata)))
     
     ;requesting Akeychain for the service CURL of the EHR system at Iehr and invoking the get-all-patients-records-anonymized service
     (and (curl/send 
           @Akeychain 
           (lambda() 
             (let ((@EHR (retrieve-curl 'hospitalEHR)))
               (curl/send @EHR 
                          (lambda() (curl/send @me (format "\nActor ArequestEHR requesting service curl from actor Akeychain (island www.uci.edu) and using the curl to retrieve all patients' ANONYMIZED electronic health records from hospital service. Response is:\n ~a" (get-all-patients-records-anonymized)))))))))
     
     
     
     ;requesting Akeychain for the service CURL of the EHR system at Iehr an sending a custom computation
     (let ((custom-computation 
            (lambda(stoday);today's date in format e.g., "20130206" (assumes I don't have date library available as a service
              (let* ((age 
                      (lambda(birth)
                        (let* ((sbirth (number->string birth))
                               (s->n/sub (lambda(s start end) (string->number (substring s start end ))))
                               (birthdays (+ (* (s->n/sub sbirth 0 4) 365) 
                                             (* (s->n/sub sbirth 4 6) 30) 
                                             (s->n/sub sbirth 6 8)))
                               (todaydays (+ (* (s->n/sub stoday 0 4) 365) 
                                             (* (s->n/sub stoday 4 6) 30) 
                                             (s->n/sub stoday 6 8))))
                          (/ (- todaydays birthdays) 365))))
                     (ehr-persistent-vec (map (lambda(x) (vector->vector/persist x))
                                              (get-all-patients-records-anonymized)))
                     
                     (ages-list (map (lambda(x) (age (vector/ref x 0))) ehr-persistent-vec)))
              (floor (/ (foldr + 0 ages-list) (length ages-list)))))))
       
       (and (curl/send 
             @Akeychain 
             (lambda() 
               (let ((@EHR (retrieve-curl 'hospitalEHR)))
                 (curl/send @EHR 
                            (lambda() (curl/send @me (format "\nActor ArequestEHR requesting service curl from actor Akeychain (island www.uci.edu) and using the curl to customize the available service to obtain the average age of all patients. The response is: ~a" (custom-computation "20130328"))))))))))
     
     
     
     ;requesting Akeychain for the service CURL of the EHR system at Iehr an attempting to access a non authorized service              
     (and (curl/send 
           @Akeychain 
           (lambda() 
             (let ((@EHR (retrieve-curl 'hospitalEHR)))
               (curl/send @EHR 
                          (lambda() (curl/send @me (get-all-patients-records)))))))
          (displayln "\nActor ArequestEHR requesting service curl from actor Akeychain (island www.uci.edu) and using the curl to retrieve all patients' electronic health records (non anonymized) from hospital service. This operation should produce an error since user is not authorized to such service:\n")))
   
   
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
                                  (displayln (format "Researcher at UCI receiving curl and storing in its keychain with key ~a \n" key)))))
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
                   (displayln "actor Akeychain spawned at island www.uci.edu\n")
                   
                   ;;spawn actor ArequestEHR
                   (let-values ([(ArequestEHR LrequestEHR @Arequest) (spawn-actor 'ArequestEHR ROOT (list ROOT) (list ROOT))])
                     ;jumpstart actor A_ArequestEHR with binding environment βrequestEHR
                     (eval-actor ArequestEHR executeRequestEHR 
                                 (pairs/environ βrequestEHR (global-defines @Akeychain))))
                   (displayln "actor ArequestEHR spawned at island www.uci.edu\n")
                   
                   ; executes procedure "execute" to allow root to receive messages
                   (motile/call execute (pairs/environ 
                                         (environs/merge BASELINE MESSAGES/SEND MESSAGES/RECEIVE)
                                         (global-defines format curl? @Akeychain)))))



