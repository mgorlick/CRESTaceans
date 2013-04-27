#lang racket/base

(require COAST
         "utils.rkt"
         "binding_env/Behr.rkt"
         "binding_env/bindings-extensions.rkt"
         (only-in "island-patient.rkt" [PUBLIC-CURL P-PUBLIC-CURL])
         (only-in "island-staff.rkt" [PUBLIC-CURL S-PUBLIC-CURL])
         (only-in "island-researcher.rkt" [PUBLIC-CURL R-PUBLIC-CURL]))


(define ISLAND-ADDRESS 
  (island/address/new #"www.hospital.com" #"127.0.0.1" 5000))

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
                             (else (displayln (format "~a: message discarded due to unknown contents" (actor/nickname this/actor))))))
                         (loop (thread-receive)))
                        (else 
                         (displayln (format "~a: message discarded due to unknown contents" (actor/nickname this/actor)))
                         (loop (thread-receive)))))))


(actor/jumpstart ROOT 
                 (λ ()
                   
                   ;;spawn and jumpstart actor Apatient with binding environment βpatientServices
                   (define-values (Apatient Lpatient @Apatient) 
                     (spawn-actor 'Apatient ROOT (list ROOT) (list ROOT)))
                   (and (eval-actor Apatient execute βpatientServices)
                        (displayln "actor Apatient spawned in hospital island\n"))
                   
                   ;;spawn and jumpstart actor Astaff with binding environment βstaffServices
                   (define-values (Astaff Lstaff @Astaff) 
                     (spawn-actor 'Astaff ROOT (list ROOT) (list ROOT)))
                   (and (eval-actor Astaff execute βstaffServices)
                        (displayln "actor Astaff spawned in hospital island\n"))
                   
                   ;;spawn actor  jumpstart actor Aresearcher with binding environment βresearcherServices
                   (define-values (Aresearcher Lresearcher @Aresearcher) 
                     (spawn-actor 'Aresearcher ROOT (list ROOT) (list ROOT)))
                   (and (eval-actor Aresearcher execute βresearcherServices)
                        (displayln "actor Aresearcher spawned in hospital island\n"))
                   
                   ;create a personalized curl for patient with patient_id = 2 and send it to the patient
                   (let* ((loc-exp-date (expiration/date->milliseconds 31 12 2020 11 59 0))
                          (LpatientNew (locative/cons Lpatient loc-exp-date 1000 #t #t))
                          (metadata (hash/new hash/eq/null 'keychain 'hospitalEHR 'patient_id 2))
                          (@ApatientJD (curl/new LpatientNew null metadata 6000000)))
                     (and (curl/send P-PUBLIC-CURL @ApatientJD)
                          (displayln "ROOT at EHR island: sending service curl to patient\n")))
                   
                   
                   ;create a personalized curl for staff and send it to Dr Doe 
                   (let* ((loc-exp-date (expiration/date->milliseconds 31 12 2020 11 59 0))
                          (LstaffNew (locative/cons Lstaff loc-exp-date 1000 #t #t))
                          (metadata (hash/new hash/eq/null 'keychain 'hospitalEHR))
                          (@AstaffDrDoe (curl/new LstaffNew null metadata 6000000)))
                     (and (curl/send S-PUBLIC-CURL @AstaffDrDoe)
                          (displayln "ROOT at EHR island: sending service curl to Dr Doe (staff)\n")))
                   
                   
                   ;create a personalized curl for reaserchers and send it to researcher at UCI 
                   (let* ((loc-exp-date (expiration/date->milliseconds 31 12 2020 11 59 0))
                          (LresearcherNew (locative/cons Lresearcher loc-exp-date 1000 #t #t))
                          (metadata (hash/new hash/eq/null 'keychain 'hospitalEHR))
                          (@AresearcherUCI (curl/new LresearcherNew null metadata 60000000)))
                 
                     (and (curl/send R-PUBLIC-CURL @AresearcherUCI)
                          (displayln "ROOT at EHR island: sending service curl to researcher at UCI\n")))
                   
                   ; executes procedure "execute" to allow root to receive messages
                   (motile/call execute (pairs/environ (environ/merge BASELINE MESSAGES/RECEIVE) (global-defines format)))))