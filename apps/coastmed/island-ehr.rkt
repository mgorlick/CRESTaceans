#lang racket/base

(require COAST
         "utils.rkt"
         "bindings.rkt"
         "bindings-extensions.rkt"
         (only-in "island-patient.rkt" [PUBLIC-CURL P-PUBLIC-CURL]))


(define ISLAND-ADDRESS 
  (island/address/new #"www.ehr.com" #"127.0.0.1" 5000))

(define-values (ROOT ROOT-LOCATIVE PUBLIC-LOCATIVE PUBLIC-CURL COMM-thd)
  (listen-on/make-root ISLAND-ADDRESS))
(displayln (format "Island starting: ~s~n" (this/island)))

; Set the authority for the ROOT actor.
(locative/cons/authority! ROOT-LOCATIVE (list ROOT))
(locative/curl/authority! ROOT-LOCATIVE (list ROOT))

(actor/jumpstart ROOT 
                 (λ ()
                   
                   ;;spawn actor Apatient
                   (define-values (Apatient Lpatient @Apatient) (spawn-actor 'Apatient ROOT (list ROOT) (list ROOT)))
                     ; defines the binding environment βArequestEHR for actor Arequest
                     (let ((βpatient (pairs/environ (environ/merge BASELINE MESSAGES/RECEIVE) (global-defines format))))
                     ;jumpstart actor ArequestEHR with binding environment βpatient
                     (eval-actor Apatient execute βpatient))
                     (displayln "actor Apatient spawned")
                   
                   ;;spawn actor Ahospital
                   (let-values ([(Ahospital Lhospital @Ahospital) (spawn-actor 'Ahospital ROOT (list ROOT) (list ROOT))])
                     ; defines the binding environment βhospital for actor Ahospital
                     (let ((βhospital (pairs/environ (environ/merge BASELINE MESSAGES/RECEIVE) (global-defines format get-all-patients-records get-patient-record))))
                     ;jumpstart actor Ahospital with binding environment hospital
                     (eval-actor Ahospital execute βhospital)))
                     (displayln "actor Ahospital spawned")
                   
                   ;;spawn actor Aresearcher
                   (let-values ([(Aresearcher Lresearcher @Aresearcher) (spawn-actor 'Aresearcher ROOT (list ROOT) (list ROOT))])
                     ; defines the binding environment βAresearcher for actor Aresearcher
                     (let ((βresearcher (pairs/environ (environ/merge BASELINE MESSAGES/RECEIVE) (global-defines format get-all-patients-records-anonymized))))
                     ;jumpstart actor Aresearcher with binding environment βresearcher
                     (eval-actor Aresearcher execute βresearcher)))
                     (displayln "actor Aresearcher spawned")
                     
        ;create a personalized curl for patient with patient_id = 2 and send it to the patient
                   (let* ((LpatientNew (locative/cons Lpatient 3.15569e7 10000 #t #t))
                          (metadata (hash/new hash/eq/null 'keychain 'hospitalEHR 'patient_id 2))
                          (@ApatientJD (curl/new Lpatient null metadata 6000000)))

                     (cond
                       ((and #t (curl/send P-PUBLIC-CURL @ApatientJD))
                        (displayln "ROOT at EHR island: sending curl to patient"))))
                   
                     ; executes procedure "execute" to allow root to receive messages
                     (motile/call execute (pairs/environ (environ/merge BASELINE MESSAGES/RECEIVE) (global-defines format)))))



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