#lang racket/base

(require COAST
         "bindings-extensions.rkt"
         db/sqlite3
         db/base)

(provide (all-defined-out))

(define conn-ehrdb (kill-safe-connection (sqlite3-connect #:database "ehr" #:mode 'read/write)))

(define (get-all-patients-records)
  (query-rows conn-ehrdb "select * from patients LEFT JOIN patient_procedures ON patients.patient_id=patient_procedures.patient_id LEFT JOIN patient_immunization_records ON patient_procedures.patient_id=patient_immunization_records.patient_id"))

(define (get-all-patients-records-anonymized)
  (query-rows conn-ehrdb "SELECT * FROM view_anonimized_patients_records"))

(define (get-patient-record patient_id)
  (query-row conn-ehrdb "select * from patients, patient_procedures, patient_immunization_records where patients.patient_id = $1 and  patient_procedures.patient_id = $1 and patient_immunization_records.patient_id = $1" patient_id))

(define (get-my-record curl)
  (get-patient-record (hash/ref (curl/get-meta curl) 'patient_id #f)))

;;transform a vector into a motile persistent vector
(define (vector->vector/persist vec)
  (list/vector vector/null (vector->list vec)))

; defines the binding environment βhospital for actor Ahospital
(define βstaffServices (pairs/environ (environs/merge BASELINE MESSAGES/SEND MESSAGES/RECEIVE DISPLAYING) 
                                 (global-defines get-all-patients-records get-patient-record vector->vector/persist)))

; defines the binding environment βArequestEHR for actor Arequest
(define βpatientServices (pairs/environ (environs/merge BASELINE MESSAGES/SEND MESSAGES/RECEIVE DISPLAYING) (global-defines get-my-record vector->vector/persist)))

; defines the binding environment βAresearcher for actor Aresearcher
(define βresearcherServices (pairs/environ (environs/merge BASELINE MESSAGES/SEND MESSAGES/RECEIVE DISPLAYING) 
                                   (global-defines get-all-patients-records-anonymized vector->vector/persist)))