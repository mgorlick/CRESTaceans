#lang racket/base

(require db/sqlite3
         db/base)

(provide (all-defined-out))

(define conn-ehrdb (kill-safe-connection (sqlite3-connect #:database "ehr" #:mode 'read/write)))

(define (get-all-patients-records)
  (query-rows conn-ehrdb "select * from patients LEFT JOIN patient_procedures ON patients.patient_id=patient_procedures.patient_id LEFT JOIN patient_immunization_records ON patient_procedures.patient_id=patient_immunization_records.patient_id"))

(define (get-all-patients-records-anonymized)
  (query-rows conn-ehrdb "SELECT * FROM view_anonimized_patients_records"))
  
(define (get-patient-record patient_id)
  (query-row conn-ehrdb "select * from patients, patient_procedures, patient_immunization_records where patients.patient_id = ? and  patient_procedures.patient_id = ? and patient_immunization_records.patient_id = ?" patient_id))