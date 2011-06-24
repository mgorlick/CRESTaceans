;; A wrapper around the SISC record primitives.
;; Michael M. Gorlick 2009.03.04

(import record)

(define (record-synopsis r)
  (let ((t (record-type r)))
    (cons (record-type-name t) (record-type-field-tags t))))

;; flavor - the record-type of the the record
;; tag - the record field for which we want a record index
;; Returns the record index of the given tag.
(define (%field-index flavor tag)
  (let loop ((i 0)
	     (tags (record-type-field-tags flavor)))
    (cond
     ((null? tags)
      (error "record type ~a has no field ~a" (record-type-name flavor) tag))

     ((eq? tag (car tags))
      i)

     (else
      (loop (+ i 1) (cdr tags))))))
