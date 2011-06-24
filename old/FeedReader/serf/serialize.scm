;; Copyright 2009 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Wrapper around SISC serialization primitives for URL-encoded serializations.
;; Low-level support includes translating SISC buffers to/from Java byte[].

(define-java-class <java-base64> |base64.Base64|)
(define :null-java-base64:)
(define-generic-java-method encode-bytes)
(define-generic-java-method decode)
(define :url-safe-option: (->jint 16))

;; Returns Scheme object x as an Base64 URL-encoded Java String.
(define (buffer/base64 b)
  (encode-bytes :null-java-base64: (buffer/java-byte-array b) :url-safe-option:))

;; Decodes a Base64 URL-encoded Java String s to a Scheme buffer.
(define (base64/buffer s)
  (let ((array (decode :null-java-base64: s))) ; String s decoded to Java byte[].
    (java-byte-array/buffer array))) ; byte[] array converted to Scheme Buffer.

;; Round trip serialization/deserialization via base64 URL encoding.
(define (serialize/test/01 x)
  (let* ((b (serialize/from x)) ; Scheme object x to Scheme Buffer
	 (safe (buffer/base64 b)) ; Scheme Buffer to base64 url-safe Java String.
	 (c (base64/buffer safe)) ; Base64 url-safe Java String back to Scheme Buffer.
	 (y (deserialize/from c))) ; Scheme Buffer to copy of Scheme object x.
    y))

;; Converts buffer b to a list of small integers.
(define (buffer/list b)
  (call-with-input-buffer
   b
   (lambda (p)
     (let loop ((outcome '())
		(i (read-byte p)))
       (cond
	((eof-object? i) (reverse outcome))
	(else (loop (cons i outcome) (read-byte p))))))))

;; Serialize an arbitrary Scheme object x into a buffer (equivalent to a Gambit-C u8vector).
(define (serialize/from x)
  (let* ((b   (open-output-buffer))         ; Create a buffer output port.
	 (out (open-serial-output-port b))) ; Create a serial output port from the buffer.
    (serialize x out) ; Serialize object x.
    (flush-output-port out) ; Paranoia.
    (close-output-port out) ; Paranoia.
    (get-output-buffer b))) ; Extract a real buffer from the buffer output port.

;; Deserialize the object contained in buffer b.
(define (deserialize/from b)
  (deserialize (open-serial-input-port (open-input-buffer b))))

;; The following two conversion routines are hideously expensive
;; and stupid but will do for now.
;; Reimplement them as  native library functions.
(define (buffer/java-byte-array b)
  (let* ((n (buffer-length b))
	 (array (java-array-new <jbyte> n)))
    (let loop ((i 0))
      (cond
       ((< i n)
	(java-array-set! array i (->jbyte (buffer-ref b i)))
	(loop (+ i 1)))
       (else array)))))

(define (java-byte-array/buffer a)
  (let* ((n (java-array-length a))
	 (buffer (make-buffer n 0)))
    (let loop ((i 0))
      (cond
       ((< i n)
	(buffer-set! buffer i (->number (java-array-ref a i)))
	(loop (+  i 1)))
       (else buffer)))))

(set! :null-java-base64: (java-null <java-base64>))
  





   
  