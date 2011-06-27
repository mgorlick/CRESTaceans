#lang racket
  ; Provides host-id definitions and utilities for conversion between
  ; string and byte representations.
  
  (require "depends.rkt")
  
  ; The size of a host-id in byte form.
  (define +host-id-bytes-length+ 20)
  
  ; Returns the length of a host-id when in bytes form.
  (define (host-id-bytes-length)
    +host-id-bytes-length+)
  (provide/contract
   [host-id-bytes-length (-> exact-positive-integer?)])
  
  ; Test to see if the given value is a bytes? and is the correct length.
  (define (host-id-bytes? host-id)
    (and (bytes? host-id)
         (= (bytes-length host-id) (host-id-bytes-length))))
  (provide/contract
   [host-id-bytes? (-> any/c boolean?)])
  
  ; The size of a host-id in string form.
  (define +host-id-string-length+ 32)
  
  ; Returns the length of the host-id when in string form.
  (define (host-id-string-length)
    +host-id-string-length+)
  (provide/contract
   [host-id-string-length (-> exact-positive-integer?)])
  
  ; Test to see if the given value is a string? and is the correct length.
  (define (host-id-string? host-id)
    (and (string? host-id)
         (= (string-length host-id) (host-id-string-length))))
  (provide/contract
   [host-id-string? (-> any/c boolean?)])
  
  ; Lookup table for converting from string to byte form.
  ; We leave out the characters 'L', '1', '0', and 'O' because
  ; they look alike which means we are left with
  ; "23456789ABCDEFGHIJKMNPQRSTUVWXYZ"
  (define +string-to-byte-ht+ (make-hash))
  (hash-set! +string-to-byte-ht+ #"2" (bytes 0))
  (hash-set! +string-to-byte-ht+ #"3" (bytes 1))
  (hash-set! +string-to-byte-ht+ #"4" (bytes 2))
  (hash-set! +string-to-byte-ht+ #"5" (bytes 3))
  (hash-set! +string-to-byte-ht+ #"6" (bytes 4))
  (hash-set! +string-to-byte-ht+ #"7" (bytes 5))
  (hash-set! +string-to-byte-ht+ #"8" (bytes 6))
  (hash-set! +string-to-byte-ht+ #"9" (bytes 7))
  (hash-set! +string-to-byte-ht+ #"A" (bytes 8))
  (hash-set! +string-to-byte-ht+ #"B" (bytes 9))
  (hash-set! +string-to-byte-ht+ #"C" (bytes 10))
  (hash-set! +string-to-byte-ht+ #"D" (bytes 11))
  (hash-set! +string-to-byte-ht+ #"E" (bytes 12))
  (hash-set! +string-to-byte-ht+ #"F" (bytes 13))
  (hash-set! +string-to-byte-ht+ #"G" (bytes 14))
  (hash-set! +string-to-byte-ht+ #"H" (bytes 15))
  (hash-set! +string-to-byte-ht+ #"I" (bytes 16))
  (hash-set! +string-to-byte-ht+ #"J" (bytes 17))
  (hash-set! +string-to-byte-ht+ #"K" (bytes 18))
  (hash-set! +string-to-byte-ht+ #"M" (bytes 19))
  (hash-set! +string-to-byte-ht+ #"N" (bytes 20))
  (hash-set! +string-to-byte-ht+ #"P" (bytes 21))
  (hash-set! +string-to-byte-ht+ #"Q" (bytes 22))
  (hash-set! +string-to-byte-ht+ #"R" (bytes 23))
  (hash-set! +string-to-byte-ht+ #"S" (bytes 24))
  (hash-set! +string-to-byte-ht+ #"T" (bytes 25))
  (hash-set! +string-to-byte-ht+ #"U" (bytes 26))
  (hash-set! +string-to-byte-ht+ #"V" (bytes 27))
  (hash-set! +string-to-byte-ht+ #"W" (bytes 28))
  (hash-set! +string-to-byte-ht+ #"X" (bytes 29))
  (hash-set! +string-to-byte-ht+ #"Y" (bytes 30))
  (hash-set! +string-to-byte-ht+ #"Z" (bytes 31))
  
  ; This function converts a host-id that is in string form into a host-id
  ; in byte form.  The string form has 32 characters and each character
  ; correlates to a byte value between 0 and 32.  Convert each character to
  ; a byte and compress the 32 bytes into 20 bytes which can be done because
  ; the upper 3 bytes of each byte should be zero's.
  (define (host-id-string->host-id-bytes host-id)
    ; Define a recursive function 'convert' which translates each character into
    ; it's byte form.
    (letrec ((convert
              (lambda (data)
                (cond
                  [(= (bytes-length data) 0) #""]
                  [else
                   (bytes-append (hash-ref +string-to-byte-ht+ (subbytes data 0 1) #"") (convert (subbytes data 1)))]))))
      ; After converting each character to a byte call compress-host-id which will
      ; compress the 32 bytes into 20 bytes.
      (compress-host-id (convert (string->bytes/utf-8 (string-upcase host-id))))))
  (provide/contract
   [host-id-string->host-id-bytes (-> host-id-string? host-id-bytes?)])
  
  ; Lookup table for converting from byte to string form.
  ; We leave out the characters 'L', '1', '0', and 'O' because
  ; they look alike which means we are left with
  ; "23456789ABCDEFGHIJKMNPQRSTUVWXYZ"
  (define +byte-to-string-ht+ (make-hash))
  (hash-set! +byte-to-string-ht+ (bytes 0) #"2")
  (hash-set! +byte-to-string-ht+ (bytes 1) #"3")
  (hash-set! +byte-to-string-ht+ (bytes 2) #"4")
  (hash-set! +byte-to-string-ht+ (bytes 3) #"5")
  (hash-set! +byte-to-string-ht+ (bytes 4) #"6")
  (hash-set! +byte-to-string-ht+ (bytes 5) #"7")
  (hash-set! +byte-to-string-ht+ (bytes 6) #"8")
  (hash-set! +byte-to-string-ht+ (bytes 7) #"9")
  (hash-set! +byte-to-string-ht+ (bytes 8) #"A")
  (hash-set! +byte-to-string-ht+ (bytes 9) #"B")
  (hash-set! +byte-to-string-ht+ (bytes 10) #"C")
  (hash-set! +byte-to-string-ht+ (bytes 11) #"D")
  (hash-set! +byte-to-string-ht+ (bytes 12) #"E")
  (hash-set! +byte-to-string-ht+ (bytes 13) #"F")
  (hash-set! +byte-to-string-ht+ (bytes 14) #"G")
  (hash-set! +byte-to-string-ht+ (bytes 15) #"H")
  (hash-set! +byte-to-string-ht+ (bytes 16) #"I")
  (hash-set! +byte-to-string-ht+ (bytes 17) #"J")
  (hash-set! +byte-to-string-ht+ (bytes 18) #"K")
  (hash-set! +byte-to-string-ht+ (bytes 19) #"M")
  (hash-set! +byte-to-string-ht+ (bytes 20) #"N")
  (hash-set! +byte-to-string-ht+ (bytes 21) #"P")
  (hash-set! +byte-to-string-ht+ (bytes 22) #"Q")
  (hash-set! +byte-to-string-ht+ (bytes 23) #"R")
  (hash-set! +byte-to-string-ht+ (bytes 24) #"S")
  (hash-set! +byte-to-string-ht+ (bytes 25) #"T")
  (hash-set! +byte-to-string-ht+ (bytes 26) #"U")
  (hash-set! +byte-to-string-ht+ (bytes 27) #"V")
  (hash-set! +byte-to-string-ht+ (bytes 28) #"W")
  (hash-set! +byte-to-string-ht+ (bytes 29) #"X")
  (hash-set! +byte-to-string-ht+ (bytes 30) #"Y")
  (hash-set! +byte-to-string-ht+ (bytes 31) #"Z")
  
  ; This function converts a host-id that is in byte form into a host-id
  ; in string form.  The byte form has 20 bytes, but 32 distinct values.
  ; The 20 bytes are expanded to 32 bytes by turned every 5 bits into a byte.
  ; Once the bytes are expanded each byte value is between 0 and 32 and
  ; is mapped to a string value between 0 and 255.
  (define (host-id-bytes->host-id-string host-id)
    ; Define the recursive function convert which changes each byte to it's
    ; string representation.
    (letrec ((convert
              (lambda (data)
                (cond
                  [(= (bytes-length data) 0) #""]
                  [else
                   (bytes-append (hash-ref +byte-to-string-ht+ (subbytes data 0 1) #"") (convert (subbytes data 1)))]))))
      ; Expand the the host-id that has 20 bytes to 32 bytes and then convert
      ; each byte to a string value.
      (string-downcase (bytes->string/utf-8 (convert (expand-host-id host-id))))))
  (provide/contract
   [host-id-bytes->host-id-string (-> host-id-bytes? host-id-string?)])
  
  ; Performs bit shifting to turn a byte string that is of length 20 into a byte string
  ; that is of length 32.  This is done by turning every 5 bits into 8 bits by adding three
  ; zero bit values in front of them.
  (define (expand-host-id host-id)
    (letrec ((expand
              (lambda (data)
                (cond
                  [(= (bytes-length data) 5) 
                   ; 5 bytes is the boundary condition and can be expanded evenly.
                   ; Convert the 5 bytes into an integer, call five-bit->eight-bit to insert the extra
                   ; zero bits and then convert it back into a byte string.
                   (integer->integer-bytes (five-bit->eight-bit (integer-bytes->integer (bytes-append (make-bytes 3 0) data) #f #t) 8) 8 #f #t)]
                  [else
                   (bytes-append (expand (subbytes data 0 5)) (expand (subbytes data 5)))]))))
      (expand host-id)))
  
  ; Returns true when the given parameter is a host-id in bytes and of the appropriate size.
  (define (expanded-host-id-bytes? host-id)
    (and (bytes? host-id) (= (bytes-length host-id) (host-id-string-length))))
  (provide/contract
   [expand-host-id (-> host-id-bytes? expanded-host-id-bytes?)])
  
  ; Takes a value v and adds 3 zero bits in front of every 5 bits starting
  ; with the least significant bit.  This should turn every 5 bits into it's
  ; own 8 bit value.
  ; Example:
  ;  Bits: '...0110 1001' -> '...011 00001001'
  (define (five-bit->eight-bit v num)
    (cond
      [(= num 1) v]
      [else
       ; Isolate the lower 5 bits.
       (bitwise-ior (- v (arithmetic-shift (arithmetic-shift v -5) 5))
                    ; Shift left by 5 bits and call ourselves recursively. When coming back out shift by 8.
                    (arithmetic-shift (five-bit->eight-bit (arithmetic-shift v -5) (- num 1)) 8))]))
  
  ; Performs bit shifting to turn a byte string that is of length 32 into a byte string
  ; that is of length 20.  This is done by turning every 8 bits into 5 bits by removing
  ; three zero bit values in front of them.
  (define (compress-host-id host-id)
    (letrec ((compress
              (lambda (data)
                (cond
                  [(= (bytes-length data) 8)
                   ; 8 bytes is the boundary condition and can be compressed evenly.
                   ; Converts the 8 bytes into an integer, call eight-bit->five-bit to remove
                   ; the extra zero bits and then converts it back into a byte string.
                   (subbytes (integer->integer-bytes (eight-bit->five-bit (integer-bytes->integer data #f #t) 8) 8 #f #t) 3 8)]
                  [else
                   (bytes-append (compress (subbytes data 0 8)) (compress (subbytes data 8)))]))))
      (compress host-id)))
  (provide/contract
   [compress-host-id (-> expanded-host-id-bytes? host-id-bytes?)])
  
  ; Takes a value v and removes 3 bits in front of every 5 bits starting with
  ; the least significant bit.  This should turn every 8 bits into it's own
  ; 5 bit value.
  ; Example:
  ;  Bits: '...011 00001001' -> '... 0110 1001'
  (define (eight-bit->five-bit v num)
    (cond
      [(= num 1) v]
      [else
       ; Isolate the lower 8 bits
       (bitwise-ior (- v (arithmetic-shift (arithmetic-shift v -8) 8))
                    ; Shift left by 8 bits and call ourselves recursively.  When coming back out shift by 5
                    ; to remove the excess bits.
                    (arithmetic-shift (eight-bit->five-bit (arithmetic-shift v -8) (- num 1)) 5))]))

