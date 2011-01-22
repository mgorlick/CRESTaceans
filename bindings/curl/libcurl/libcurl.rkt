#lang racket

(require ffi/unsafe)

(define libcurl (ffi-lib "libcurl" "4"))

(define-syntax-rule (defcurl obj typ)
  (define obj (get-ffi-obj (regexp-replaces 'obj '((#rx"-" "_"))) libcurl typ)))
(define-syntax-rule (defcurl* typ obj ...)
  (begin (defcurl obj typ)
         ...))

(define-cpointer-type _CURL-pointer)
(define CURLInfo-String #x100000)
(define CURLInfo-Long #x200000)
(define CURLInfo-Double #x300000)
(define CURLInfo-SList #x400000)
(define CURLInfo-Mask #x0fffff)
(define CURLInfo-Typemask #xf00000)

(define-syntax-rule (def-curl-info id base arg)
  (define id (+ base arg)))
(define-syntax-rule (def-curl-info* base ((id arg) ...))
  (begin (def-curl-info id base arg)
         ...))

(def-curl-info*
  CURLInfo-String
  ([Effective-URL 1]
   [Content-Type 18]
   [FTP-Entry-Path 30]
   [Redirect-URL 31]
   [Primary-IP 32]))

(def-curl-info*
  CURLInfo-Long
  ([Response-Code 2]
   [Header-Size 11]
   [Request-Size 12]
   [SSL-VerifyResult 13]
   [Filename 14]
   [Redirect-Count 20]
   [HTTP-ConnectCode 22]
   [HTTPAuth-Avail 23]
   [ProxyAuth-Avail 24]
   [OS-Errno 25]
   [Num-Connects 26]
   [LastSocket 29]
   [Condition-Unmet 35]))

(def-curl-info*
  CURLInfo-Double
  ([Total-Time 3]
   [NameLookup-Time 4]
   [Connect-Time 5]
   [Pretransfer-Time 6]
   [Size-Upload 7]
   [Size-Download 8]
   [Speed-Download 9]
   [Speed-Upload 10]
   [Content-Length-Download 15]
   [Content-Length-Upload 16]
   [Redirect-Time 19]
   [Appconnect-Time 33]))

(def-curl-info*
  CURLInfo-SList
  ([SSL-Engines 27]
   [CookeList 28]
   [CertInfo 34]))

(define _CURLCode
  (_enum
   '(ok = 0
        unsupported-protocol
        failed-init
        url-malformat
        obsolete4
        couldnt-resolve-proxy
        couldnt-resolve-host
        couldnt-connect
        ftp-weird-server-reply
        remote-access-denied
        obsolete10
        ftp-weird-pass-reply
        obsolete12
        ftp-werd-pasv-reply
        ftp-weird-227-format
        cant-get-host
        obsolete16
        ftp-couldnt-set-type
        partial-file
        couldnt-retr-file
        obsolete20
        quote-error
        http-returned-error
        write-error
        obsolete24
        upload-failed
        read-error
        out-of-memory
        operation-timeout
        obsolete29
        ftp-port-failed
        ftp-couldnt-use-rest
        obsolete32
        range-error
        http-post-error
        ssl-connect-error
        bad-download-resume
        file-couldnt-read-file
        ldap-cannot-bind
        ldap-search-failed
        obsolete40
        function-not-found
        aborted-by-callback
        bad-function-argument
        obsolete44
        interface-failed
        obsolete46
        too-many-redirects
        unknown-telnet-option
        telnet-option-syntax
        obsolete50
        peer-failed-verification
        got-nothing
        ssl-engine-notfound
        ssl-engine-setfailed
        send-error
        recv-error
        obsolete57
        ssl-certproblem
        ssl-cipher
        ssl-cacert
        bad-content-encoding
        ldap-invalid-url
        use-ssl-failed
        send-fail-rewind
        ssl-engine-initfailed
        login-denied
        tftp-notfound
        tftp-perm
        remote-disk-full
        tftp-illegal
        tftp-unknownid
        remote-file-exists
        tftp-nosuchuser
        conv-failed
        conv-reqd
        ssl-cacert-badfile
        remote-file-not-found
        ssh
        ssl-shutdown-failed
        again
        ssl-crl-badfile
        ssl-issuer-error
        last)))

(define-cstruct _CURL-SList
  ([data _string]
   [next _CURL-SList]))

(define-cpointer-type _CURL-HTTPPost-pointer)

(define _time_t _int)
(define _size_t _int)
(define _CURL-formget-callback (_fun _void _string _size_t -> _size_t))

;; general api
;(defcurl curl-formadd (_fun _CURL-HTTPPost-pointer 
(defcurl curl-formfree (_fun _CURL-HTTPPost-pointer -> _void))
(defcurl curl-formget (_fun _CURL-HTTPPost-pointer _pointer _CURL-formget-callback -> _void))
(defcurl curl-free (_fun _string -> _void))
(defcurl curl-getdate (_fun _string ((_ptr i _time_t) = #f) -> _time_t))
(defcurl curl-global-cleanup (_fun -> _void))
(defcurl curl-global-init (_fun _long -> _CURLCode))
(defcurl curl-slist-append (_fun _CURL-SList-pointer _string -> _CURL-SList-pointer))
(defcurl curl-slist-free-all (_fun _CURL-SList-pointer -> _void))
(defcurl curl-unescape (_fun _string _int -> _string))
(defcurl curl-version (_fun -> _string))

;; "easy" interface
(defcurl curl-easy-init (_fun -> _CURL-pointer))
(defcurl curl-easy-escape (_fun _CURL-pointer _string _int -> _string))
(defcurl* (_fun _CURL-pointer -> _void)
  curl-easy-cleanup
  curl-easy-reset)
(defcurl curl-easy-duphandle (_fun _CURL-pointer -> _CURL-pointer))
(defcurl curl-easy-getinfo (_fun _CURL-pointer _int _pointer -> _CURLCode))
(defcurl curl-easy-pause (_fun _CURL-pointer _int -> _CURLCode))
(defcurl curl-easy-perform (_fun _CURL-pointer -> _CURLCode))
(defcurl curl-easy-strerror (_fun _CURLCode -> _string))
(defcurl curl-easy-unescape (_fun _CURL-pointer _string _int (o : (_ptr o _int))
                                  -> (s : _string)
                                  -> (values s o)))