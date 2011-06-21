(require-library 'sisc/libs/srfi/srfi-13) ; String library.
(require-library 'sisc/libs/srfi/srfi-19) ; Time/date library.
(require-library 'sisc/libs/srfi/srfi-27) ; Random numbers.

(module
 serf/utility/base
 ; Exports.
 (<java-string>
  <java-string-builder>
  :java-null-string:
  java-to-string
  java-to-length)
  ;set-generic-java-method ; Helper macro. (set-generic-java-method get-something)
  ;set-java-class)         ; Helper macro. (set-java-class <java-widget> |java.foo.Widget|)
 ; Implementation.
 (import s2j)
; (import* s2j-reflection java/mangle-method-name) ; s2j-reflection is a native module.
 (include "utility/base.scm"))

(display "serf/object\n")

(module
 serf/object
 (object?
  make-method!
  make-predicate!
  unmake-method!
  make-generic-method
  make-generic-predicate
  make-object
  get-method
  get-all-methods
  instantiate
  object/forge)
 (include "object.scm"))

(display "serf/time\n")

(module
 serf/time
 ; Exports.
 (now/utc
  now/monotonic
  utc-to-iso8601 ; Deprecated.
  utc/iso8601
  utc-to-string  ; Deprecated.
  utc/string
  milliseconds-to-duration
  duration-to-milliseconds)
 ; Implementation.
 (import srfi-19) ; Time/date.
 (include "utility/time.scm"))

(display "serf/uri\n")

(module
 serf/uri
 ; Exports.
 (<java.net.uri>
  ; Constants
  :uri-default-port:
  ; Constructor
  uri/new
  ; Accessors, tests, and conversions.
  uri/uri?
  uri/scheme
  uri/authority
  uri/host
  uri/port
  uri/path
  uri/query
  uri/absolute?
  uri/ascii
  uri/path+query
  uri/explode)
 ; Implementation.
 (import s2j)
 (import pattern-matching)
 (import type-system)
 (import* serf/utility/base :java-null-string:)
 (include "network/uri.scm"))

(display "serf/uuid\n")

(module
 serf/uuid
 ; Exports
 (uuid/string
  uuid/symbol)
 ; Implementation
 (import s2j)
 (include "utility/uuid.scm"))

(display "serf/log\n")

(module
 serf/log
 ; Exports
 (log/new
  log/level
  log/level!
  log/get
  log/trace
  log/debug
  log/info
  log/warn
  log/error
  log/fatal
  log/audit
  snip-package-name
  java-object-to-id
  string-at-most)
 ; Implementation
 (import s2j)
 (import srfi-13)
 (import serf/utility/base)
 (include "utility/log.scm"))

(display "serf/queue\n")

(module
 serf/queue
 ; Exports.
 (queue/new
  queue/empty?
  queue/nonempty?
  queue/full?
  queue/capacity
  queue/occupancy
  queue/put!
  queue/take!
  queue/rewind!
  queue/advance!
  queue/peek
  queue/extract!
  queue/to-list)
 ; Implementation.
 (import generic-procedures)
 (import oo)
 (import record)
 (import threading)
 (import type-system)
 (import serf/time)
 (include "queue.scm"))

(display "serf/mailbox\n")

(module
 serf/mailbox
 ; Exports
 (<mailbox>
  mailbox/new
  mailbox/local?
  mailbox/remote?
  mailbox/empty?
  mailbox/nonempty?
  mailbox/capacity
  mailbox/backlog
  mailbox/uri
  mailbox/send!
  mailbox/receive!
  mailbox/filter!)
 ; Implementation.
 (import generic-procedures)
 (import oo)
 (import type-system)
 (import serf/queue)
 (import* serf/uri <java.net.uri>)
 (include "fiber/mailbox.scm"))

(display "serf/message\n")

(module
 serf/message
 ; Exports
 (message/path+body
  message/path+body+echo
  message/path+body+metadata
  message/path+body+reply
  message/all
  serf/message?
  :message/path
  :message/body
  :message/metadata
  :message/reply
  :message/echo
  :no-metadata:
  :no-reply:
  message/test/01 ; Debugging.
  @ ! ?)
 ; Implementation.
 (import oo)
 (import pattern-matching)
 (import record)
 (import serf/mailbox)
 (include "fiber/message.scm"))

(display "serf/fiber\n")

(module
 serf/fiber
 ; Exports.
 (fiber/new
  fiber/start
  fiber/moniker
  fiber/moniker!
  fiber/snooze
  this-dispatch
  this-fiber
  this-log
  this-mailbox
  this-peer
  this-uri)
 ; Implementation
 (import generic-procedures)
 (import oo)
 (import threading)
 (import type-system)
 (import serf/mailbox)
 (import* serf/log log/audit)
 (include "fiber/fiber.scm"))

(display "serf/network/address\n")

(module
 serf/network/address
 ; Exports
 (address/canonical)
 ; Implementation
 (import s2j)
 (include "network/address.scm"))

(display "serf/imposter/proxy\n")

#;(module
 serf/imposter/proxy
 (proxy-http-request-responder
  proxy-connection-listener
  proxy-http-session-responder
  ; Methods for <java-http-request-responder>
  initialize-context
  finalize-context
  submit-request
  handle-response
  ; Methods for <java-connection-listener>.
  connection/open
  connection/timeout
  connection/closed
  connection/fatal-io-exception
  connection/fatal-protocol-exception
  ; Methods for <java-http-session-responder>.
  session/cancelled
  session/completed
  session/failed
  session/timeout)

 (import s2j)
 (import serf/object)
 (import serf/log)
 (include "network/imposter/proxy.scm"))

(display "serf/imposter/bottom\n")

#;(module
 serf/imposter/bottom
 (bottom/new
  uri-to-request-path)
 (import s2j)
 (import serf/fiber)
 (include "network/imposter/bottom.scm"))

(display "serf/serialize\n")

(module
 serf/serialize
 (serialize/from          ; Serialize an arbitrary Scheme object.
  deserialize/from        ; Deserialize from a Scheme buffer.
  ; Conversions
  buffer/java-byte-array  ; Translate a Scheme buffer to a Java byte[]
  java-byte-array/buffer  ; Translate a Java byte[] to a Scheme buffer.
  buffer/base64           ; Convert a Scheme buffer to a base64 Java String.
  base64/buffer)          ; Convert a base64 Java String to a Scheme buffer.
 ; Implementation
 (import buffers)
 (import buffer-io)
 (import serial-io)
 (import s2j)
 (include "serialize.scm"))

(display "serf/http/entity\n")

(module
 serf/http/entity
 ; Exports
 (http/entity/new
  http/entity?
  http/entity/length
  http/entity/string
  http/entity/string/force
  http/entity/java-string
  http/entity/java-byte-array
  http/entity/buffer)
 ; Implementation
 (import buffers)
 (import s2j)
 (import type-system)
 (import serf/serialize)
 (include "network/http/entity.scm"))

(display "serf/sham/matcher\n")

(module
 serf/sham/matcher
 (<sham/matcher>
  matcher/register
  matcher/unregister
  matcher/match)
 (import s2j)
 (import serf/object)
 (include "network/sham/matcher.scm"))

(display "serf/http/header\n")

(module
 serf/http/header
 (http/header/new
  http/header/content/type/parse
  http/header/unwrap
  :http/content/csv:
  :http/content/form/urlencoded:
  :http/content/html:
  :http/content/json:
  :http/content/serf/binary:
  :http/content/serf/base64:
  :http/content/text:
  :http/content/xml:
  :mime/csv:
  :mime/form/urlencoded:
  :mime/html:
  :mime/json:
  :mime/serf/binary:
  :mime/serf/base64:
  :mime/text:
  :mime/xml:)
 (import s2j)
 (include "network/http/header.scm"))

(display "serf/http/request\n")

(module
 serf/http/request
 (<http/request>
  http/request
  http/request/encoding
  http/request/encoding!
  http/request/entity
  http/request/headers
  http/request/headers!
  http/request/header!
  http/request/header?
  http/request/header
  http/request/length ; Deprecated.
  http/request/content/length ; Encouraged.
  http/request/method
  http/request/remove!
  http/request/type ; Deprecated.
  http/request/content/type ; Encourged.
  http/request/type! 
  http/request/uri
  http/request/uri*
  :null-java-http-request:
  request/test/01
  request/test/02)
 (import pattern-matching)
 (import type-system)
 (import s2j)
 (import serf/utility/base)
 (import serf/object)
 (import serf/http/entity)
 (import serf/http/header)
 (include "network/http/request.scm"))

(display "serf/http/response\n")

(module
 serf/http/response
 (<http/response>
  http/response
  http/response/encoding
  http/response/encoding!
  http/response/entity
  http/response/entity!
  http/response/headers
  http/response/headers!
  http/response/header!
  http/response/header?
  http/response/header
  http/response/length ; Deprecated.
  http/response/content/length
  http/response/remove!
  http/response/reason
  http/response/reason!
  http/response/status
  http/response/status!
  http/response/type ; Deprecated.
  http/response/content/type
  http/response/type!
  ;response/test/01
  ;response/test/02)
)
 (import pattern-matching)
 (import type-system)
 (import s2j)
 (import serf/utility/base)
 (import serf/object)
 (import serf/http/entity)
 (import serf/http/header)
 (include "network/http/response.scm"))

(display "serf/imposter\n")

(module
 serf/imposter
 (<serf/imposter>
 ; imposter/dispatch)
)
 (import hashtable)
 (import pattern-matching)
 (import record)
 (import s2j)
 (import srfi-13)
 (import serf/fiber)
 (import serf/http/entity)
 (import serf/http/header)
 (import serf/http/request)
 (import serf/http/response)
 ;(import serf/imposter/bottom)
 ;(import serf/imposter/proxy)
 (import serf/log)
 (import serf/mailbox)
 (import serf/message)
 (import serf/network/address)
 (import serf/object)
 (import serf/queue)
 (import serf/time)
 ; (import serf/network/http)
 (import serf/uri)
 (include "network/imposter/imposter-ng.scm"))

;(display "serf/sham/proxy\n")

; (module
;  serf/sham/proxy
;  (proxy-http-request-responder
;   proxy-connection-listener
;   ; Exports (Sham methods).
;   sham/respond
;   connection/open
;   connection/timeout
;   connection/closed
;   connection/fatal-io-exception
;   connection/fatal-protocol-exception)
;  ; Implementation. 
;  (import s2j)
;  (import serf/object)
;  (import* serf/http/request <http/request>)
;  (import* serf/http/response <http/response>)
;  (include "network/sham/proxy.scm"))

;(display "serf/sham/bottom\n")

; (module
;  serf/sham/bottom
;  (bottom/new)
;  (import s2j)
;  (import serf/fiber)
;  (import serf/utility/base)
;  (include "network/sham/bottom.scm"))

(display "serf/sham\n")

(module
 serf/sham
 ; Exports
 (<serf/sham>
  sham/register
  sham/unregister
  sham/test/01
  sham/test/02)

 ; Implementation
 (import hashtable)
 (import oo) ; For (make <mailbox>)
 (import pattern-matching)
 (import s2j)
 (import srfi-19)
 ;(import serf/utility/base)
 (import serf/fiber)
 (import serf/log)
 (import serf/mailbox)
 (import serf/message)
 (import serf/object)
 (import serf/http/request)
 (import serf/http/response)
 (import serf/http/entity)
 (import serf/time)
 (import serf/uri)
 ;(import serf/sham/bottom)
 (import serf/sham/matcher)
 ;(import serf/sham/proxy)
 (include "network/sham/sham-ng.scm"))

(display "serf/json\n")

(module
 serf/json
 (json/translate  ; Translate a Scheme or Java string JSON representation into the Scheme equivalent.
  json/parser/new ; Create a virgin JSON simple parser.
  json/parse      ; (json/parse parser s) Parse Scheme or Java string s with the given JSON simple parser.
  json/string     ; Given a equivalent Scheme object return the JSON string representation.
  json/translate/any  ; Translate the output of json/parse into the equivalent Scheme object.
  json/list/string   ; Convert an association list to the text form of JSON collection.
  json/vector/string) ; Convert a Scheme vector to the text form of a JSON array.
 ; Implementation.
 (import srfi-13) ; String library.
 (import hashtable)
 (import s2j)
 (include "json.scm"))

(display "serf/abdera\n")

(module
 serf/abdera
 (abdera/parse        ; Parse a feed string into List<Entry>
  abdera/feed/list    ; Return List<Entry> from Abdera object as (e_0 e_1 ...)
  abdera/entry/author ; Return Entry author as Person object
  abdera/entry/title  ; Return Entry title as Scheme string.
  abdera/entry/content ; Return Entry content as Scheme string
  abdera/entry/links   ; Return List<Link> of Entry as (link_0 link_1 ...)
  abdera/entry/published ; Return date published as Scheme time-utc
  abdera/entry/updated   ; Return date updated as Scheme time-utc
  abdera/link/href       ; Return Link href as Scheme string
  abdera/link/title      ; Return Link title as Scheme string
  abdera/person/name     ; Return Person name as Scheme string
  abdera/person/email    ; Return Person email address as Scheme string
  :gpo-atom-feed:) ; For testing
 (import s2j)
 (import srfi-19) ; Date/Time
 (include "abdera.scm"))

(display "serf/htmlparser\n")

(module
 serf/htmlparser
 (htmlparser/parse    ; Parse a string
  htmlparser/get-text ; Return List<Entry> from Abdera object as (e_0 e_1 ...)
 )
 (import s2j)
 (include "htmlparser.scm"))

(display "serf/wordcount\n")

(module
 serf/wordcount
 (wordcount/list
 )
 (import srfi-13)
 (import hashtable)
 (include "wordcount.scm"))


(display "serf/widgets-test\n")

(module
 serf/widgets-test
 (cloud/json)
 (import srfi-13) ; String library.
 (import srfi-27) ; Random numbers.
 (include "widgets/tagcloud-test.scm"))

(display "serf/widgets\n")

(module
 serf/widgets
 (
  thunk/clock
  thunk/rss-feed
  thunk/tagcloud
  thunk/qrcode
  thunk/urlsel
  thunk/calendar
  thunk/google-news
  thunk/sparkline
 )
 (import oo)
 (import srfi-1)
 (import srfi-13)
 (import pattern-matching)
 (import hashtable)
 (import serf/mailbox)
 (import serf/message)
 (import serf/fiber)
 (import serf/http/request)
 (import serf/http/response)
 (import serf/http/entity)
 (import serf/time)
 (import serf/uri)
 (import serf/json)
 (import serf/abdera)
 (import serf/htmlparser)
 (import serf/wordcount)
 (include "widgets/clock.scm")
 (include "widgets/tagcloud.scm")
 (include "widgets/rss-feed.scm")
 (include "widgets/qrcode.scm")
 (include "widgets/urlsel.scm")
 (include "widgets/sparkline.scm")
 (include "widgets/calendar.scm")
 (include "widgets/google-news.scm")
)

(display "serf/widgets/manager\n")

(module
 serf/widgets/manager
 (thunk/manager)
 (import srfi-13)
 (import pattern-matching)
 (import hashtable)
 (import serf/widgets)
 (import serf/message)
 (import serf/fiber)
 (import serf/http/request)
 (import serf/http/response)
 (import serf/http/entity)
 (import serf/uri)
 (import serf/json)
 (include "widgets/manager.scm"))

(display "serf/peer\n")

(module
 serf/peer
 (<serf/peer>
  peer/authority
  peer/birth
  peer/dispatch
  peer/host
  peer/log
  peer/mailbox
  peer/port
  peer/sham
  peer/spawn
  peer/spawn/unsafe
  mailbox/uuid ; May be added back later.
  mailbox/path
  peer/test/01
  peer/test/02
  peer/test/03
  ;request/echo ; debugging
  ;:request: ; debugging
  ;:request-entity: ; debugging
  who-am-i)
 (import hashtable)
 (import networking)
 (import oo) ; Remove this when I redo <mailbox> in serf/object.
 (import pattern-matching)
 (import serf/fiber)
 (import serf/http/entity) ; For the sake of (peer/test/XX ...)
 (import serf/http/header)
 (import serf/http/request)  ; For the sake of (peer/test/XX ...)
 (import serf/http/response) ; For the sake of (peer/test/XX ...)
 (import serf/imposter)
 (import serf/log)
 (import serf/mailbox)
 (import serf/message)
 ;(import serf/network/http) ; For the sake of (peer/test/XX ...)

 (import serf/object)
 (import serf/sham)
 (import serf/time)
 (import serf/uri)
 (import serf/uuid)
 (import serf/widgets-test) ; For the sake of (peer/test/XX ...)
 (import srfi-13)      ; For the sake of (peer/test/XX ...)

 (import serf/widgets)
 (import serf/json)
 (include "widgets/manager.scm")
 (include "peer.scm"))

