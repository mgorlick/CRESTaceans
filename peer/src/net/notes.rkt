#lang racket

#|

Rough thoughts on the layout of a CREST/BEEP message (all visible line breaks are for clarity, not part of the grammar):
[... BEEP specific headers ...]
Sender-Public-Key:<base64-URL-encoded-bytestring>\r\n
Receiver-Public-Key:<base64-URL-encoded-bystring>\r\n
HMAC:<hmac(sha1, D-H-shared-key, msg)>\r\n
Content-Length:<sizeof encrypted msg>\r\n
... other metadata that needs to go here ...
\r\n
encrypt(aes-256, D-H-shared-key) {
GET /public-key/swiss-number/path;param?query=values#fragment CREST/BEEP\r\n
... other header...\r\n
...
... gzipped body ...
}
[... BEEP specific footers ...]

|#