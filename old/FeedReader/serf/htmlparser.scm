;; Copyright 2009 Justin R. Erenkrantz, Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; defines the AbderaParser class that constructs the list of entries of an atom feed
(define-java-class <java-html-parser> |HTMLParser|)
(define-generic-java-method get-text)

;; Constructs the parsed Atom feed given by string s.
(define (htmlparser/parse s)
  (java-new <java-html-parser> (->jstring s)))

;; Returns the individual Entry objects in the feed as a list in the order
;; in which they appear in the feed.
(define (htmlparser/get-text parser)
  (->string (get-text parser)))
