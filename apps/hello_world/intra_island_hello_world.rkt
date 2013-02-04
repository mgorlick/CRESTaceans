#lang racket/base

(require COAST)

;; ROOT ACTOR SENDs A MESSAGE TO ACTOR A TO DISPLAY HELLO WORLD AND RACKET PROGRAMS SENDS ALSO A MESSAGE TO ROOT FOR IT TO DISPLAY HELLO WORLD

;; defining the island's address
(define ISLAND-ADDRESS 
  (island/address/new #"www.hello.com" #"127.0.0.1" 5003))

;; startup the island
(define-values (ROOT ROOT-LOCATIVE PUBLIC-LOCATIVE PUBLIC-CURL COMM-thd)
  (listen-on/make-root ISLAND-ADDRESS))
(printf "Island starting: ~s~n" (this/island))


; Set the authority for the ROOT actor.
(locative/cons/authority! ROOT-LOCATIVE (list ROOT))
(locative/curl/authority! ROOT-LOCATIVE (list ROOT))


(define-syntax-rule (motile/procedure id body)
  (define id (motile/call (motile/compile (quote body)) BASELINE)))

; EXECUTE IS THE PERMANENT COMPUTATION THAT WILL BE RUNNING ON ROOT AND ACTOR A. IT IS CONTINUOUSLY WAITING FOR MESSAGES AND DESCRIBES HOW TO READ THOSE MESSAGES
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
                         (displayln (format "~a: message discarded due to unknown contents" 'nickname))
                         (loop (thread-receive))))))) ; Throw the message away.


; Start ROOT actor
(actor/jumpstart ROOT (λ ()
                        ;creates actor A_hello
                        (define-values (A_hello a/l_hello) (actor/new ROOT 'A_hello))
                        
                        ;sets the authority to constrain locatives and create curls for actor A_hello
                        (locative/cons/authority! a/l_hello (list ROOT A_hello))
                        (locative/curl/authority! a/l_hello (list ROOT A_hello))
                        
                        ; creates the CURL for actor A_hello
                        (define @A_hello (curl/new a/l_hello null #f))
                        
                        ; defines the binding environment B_hello
                        (define B_hello (list/environ (pairs/environ BASELINE (global-defines thread-receive delivery? delivery/contents-sent format)) (list 'nickname (actor/nickname A_hello))))
                        
                        ;jumpstart actor A_hello with binding environment B_hello
                        (eval-actor A_hello execute B_hello)
                        
                        ;creates the motile procedure hello
                        (motile/procedure hello (lambda() (displayln "hello world from actor A_hello")))
                        
                        ;sends actor A the expresion hello to be executed
                        (curl/send @A_hello hello)
                        
                        ; executes procedure "execute" which allows root to receive messages in a binding environment context.
                        (motile/call execute (pairs/environ BASELINE (global-defines thread-receive delivery? delivery/contents-sent format)))))
                       

; send a message to ROOT to make it send hello
  (motile/procedure hello (lambda() (displayln "hello world from ROOT actor")))
  (curl/send PUBLIC-CURL hello)
  