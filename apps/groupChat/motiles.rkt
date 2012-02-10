#lang racket/base

(require "../../Motile/compile/compile.rkt")
(provide (all-defined-out))

;; notes on syntactic conventions to follow:
;; flub$ - a locative named flub
;; quux@ - a curl named quux
;; frobnicatorλ - the compiled source of a lambda `frobnicator' used as a motile actor
;; foo& - a box named foo
;; clax^ - a collection of metadata (i.e. produced with `make-metadata') named clax
;; blar# - a bytestring named `blar'

; a video-specific big-bang takes a device name/width/height info along with two locations,
; spawns a proxy to connect a new decoder with a new encoder,
; then spawns the encoder and decoder.
(define (server-startup pubsub-site-public-curl@ ; where to put pubsub
                        proxyname) ; human readable, locally unique proxyname                        
  (motile/compile
   `(letrec 
        ([f (lambda ()
              (define startup-me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              ; spawn the proxy
              (curl/send ,pubsub-site-public-curl@ (spawn/new (pubsubproxy startup-me@ ,proxyname)
                                                              (make-metadata is/proxy (nick 'pubsub))
                                                              #f))
              ;(let ([endpoints (:remote/body (delivery/contents-sent (mailbox-get-message)))])
                ;(cond [(and endpoints
                ;            (hash/contains? endpoints 'subscribeAt)
                ;            (hash/contains? endpoints 'publishAt))
                      ; (curl/send ,pubsub-site-public-curl@ (spawn/new (chatclientcontroller (hash/ref endpoints 'subscribeAt startup-me@ )
                      ;                                                                       (hash/ref endpoints 'publishAt startup-me@ ))
                      ;                                        (make-metadata is/endpoint (nick 'chatclientcontroller))
                      ;                                        #f))]                                             
                ;      [else #f])
                ;)              
              )])
      (f))))

(define newgroup+client-startup 
  (motile/compile
   `(lambda (pubsub-site-public-curl@ ; where to put group pubsub
                                 proxyname ; human readable, locally unique proxyname                        
                                 client-site-public-curl@ ; where to put the first GUI client
                      )
              (define startup-me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              ; spawn the proxy
      (printf "Starter: Spawning new Group \n")
              (curl/send pubsub-site-public-curl@ (spawn/new (pubsubproxy startup-me@ proxyname)
                                                              (make-metadata is/proxy (nick 'pubsub))
                                                              #f))
              ; spawn the first GUI client (no need for using the registry)
              (let ([endpoints (:remote/body (delivery/contents-sent (mailbox-get-message)))])
                (cond [(and endpoints
                            (hash/contains? endpoints 'subscribeAt)
                            (hash/contains? endpoints 'publishAt))
                       (printf "Starter: Spawning new Client \n")
                       (curl/send client-site-public-curl@ (spawn/new (chatclientcontroller (hash/ref endpoints 'subscribeAt startup-me@ )
                                                                                             (hash/ref endpoints 'publishAt startup-me@ )
                                                                                             proxyname)
                                                              (make-metadata is/endpoint (nick 'chatclientcontroller))
                                                              #f))]                                             
                      [else #f])
                )              
              )))
      
(define newclient-startup 
  (motile/compile
   `(lambda ( proxyname ; human readable, locally unique proxyname                        
              client-site-public-curl@ ; where to put the GUI client
                      )
      ;(lambda ()
              (define reg@ (get-local-registry-curl))
              (define (reg-sub registry@)
                (curl/send/promise registry@ 
                                   (remote/new (RegistryGet/new (string-append "SUBSCRIBE-CURL::" proxyname)) (make-metadata) #f)
                                   10000))
              (define (reg-pub registry@)
                (curl/send/promise registry@ 
                                   (remote/new (RegistryGet/new (string-append "PUBLISH-CURL::" proxyname)) (make-metadata) #f)
                                   10000))
              (define (unpack-promise p); generic way to wait for a promise and look at its final-value body.
                (define res (promise/wait p 10000 #f))
                (:remote/body res))
              
              (define sub@ (unpack-promise (reg-sub reg@)))
              (define pub@ (unpack-promise (reg-pub reg@)))
      (printf "Starter: Spawning only new Client \n")
      
        (printf "New Chat controller will connect to <PUB><SUB> at: <~a:><~a>~n" pub@ sub@)
              (when (and (curl? sub@) (curl? pub@))
                 (curl/send client-site-public-curl@ (spawn/new (chatclientcontroller sub@ pub@ proxyname)
                                                              (make-metadata is/endpoint (nick 'chatclientcontroller))
                                                              #f)))
              )))

(define (local-chatcontroller controller-site-public-curl@  proxyname)
  (motile/compile
   `(letrec 
        ([f (lambda ()
               (define reg@ (get-local-registry-curl))
              (define (reg-sub registry@)
                (curl/send/promise registry@ 
                                   (remote/new (RegistryGet/new (string-append "SUBSCRIBE-CURL::" ,proxyname)) (make-metadata) #f)
                                   10000))
              (define (reg-pub registry@)
                (curl/send/promise registry@ 
                                   (remote/new (RegistryGet/new (string-append "PUBLISH-CURL::" ,proxyname)) (make-metadata) #f)
                                   10000))
              (define (unpack-promise p); generic way to wait for a promise and look at its final-value body.
                (define res (promise/wait p 10000 #f))
                (:remote/body res))
              
              (define sub@ (unpack-promise (reg-sub reg@)))
              (define pub@ (unpack-promise (reg-pub reg@)))
             ;  (printf "New Chat controller will connect to <PUB><SUB> at: <~a:><~a>~n" pub@ sub@)
              (when (and (curl? sub@) (curl? pub@))
                  (curl/send ,controller-site-public-curl@ (spawn/new (chatclientcontroller sub@ pub@ ,proxyname)
                                                              (make-metadata is/endpoint (nick 'chatclientcontroller))
                                                              #f)))
              )])
      (f))))

(define (registry)
  (motile/compile
   `(letrec 
        ([f (lambda ()
              ;; create serializable curls for both the publisher and the subscriber
              ;; to use.
              (define *me/ctrl (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t))
              (define reg-me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              
              (set-local-registry-curl! reg-me@)
              (let loop (;[provider-curls hash/equal/null]
                         [reg-content hash/equal/null] ;subscriber-curls
                         )
                (define m (mailbox-get-message))
                (define contents (delivery/contents-sent m))
                (define body (:remote/body contents))   
                (define sender@ (:remote/reply contents))
                (cond
                  ;; Requests for lookup of parameters, and return values, implement as promise
                  [(RegistryGet? body)
                   (let ([param (:RegistryGet/param body)]                         
                         ;[content/hidden-location (!:remote/reply contents sub-me@)]
                         )
                    ;  (printf "Looking for param: ~a~n" param)
                     (if (hash/contains? reg-content param)
                       (let ((value (hash/ref reg-content param #f)))
                         (curl/send (delivery/promise-fulfillment m) (remote/new value (make-metadata) reg-me@)))
                       ;; else
                       (curl/send (delivery/promise-fulfillment m) (remote/new (404-Not-Found/new "Param not found" (delivery/contents-sent m)) (make-metadata) reg-me@))
                     )                     
                   )
                   (loop reg-content)]                                    
                  ;; the control messages directed at the registry.
                  [(RegistryAdd? body)
                   (let (
                         [param (:RegistryAdd/param body)]
                         [value (:RegistryAdd/value body) ])
                     ;; upon adding, issue a CURL that lets the holder
                     ;; interact with that parameter. TODO: handling promise on other side
                     (if (hash/contains? reg-content param)                     
                       (curl/send (delivery/promise-fulfillment m) (remote/new (409-Conflict/new "Param already registered" (delivery/contents-sent m)) (make-metadata) reg-me@))
                       ;; else                       
                       (when (delivery/promise-fulfillment m) ; delivery/promise-fulfillment returns a onetime curl when the sender used curl/send/promise
                         (printf "Added Param to Registry: <~a>~n" param)
                         (curl/send (delivery/promise-fulfillment m)
                                    (remote/new (curl/new/any *me/ctrl
                                                            null
                                                            (hash/new hash/eq/null
                                                                      'for-param param
                                                                      'allowed 'remove
                                                                      ))
                                              null #f))))                    
                     (loop (hash/cons reg-content param value) ))]
                  [(RegistryDel? body)
                   (let ([meta (curl/get-meta (delivery/curl-used m))])
                     ;; look for a CURL that allows subscription interaction.
                     (cond [(and meta 
                                 (eq? 'remove (metadata-ref meta 'allowed))
                                 (hash/contains? reg-content (metadata-ref meta 'for-param)))
                            (printf "Removed Param from Registry: <~a>~n" (metadata-ref meta 'for-param))
                            (loop (hash/remove reg-content (metadata-ref meta 'for-param)) )]
                           ;; disallow subscription tinkering otherwise. TODO: polite reply that not possible e.g., 410-GONE
                           [else
                            (loop reg-content )]))]
                  [else 
                   (printf "Registry else: ~a~n" body)
                   ;; TODO polite reply that not understood: 415-Unsupported-Media-Type
                   (when (sender@)
                     (curl/send sender@ (remote/new (415-Unsupported-Media-Type/new "Message Type not supported" (delivery/contents-sent m)) (make-metadata) reg-me@)))
                   (loop reg-content )])))])
      (f))))


;; pubsubproxy is a N-to-M router 
(define (pubsubproxy on-birth-notify@ component-name)
  (motile/compile
   `(letrec 
        ([f (lambda ()
              ;; create serializable curls for both the publisher and the subscriber
              ;; to use.
              (define *me/ctrl (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t))
              (define pub-me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              (define sub-me@ (curl/new/any (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              ;; notify whoever I'm supposed to that I'm online now. 
                           
              ; get registry              
              (define reg@ (get-local-registry-curl))
              ; store endpoints at registries and store promise returns to be able to remove endpoints from registry upon shutdown
              
               (define (reg-sub registry@)
                (curl/send/promise registry@ 
                                   (remote/new (RegistryAdd/new (string-append "SUBSCRIBE-CURL::" ,component-name) sub-me@) (make-metadata) #f)
                                   10000))
              (define (reg-pub registry@)
                (curl/send/promise registry@ 
                                   (remote/new (RegistryAdd/new (string-append "PUBLISH-CURL::" ,component-name) pub-me@) (make-metadata) #f)
                                   10000))
              (define (unpack-promise p); generic way to wait for a promise and look at its final-value body.
                (define res (promise/wait p 10000 #f))
                (:remote/body res))
              
              (define my-sub/regctrl@ (unpack-promise (reg-sub reg@)))
              (define my-pub/regctrl@ (unpack-promise (reg-pub reg@)))
              
              (define (quitProxy my-sub/regctrl@ my-pub/regctrl@)
                (curl/send my-sub/regctrl@
                              (remote/new (RegistryDel/new (string-append "SUBSCRIBE-CURL::" ,component-name)) (make-metadata) #f))
                   (curl/send my-pub/regctrl@
                              (remote/new (RegistryDel/new (string-append "PUBLISH-CURL::" ,component-name)) (make-metadata) #f))
                )
              
              (curl/send ,on-birth-notify@ (remote/new  (hash/new hash/eq/null
                                                                      'publishAt pub-me@
                                                                      'subscribeAt sub-me@)
                                                        '() #f))
              (let loop ([subscriber-curls hash/equal/null]
                         )
                (define m (mailbox-get-message))
                (define contents (delivery/contents-sent m))
                (define body (:remote/body contents))
                (cond
                  ;; the types that the router knows to send forward.
                  [(ChatMsg? body)
                   (let ([content/hidden-location (!:remote/reply contents sub-me@)])
                     (hash/for-each subscriber-curls 
                                    (lambda (id.subber@)
                                      (curl/send (cdr id.subber@) content/hidden-location))))
                   (loop subscriber-curls)]
                  ;; the control messages coming from the forward direction,
                  ;; directed at the router.
                  [(AddSubscriber? body)
                   (let ([id (make-uuid)])
                     ;; upon subscription, issue a CURL that lets the holder
                     ;; interact with that subscription.
                     (printf "Chat Proxy received subscription \n") 
                     
                     (when (delivery/promise-fulfillment m)
                       (curl/send (delivery/promise-fulfillment m)
                                  (remote/new (curl/new/any *me/ctrl
                                                            null
                                                            (hash/new hash/eq/null
                                                                      'allowed 'remove
                                                                      'for-sub id))
                                              null #f)))
                     (loop (hash/cons subscriber-curls id (:AddSubscriber/curl body)) ))]
                  [(RemoveSubscriber? body)
                   (let ([meta (curl/get-meta (delivery/curl-used m))])
                     ;; look for a CURL that allows subscription interaction.                     
                     (cond [(and meta 
                                 (eq? 'remove (metadata-ref meta 'allowed))
                                 (hash/contains? subscriber-curls (metadata-ref meta 'for-sub)))
                            (let ([subscriber-curls (hash/remove subscriber-curls (metadata-ref meta 'for-sub))])
                              (printf "Chat Proxy received unsubscription, ~a subscribers left ~n" (hash/length subscriber-curls))
                              (if (hash/empty? subscriber-curls)                                    
                                  (quitProxy my-sub/regctrl@ my-pub/regctrl@)
                                  (loop subscriber-curls)
                                  ))]
                           ;; disallow subscription tinkering otherwise.
                           [else
                            (loop subscriber-curls )]))]                 
                  [(Quit? body)       
                   (printf "Chat Proxy received quit request \n") 
                   (quitProxy my-sub/regctrl@ my-pub/regctrl@)
                   ]
                  [else 
                   (printf "proxy else: ~a~n" body)
                   (loop subscriber-curls )])))])
      (f))))

(define (chatclientcontroller where-to-subscribe@ where-to-publish@ proxyname)
  (motile/compile
   `(letrec 
        ([f (lambda ()
              (define me/sub@ (curl/new/any 
                               (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))
              (define me/ctrl@ (curl/new/any 
                                (locative/cons/any (this/locative) A-LONG-TIME A-LONG-TIME #t #t) null #f))             
              (define (add-sub subscription-endpoint@)
                (curl/send/promise subscription-endpoint@ 
                                   (remote/new (AddSubscriber/new me/sub@) (make-metadata) #f)
                                   10000))
              (define (unpack-promise p); generic way to wait for a promise and look at its final-value body.
                (define res (promise/wait p 10000 #f))
                (:remote/body res))
              ; 1. reserve display.              
              (define guiLocal (newChatGUI me/sub@ (current-thread) ,proxyname))              
              ; 2. add subscription.              
              (define my-sub/ctrl@ (unpack-promise (add-sub ,where-to-subscribe@)))
              ; 3. start receiving loop
              (let loop ()
                (define m (mailbox-get-message))
                (define body (:remote/body (delivery/contents-sent m)))
                (define desc^ (:remote/metadata (delivery/contents-sent m)))
                (define reply@ (:remote/reply (delivery/contents-sent m)))
                (cond 
                  [(LocalMsg? body)
                 ; forward to proxy                                   
                   (curl/send ,where-to-publish@ (remote/new (ChatMsg/new (:LocalMsg/nickname body) (:LocalMsg/text body) (:LocalMsg/timestamp body)) (make-metadata) me/ctrl@))
                   (loop)]                      
                  [(ChatMsg? body)
                   ; forward to gui
                   (when (thread? guiLocal)
                     (thread-send guiLocal body))                   
                   (loop)]                      
                  [(CreateChat? body)                                      
                   (let ([target@ (curl/get-public (:CreateChat/host body) (:CreateChat/port body))]
                         )           
                    (printf "Spawning new Client and Group \n") 
                     (curl/send target@ (spawn/new 
                                         (lambda ()
                                           (newgroup+client-startup target@
                                                          (:CreateChat/groupname body)
                                                          PUBLIC-CURL))                                                    
                                         (make-metadata is/proxy (nick 'new-group+client-deployer)) #f))                     
                   )
                   (loop)]
                  [(JoinChat? body)                                      
                   (let ([target@ (curl/get-public (:JoinChat/host body) (:JoinChat/port body))]
                         )                     
                     (printf "Spawning new Client \n")
                     (printf "~s~n"  (:JoinChat/groupname body))
                     (curl/send target@ (spawn/new 
                                         (lambda ()
                                           (newclient-startup (:JoinChat/groupname body)
                                                           PUBLIC-CURL))                                                    
                                         (make-metadata is/proxy (nick 'new-client-deployer)) #f))                     
                   )
                   (loop)]
                  [(CP-Chat? body)                                      
                   (let ([target@ (curl/get-public (:JoinChat/host body) (:JoinChat/port body))]
                         )                    
                     (printf "Spawning new Client copy \n")
                     (curl/send target@ (spawn/new f                                                 
                                         (make-metadata is/endpoint (nick 'chatclientcontroller)) #f))                     
                   )
                   (loop)]
                  [(MV-Chat? body)                                      
                   (let ([target@ (curl/get-public (:JoinChat/host body) (:JoinChat/port body))]
                         )
                     (printf "Spawning a copy of Client and removing original \n")
                     (curl/send target@ (spawn/new f                                                 
                                         (make-metadata is/endpoint (nick 'chatclientcontroller)) #f))                    
                     (curl/send my-sub/ctrl@
                                  (remote/new (RemoveSubscriber/new) (make-metadata) #f))                                          
                   )
                   ]
                  [(Quit? body)
                    (printf (string-append ,proxyname " ChatClientController quitting\n"))
                       (curl/send my-sub/ctrl@
                                  (remote/new (RemoveSubscriber/new) (make-metadata) #f))
                  ]
                  [else
                   (displayln "ChatClientController throwing away a message")
                   (loop)])))])
      (f))))


