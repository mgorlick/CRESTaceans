



(define-java-proxy
  (proxy-connection-listener imposter)
  (<java-connection-listener>)

  (define (connection-open this c) #t)

  (define (connection-timeout this c) #t)

  (define (connection-closed this c) #t)

  (define (|fatal-IO-exception| this e c) #f)

  (define (fatal-protocol-exception this e c) #f))


