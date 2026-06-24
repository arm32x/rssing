#lang racket

(require net/http-client)
(require xml)
(require xml/path)

(require "../article.rkt")
(require "../utils/dates.rkt")
(require "../utils/http.rkt")
(require "../utils/threading.rkt")

(provide reddit-rss-articles)

; The host to request Reddit RSS feeds from
(define reddit-host (make-parameter "www.reddit.com"))
; The host of the redlib instance to use in generated feeds
; redlib.arm32.ax will redirect to whatever instance I currently prefer
(define redlib-host (make-parameter "redlib.arm32.ax"))

; Maximum number of times to retry requests, not including the initial attempt
(define max-retry-attempts 3)

; TODO - Use a smarter implementation, possibly the one from Redlib
(define (rewrite-urls url-string)
  (string-replace url-string (reddit-host) (redlib-host) #:all? #t))

(define (reddit-rss-feed-xexpr feed-path)
  (let retry ([retry-attempts-remaining max-retry-attempts])
    (let-values ([[status-line headers input-port]
                  (http-sendrecv (reddit-host) feed-path #:ssl? 'secure)])
      ; TODO - Do this without exceptions
      (with-handlers ([(λ (ex)
                         (and 
                           (exn:fail:network:http? ex)
                           (equal? (exn:fail:network:http-status-code ex) 429)))
                       (λ (ex)
                          (printf "Hit rate limit: ~a ~a~n"
                                  (exn:fail:network:http-status-code ex)
                                  (exn:fail:network:http-reason-phrase ex))
                          (printf "Rate limit used: ~a~n" (header-ref headers "x-ratelimit-used"))
                          (printf "Rate limit remaining: ~a~n" (header-ref headers "x-ratelimit-remaining"))
                          (let* ([reset-seconds-real (string->number (header-ref headers "x-ratelimit-reset"))]
                                 [reset-seconds-used (+ reset-seconds-real 1)])
                            (printf "Rate limit reset: ~a~n" reset-seconds-real)
                            (if (> retry-attempts-remaining 0)
                              (begin
                                (printf "Retrying in ~a seconds (attempt ~a of ~a)...~n"
                                        reset-seconds-used
                                        (+ 1 (- max-retry-attempts retry-attempts-remaining))
                                        max-retry-attempts)
                                (sleep reset-seconds-used)
                                (printf "Retrying...~n")
                                ; Hopefully this has tail call elimination
                                (retry (- retry-attempts-remaining 1)))
                              (begin
                                (printf "Max retry attempts reached~n")
                                (raise ex)))))])
        (ensure-success-status-code status-line))
      (~> input-port read-xml document-element xml->xexpr))))

(define (reddit-rss-articles ; URL path to RSS (technically Atom) feed, with leading slash
                             feed-path
                             ; If true, only posts/submissions (fullname starts with t3_) will be included
                             #:posts-only? [posts-only? #f]
                             #:subreddit   [subreddit   'any])
  (let ([feed-xexpr (reddit-rss-feed-xexpr feed-path)])
    (for/list ([element-xexpr (list-tail feed-xexpr 2)]
               #:when (eqv? (first element-xexpr) 'entry)
               #:do   [(define id           (se-path* '(entry id) element-xexpr))
                       (define title        (string-join (se-path*/list '(entry title) element-xexpr) ""))
                       (define author-name  (se-path* '(entry author name) element-xexpr))
                       (define author-url   (se-path* '(entry author uri) element-xexpr))
                       (define date-updated (se-path* '(entry updated) element-xexpr))
                       ; Reddit only includes one <link> element for now, so this works fine.
                       ; I'll use a more sophisticated solution if this breaks.
                       (define url          (se-path* '(entry link #:href) element-xexpr))
                       (define content-type (se-path* '(content #:type) element-xexpr))
                       (define content      (string-join (se-path*/list '(content) element-xexpr) ""))
                       (define category     (se-path* '(entry category #:term) element-xexpr))]
               #:when (or (not posts-only?) (string-prefix? id "t3_"))
               #:when (or (eqv? subreddit 'any) (string-ci=? category subreddit)))
      (article/kw #:id             (format "tag:rssing.arm32.ax,2025-03-02:reddit/~a" id)
                  #:title          title
                  #:url            (rewrite-urls url)
                  #:content        (cons
                                     (match content-type ["html" 'html] ["text" 'text])
                                     (rewrite-urls content))
                  #:date-updated   (string->date/rfc3339 date-updated)
                  #:extra-metadata `((author
                                       (name ,author-name)
                                       (uri ,(rewrite-urls author-url))))))))
