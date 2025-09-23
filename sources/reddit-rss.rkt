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

; TODO - Use a smarter implementation, possibly the one from Redlib
(define (rewrite-urls url-string)
  (string-replace url-string (reddit-host) (redlib-host) #:all? #t))

(define (reddit-rss-feed-xexpr feed-path)
  (let-values ([[status-line headers input-port]
                (http-sendrecv (reddit-host) feed-path #:ssl? 'secure)])
    (ensure-success-status-code status-line)
    (~> input-port read-xml document-element xml->xexpr)))

(define (reddit-rss-articles ; URL path to RSS (technically Atom) feed, with leading slash
                             feed-path
                             ; If true, only posts/submissions (fullname starts with t3_) will be included
                             #:posts-only? [posts-only? #f]
                             #:subreddit   [subreddit   'any])
  (let ([feed-xexpr (reddit-rss-feed-xexpr feed-path)])
    (for/list ([element-xexpr (list-tail feed-xexpr 2)]
               #:when (eqv? (first element-xexpr) 'entry)
               #:do   [(define id           (se-path* '(entry id) element-xexpr))
                       (define title        (se-path* '(entry title) element-xexpr))
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
