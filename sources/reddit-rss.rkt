#lang racket

(require net/http-client)
(require xml)

(require "../article.rkt")
(require "../utils/dates.rkt")
(require "../utils/http.rkt")
(require "../utils/threading.rkt")

(provide reddit-rss-articles)

; The host to request Reddit RSS feeds from
(define reddit-host (make-parameter "www.reddit.com"))
; The host of the redlib instance to use in generated feeds
(define redlib-host (make-parameter "redlib.catsarch.com"))

; TODO - Use a smarter implementation, possibly the one from Redlib
(define (rewrite-urls url-string)
  (string-replace url-string (reddit-host) (redlib-host) #:all? #t))

(define (reddit-rss-feed-xexpr feed-path)
  (let-values ([[status-line headers input-port]
                (http-sendrecv (reddit-host) feed-path #:ssl? 'secure)])
    (ensure-success-status-code status-line)
    (~> input-port read-xml document-element xml->xexpr)))

(define (reddit-rss-articles feed-path                            ; URL path to RSS (technically Atom) feed, with leading slash
                             #:posts-only? [posts-only? #f]       ; If true, only posts/submissions (fullname starts with t3_) will be included
                             #:rewrite-urls? [rewrite-urls? #t])  ; If true, rewrite all links to point to Redlib
  (let ([feed-xexpr (reddit-rss-feed-xexpr feed-path)])
    (for/list ([element-xexpr (list-tail feed-xexpr 2)]
               #:when        (eqv? (first element-xexpr) 'entry)
               #:do          [(match-define (list-no-order
                                              `(id () ,id)
                                              `(title () ,title)
                                              `(author ()
                                                 (name () ,author-name)
                                                 (uri () ,author-url))
                                              `(updated () ,(app string->date/rfc3339 date-updated))
                                              _ ...)
                                            (list-tail element-xexpr 2))]
               #:when        (or (not posts-only?) (string-prefix? id "t3_")))
      (article/kw #:id             (format "tag:rssing.arm32.ax,2025-03-02:reddit-rss/~a" id)
                  #:title          title
                  #:date-updated   date-updated
                  #:extra-metadata `((author ()
                                       (name () ,author-name)
                                       (uri () ,(rewrite-urls author-url))))))))
