#lang racket

(require net/http-client)
(require xml)

(require "../utils/http.rkt")
(require "../utils/threading.rkt")

(provide reddit-rss-articles)

; The host to request Reddit RSS feeds from
(define reddit-host (make-parameter "www.reddit.com"))
; The host of the redlib instance to use in generated feeds
(define redlib-host (make-parameter "redlib.catsarch.com"))

(define (reddit-rss-feed-xexpr feed-path)
  (let-values ([[status-line headers input-port]
                (http-sendrecv (reddit-host) feed-path #:ssl? 'secure)])
    (ensure-success-status-code status-line)
    (~> input-port read-xml document-element xml->xexpr)))

(define (reddit-rss-articles feed-path                          ; URL path to RSS (technically Atom) feed, with leading slash
                             #:posts-only [posts-only #f]       ; If true, only posts/submissions (fullname starts with t3_) will be included
                             #:rewrite-urls [rewrite-urls #t])  ; If true, rewrite all links to point to Redlib
  '() #| TODO |#)
