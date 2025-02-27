#lang racket

(provide reddit-rss-articles)

; The host to request Reddit RSS feeds from
(define reddit-host (make-parameter "www.reddit.com"))
; The host of the redlib instance to use in generated feeds
(define redlib-host (make-parameter "redlib.catsarch.com"))

(define (reddit-rss-articles feed-path                          ; URL path to RSS (technically Atom) feed, with leading slash
                             #:posts-only [posts-only #f]       ; If true, only posts/submissions (fullname starts with t3_) will be included
                             #:rewrite-urls [rewrite-urls #t])  ; If true, rewrite all links to point to Redlib
  '())  ; TODO
