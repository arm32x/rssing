#lang racket

(require srfi/19)
(require net/url)

(require "./utils/dates.rkt")
(require "./utils/keyword-structs.rkt")

(provide (struct-out feed) feed/kw)
(provide feed->xexpr)

; The location where generated feeds are hosted, for <link rel="self">
(define base-url (string->url "https://arm32x.github.io/rssing/"))
; The GitHub repository URL, for <generator>
(define repo-url-string "https://github.com/arm32x/rssing")

(struct/kw feed (; File to write the generated feed to
                 filename
                 ; A URI that uniquely identifies the generated feed
                 id
                 ; Title of the generated feed
                 title
                 ; Extra X-expressions to insert into the feed metadata
                 [extra-metadata '()]
                 ; Function that returns a list of articles
                 articles)
                #:transparent)

(define (feed->xexpr feed)
  `(feed ([xmlns "http://www.w3.org/2005/Atom"])
     (id ,(feed-id feed))
     (title ,(feed-title feed))
     (updated ,(date->string/rfc3339 (current-date)))
     (generator ([uri ,repo-url-string]) "RSSing")
     (link ([rel "self"]
            [type "application/atom+xml"]
            [href ,(url->string (combine-url/relative base-url (feed-filename feed)))]))
     ,@(feed-extra-metadata feed)))

