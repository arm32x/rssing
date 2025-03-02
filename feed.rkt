#lang racket

(require srfi/19)
(require net/url)
(require (prefix-in xml: xml))

(require "./article.rkt")
(require "./utils/dates.rkt")
(require "./utils/keyword-structs.rkt")

(provide (struct-out feed) feed/kw)
(provide feed-resolve-articles)
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
                 ; List of articles (resolved) or a procedure that returns a list of articles
                 ; (not resolved). Some functions require the articles to be resolved first,
                 ; which can be done with feed-resolve-articles.
                 articles)
                #:transparent)

(define (feed-resolve-articles feed-instance)
  (if (procedure? (feed-articles feed-instance))
    (struct-copy feed feed-instance
                 [articles ((feed-articles feed-instance))])
    feed-instance))

(define (feed->xexpr feed)
  `(feed ([xmlns "http://www.w3.org/2005/Atom"])
     (id ,(feed-id feed))
     (title ,(feed-title feed))
     (updated ,(date->string/rfc3339 (current-date)))
     (generator ([uri ,repo-url-string]) "RSSing")
     (link ([rel "self"]
            [type "application/atom+xml"]
            [href ,(url->string (combine-url/relative base-url (feed-filename feed)))]))
     ,@(feed-extra-metadata feed)
     ,@(match (feed-articles feed)
         [(list articles ...) (map article->xexpr articles)]
         [(? procedure?)      (list (xml:comment "not resolved"))])))

