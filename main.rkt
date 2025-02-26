#lang racket

(require json)
(require net/http-client)
(require net/url)
(require srfi/19)  ; Date parsing
(require srfi/26)  ; cut macro
(require xml)

(require "./utils/keyword-structs.rkt")

; The location where generated feeds are hosted, for <link rel="self">
(define base-url (string->url "https://arm32x.github.io/rssing/"))
; The GitHub repository URL, for <generator>
(define repo-url-string "https://github.com/arm32x/rssing")

; The host to request Reddit RSS feeds from
(define reddit-host (make-parameter "www.reddit.com"))
; The host of the redlib instance to use in generated feeds
(define redlib-host (make-parameter "redlib.catsarch.com"))

(define (date->rfc3339-string date)
  ; ISO 8601 as implemented by Racket is missing the colon in the time zone
  (let* ([iso8601-string (date->string date "~4")]
         [rfc3339-string (regexp-replace #px"([-+]\\d{2})(\\d{2})$" iso8601-string "\\1:\\2")])
    rfc3339-string))

(struct/kw article (id                     ; A URI that uniquely identifies the article
                    title                  ; Title of the article
                    date-updated           ; Date of the last significant update to the article
                    [date-published null]  ; Date the article was originally published
                    [extra-metadata '()])  ; Extra xexprs to insert into the article metadata
                   #:transparent)

(define (article-title-contains? article contained)
  (string-contains? (article-title article) contained))

(define (article->xexpr article)
  `(entry
     (id ,(article-id article))
     (title ,(article-title article))
     (updated ,(date->rfc3339-string (article-date-updated article)))
     ; This is the most convenient way I can find to conditionally add an element
     ,@(match (article-date-published article)
         ['()            '()]
         [date-published `((published ,(date->rfc3339-string date-published)))])))

(struct/kw feed (filename              ; File to write the generated feed to
                 id                    ; A URI that uniquely identifies the generated feed
                 title                 ; Title of the generated feed
                 [extra-metadata '()]  ; Extra xexprs to insert into the feed metadata
                 articles)             ; Function that returns a list of articles
                #:transparent)

(define (feed->xexpr feed)
  `(feed ([xmlns "http://www.w3.org/2005/Atom"])
     (id ,(feed-id feed))
     (title ,(feed-title feed))
     (updated ,(date->rfc3339-string (current-date)))
     (generator ([uri ,repo-url-string]) "RSSing")
     (link ([rel "self"]
            [type "application/atom+xml"]
            [href ,(url->string (combine-url/relative base-url (feed-filename feed)))]))
     ,@(feed-extra-metadata feed)))

(define (reddit-rss-articles feed-path                          ; URL path to RSS (technically Atom) feed, with leading slash
                             #:posts-only [posts-only #f]       ; If true, only posts/submissions (fullname starts with t3_) will be included
                             #:rewrite-urls [rewrite-urls #t])  ; If true, rewrite all links to point to Redlib
  '())  ; TODO

(define feeds
  (list
    (let ([title "Engineering, Magic, and Kitsune"])
      (feed/kw #:filename "engineering-magic-and-kitsune.atom"
               #:id       "tag:rssing.arm32.ax,2025-02-24:feed/engineering-magic-and-kitsune"
               #:title    title
               #:articles (λ () (filter (cut article-title-contains? <> title)
                                        (reddit-rss-articles "/user/SteelTrim.rss" #:posts-only #t)))))))

(define (write-xexpr-to-file xexpr file-path)
  (let ([output-port (open-output-file
                       file-path
                       #:mode 'text
                       #:exists 'truncate/replace)])
    (display-xml/content
      (xexpr->xml xexpr)
      output-port
      #:indentation 'scan)))

; vim: sw=2 ts=2 et
