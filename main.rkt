#lang racket

(require srfi/26)  ; cut macro
(require xml)

(require "./article.rkt")
(require "./feed.rkt")
(require "./sources/reddit-rss.rkt")

(define feeds
  (list
    (let ([title "Engineering, Magic, and Kitsune"])
      (feed/kw #:filename "engineering-magic-and-kitsune.atom"
               #:id       "tag:rssing.arm32.ax,2025-02-24:feed/engineering-magic-and-kitsune"
               #:title    title
               #:articles (λ () (filter (cut article-title-contains? <> title)
                                        (reddit-rss-articles "/user/SteelTrim.rss" #:posts-only? #t)))))))

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
