#lang racket

(require srfi/26)  ; cut macro
(require xml)

(require "./article.rkt")
(require "./feed.rkt")
(require "./sources/reddit-rss.rkt")
(require "./utils/threading.rkt")

(define feeds
  (list
    (let ([slug  "engineering-magic-and-kitsune"]
          [title "Engineering, Magic, and Kitsune"])
      (feed/kw #:filename (format "~a.atom" slug)
               #:id       (format "tag:rssing.arm32.ax,2025-02-24:feed/~a" slug)
               #:title    title
               #:articles (λ () (filter (cut article-title-contains? <> title)
                                        (reddit-rss-articles "/user/SteelTrim/submitted.rss" #:posts-only? #t)))))

    ; TODO - Add Patreon API support. This series releases one chapter ahead on Patreon for free.
    (let ([slug  "magic-is-programming"]
          [title "Magic is Programming"])
      (feed/kw #:filename (format "~a.atom" slug)
               #:id       (format "tag:rssing.arm32.ax,2025-02-24:feed/~a" slug)
               #:title    title
               #:articles (λ () (filter (cut article-title-contains? <> title)
                                        (reddit-rss-articles "/user/Douglasjm/submitted.rss" #:posts-only? #t)))))

    (let ([slug  "wearing-power-armor-to-a-magic-school"]
          [title "Wearing Power Armor to a Magic School"])
      (feed/kw #:filename (format "~a.atom" slug)
               #:id       (format "tag:rssing.arm32.ax,2025-02-24:feed/~a" slug)
               #:title    title
               #:articles (λ () (filter (cut article-title-contains? <> title)
                                        (reddit-rss-articles "/user/Jcb112/submitted.rss" #:posts-only? #t)))))))

(define (write-xexpr-to-file xexpr file-path)
  (let ([output-port (open-output-file
                       file-path
                       #:mode 'text
                       #:exists 'truncate/replace)])
    (display-xml/content
      (xexpr->xml xexpr)
      output-port
      #:indentation 'scan)))

(define (generate-feed feed)
  (~> feed
      feed-resolve-articles
      feed->xexpr
      (write-xexpr-to-file (feed-filename feed))))

(for-each generate-feed feeds)

; vim: sw=2 ts=2 et
