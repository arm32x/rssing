#lang racket

(require net/url-connect)
(require srfi/26)  ; cut macro
(require xml)

(require "./article.rkt")
(require "./feed.rkt")
(require "./sources/reddit-json.rkt")
(require "./sources/reddit-rss.rkt")
(require "./utils/threading.rkt")

(define feeds
  (list
    (let ([slug  "engineering-magic-and-kitsune"]
          [title "Engineering, Magic, and Kitsune"])
      (feed/kw #:filename (format "~a.atom" slug)
               #:id       (format "tag:rssing.arm32.ax,2025-02-24:feed/~a" slug)
               #:title    title
               #:articles (λ () (reddit-json-articles
                                  #:by                  "SteelTrim"
                                  #:when-in-subreddit   "HFY"
                                  #:when-title-contains title))))

    ; TODO - Add Patreon API support. This series releases one chapter ahead on Patreon for free.
    (let ([slug  "magic-is-programming"]
          [title "Magic is Programming"])
      (feed/kw #:filename (format "~a.atom" slug)
               #:id       (format "tag:rssing.arm32.ax,2025-03-06:feed/~a" slug)
               #:title    title
               #:articles (λ () (reddit-json-articles
                                  #:by                  "Douglasjm"
                                  #:when-in-subreddit   "HFY"
                                  #:when-title-contains title))))

    (let ([slug  "wearing-power-armor-to-a-magic-school"]
          [title "Wearing Power Armor to a Magic School"])
      (feed/kw #:filename (format "~a.atom" slug)
               #:id       (format "tag:rssing.arm32.ax,2025-03-06:feed/~a" slug)
               #:title    title
               #:articles (λ () (reddit-json-articles
                                  #:by                  "Jcb112"
                                  #:when-in-subreddit   "HFY"
                                  #:when-title-contains title))))))

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

; I'd really prefer it if Racket verified TLS cerficates by default like every other programming
; language I'm aware of, but it doesn't.
;
; This enables certificate verification for URL-based HTTP requests (including http-sendrecv/url),
; but not plain http-sendrecv. You still need to specify #:ssl? 'secure manually for those.
(current-https-protocol 'secure)

(for-each generate-feed feeds)

; vim: sw=2 ts=2 et
