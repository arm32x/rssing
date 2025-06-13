#lang racket

(require srfi/26)  ; cut macro
(require xml)

(require "./article.rkt")
(require "./feed.rkt")
(require "./sources/reddit-rss.rkt")
(require "./utils/threading.rkt")

; FIXME - Should this be here or in sources/reddit-rss.rkt?
(define (reddit-rss-feed #:slug      slug
                         #:title     title
                         #:username  username
                         #:subreddit [subreddit 'any])
  (feed/kw #:filename (format "~a.atom" slug)
           #:id       (format "tag:rssing.arm32.ax,2025-02-24:feed/~a" slug)
           #:title    title
           #:articles (λ () (filter (cut article-title-contains? <> title)
                                    (reddit-rss-articles (format "/user/~a/submitted.rss" username)
                                                         #:posts-only? #t
                                                         #:subreddit   subreddit)))))

(define feeds
  (list
    (reddit-rss-feed #:slug     "an-otherworldly-scholar"
                     #:title    "An Otherworldly Scholar"
                     #:username "ralo_ramone")

    (reddit-rss-feed #:slug     "dungeon-life"
                     #:title    "Dungeon Life"
                     #:username "Khenal")

    (reddit-rss-feed #:slug     "engineering-magic-and-kitsune"
                     #:title    "Engineering, Magic, and Kitsune"
                     #:username "SteelTrim")

    (reddit-rss-feed #:slug     "magic-is-electricity"
                     #:title    "Magic is Electricity?!"
                     #:username "97cweb")

    ; TODO - Add Patreon API support. This series releases one chapter ahead on Patreon for free.
    (reddit-rss-feed #:slug     "magic-is-programming"
                     #:title    "Magic is Programming"
                     #:username "Douglasjm")

    (reddit-rss-feed #:slug     "the-human-from-a-dungeon"
                     #:title    "The Human From a Dungeon"
                     #:username "itsdirector")

    (reddit-rss-feed #:slug      "theres-always-another-level"
                     #:title     "There's Always Another Level"
                     #:username  "PerilousPlatypus"
                     #:subreddit "PerilousPlatypus")

    (reddit-rss-feed #:slug     "wearing-power-armor-to-a-magic-school"
                     #:title    "Wearing Power Armor to a Magic School"
                     #:username "Jcb112")))

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

(for ([feed feeds])
  (printf "Generating ~a~n" (feed-filename feed))
  (generate-feed feed))

; vim: sw=2 ts=2 et
