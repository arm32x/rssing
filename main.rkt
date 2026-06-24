#lang racket

(require srfi/26)  ; cut macro
(require xml)

(require "./article.rkt")
(require "./feed.rkt")
(require "./sources/reddit-rss.rkt")
(require "./utils/threading.rkt")

(define feeds
  (list
    (reddit-rss-feed #:slug      "magic-is-electricity"
                     #:title     "Magic is Electricity?!"
                     #:username  "97cweb")

    (reddit-rss-feed #:slug      "theres-always-another-level"
                     #:title     "There's Always Another Level"
                     #:username  "PerilousPlatypus"
                     #:subreddit "PerilousPlatypus")

    (reddit-rss-feed #:slug      "wearing-power-armor-to-a-magic-school"
                     #:title     "Wearing Power Armor to a Magic School"
                     #:username  "Jcb112")))

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

; Make sure output is line-buffered so GitHub Actions logs aren't delayed
(with-handlers ([exn:fail?
                 (λ () (eprintf "Warning: Could not set stdout to line-buffered. Messages may be delayed~n"))])
  (file-stream-buffer-mode (current-output-port) 'line))

(for ([feed feeds])
  (printf "Generating ~a~n" (feed-filename feed))
  (generate-feed feed))

; vim: sw=2 ts=2 et
