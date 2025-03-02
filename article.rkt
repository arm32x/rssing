#lang racket

(require "./utils/dates.rkt")
(require "./utils/keyword-structs.rkt")

(provide (struct-out article) article/kw)
(provide article-title-contains?)
(provide article->xexpr)

(struct/kw article (; A URI that uniquely identifies the article
                    id
                    ; Title of the article
                    title
                    ; Date of the last significant update to the article
                    date-updated
                    ; Date the article was originally published
                    [date-published null]
                    ; Extra X-expressions to insert into the article metadata
                    [extra-metadata '()])
                   #:transparent)

(define (article-title-contains? article contained)
  (string-contains? (article-title article) contained))

(define (article->xexpr article)
  `(entry
     (id ,(article-id article))
     (title ,(article-title article))
     (updated ,(date->string/rfc3339 (article-date-updated article)))
     ; This is the most convenient way I can find to conditionally add an element
     ,@(match (article-date-published article)
         ['()            '()]
         [date-published `((published ,(date->string/rfc3339 date-published)))])))
