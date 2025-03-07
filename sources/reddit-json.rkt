#lang racket

(require json)
(require net/url)
(require net/url-connect)
(require racket/generator)
(require srfi/19)  ; Date handling

(require "../article.rkt")
(require "./reddit-common.rkt")
(require "../utils/http.rkt")
(require "../utils/threading.rkt")

(provide reddit-json-articles)

(define (reddit-json-user-submissions username
                                      #:after  [after #f]
                                      #:limit  [limit #f])
  (let*-values ([[request-url] (~> (url/kw #:scheme "https" #:host (reddit-host))
                                   (combine-url/relative (format "/user/~a/submitted.json" username))
                                   (struct-copy url _ [query `((sort . "new")
                                                               ,@(if after `((after . ,after)) '())
                                                               ,@(if limit `((limit . ,(~a limit))) '()))]))]
                [[status-line headers input-port] (http-sendrecv/url request-url)])
    (ensure-success-status-code status-line)
    (let* ([response-jsexpr (read-json input-port)]
           [response-jsexpr (hash-ref response-jsexpr 'data)])
      (cons
        (map (λ (x) (hash-ref x 'data))
             (hash-ref response-jsexpr 'children))
        (hash-ref response-jsexpr 'after)))))

(define (reddit-json-user-submissions/stream username
                                             #:after             [after #f]
                                             #:limit-per-request [limit-per-request #f]
                                             #:max-requests      max-requests)
  (if (<= max-requests 0)
    (stream)
    (match-let ([(cons submissions next-after) (reddit-json-user-submissions username
                                                                             #:after after
                                                                             #:limit limit-per-request)])
      (stream-append submissions
                     (stream-lazy #:who 'reddit-json-user-submissions/stream
                                  (reddit-json-user-submissions/stream username
                                                                       #:after             next-after
                                                                       #:limit-per-request limit-per-request
                                                                       #:max-requests      (- max-requests 1)))))))

(define (reddit-json-articles ; Username of Reddit user who posts the articles of interest (without u/)
                              #:by            username
                              ; A function that filters Reddit submissions as jsexprs before they're
                              ; processed into articles
                              #:when          [predicate (λ _ #t)]
                              ; Maximum number of articles to return
                              #:max-articles  [max-articles 10]
                              ; Maximum number of requests to make to Reddit before giving up
                              #:max-requests  [max-requests 5])
  (for/list ([submission-jsexpr (~> (reddit-json-user-submissions/stream username #:max-requests max-requests)
                                    (stream-filter predicate _))]
             [index             (in-naturals)]
             #:break (>= index max-articles))
    (let ([id              (hash-ref submission-jsexpr 'name)]
          [title           (hash-ref submission-jsexpr 'title)]
          [author-username (hash-ref submission-jsexpr 'author)]
          [date-updated    (hash-ref submission-jsexpr 'edited)]
          [date-published  (hash-ref submission-jsexpr 'created)]
          [url             (hash-ref submission-jsexpr 'url)]
          [content         (hash-ref submission-jsexpr 'selftext_html)])
      (article/kw #:id             (format "tag:rssing.arm32.ax,2025-03-02:reddit/~a" id)
                  #:title          title
                  #:url            (rewrite-reddit-urls url)
                  #:content        (cons 'html (rewrite-reddit-urls content))
                  ; Reddit returns false instead of a timestamp if the post has never been edited
                  #:date-updated   (if date-updated
                                     (seconds->date date-updated #f)
                                     (seconds->date date-published #f))
                  #:date-published (seconds->date date-published #f)
                  #:extra-metadata `((author
                                       (name ,(format "/u/~a" author-username))
                                       (uri ,(format "https://~a/user/~a" (redlib-host) author-username))))))))
