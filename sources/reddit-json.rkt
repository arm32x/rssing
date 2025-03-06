#lang racket

(require json)
(require net/url)
(require net/url-connect)
(require racket/generator)

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
                              username
                              ; A function that filters Reddit submissions as jsexprs before they're
                              ; processed into articles
                              #:when          [predicate (λ _ #t)]
                              ; If true, rewrite all links to point to Redlib
                              #:rewrite-urls? [rewrite-urls? #t]
                              ; Maximum number of articles to return
                              #:max-articles  [max-articles 10]
                              ; Maximum number of requests to make to Reddit before giving up
                              #:max-requests  [max-requests 5])
  '() #| TODO |#)
