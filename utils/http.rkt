#lang racket

(require net/url)
(require net/http-client)

(require "./threading.rkt")

(provide ensure-success-status-code)
(provide exn:fail:network:http)
(provide url/kw)

(struct exn:fail:network:http exn:fail:network (status-code reason-phrase)
  #:extra-constructor-name make-exn:fail:network:http
  #:transparent)

(define (ensure-success-status-code status-line)
  (match-let* ([(regexp
                  #px"^[^ ]* ([^ ]*) (.*)$"
                  (list _ status-code-bytes reason-phrase-bytes))
                status-line]
               [status-code   (string->number (bytes->string/latin-1 status-code-bytes))]
               [reason-phrase (bytes->string/latin-1 reason-phrase-bytes)])
    (when (or (< status-code 200) (> status-code 299))
      (raise (exn:fail:network:http
               (format "HTTP status code does not indicate success: ~a ~a" status-code reason-phrase)
               (current-continuation-marks)
               status-code
               reason-phrase)))))

(define (url/kw #:scheme         [scheme #f]
                #:user           [user #f]
                #:host           [host #f]
                #:port           [port #f]
                #:path-absolute? [path-absolute? #t]
                #:path           [path '()]
                #:query          [query '()]
                #:fragment       [fragment #f])
  (url scheme
       user
       host
       port
       path-absolute?
       path
       query
       fragment))
