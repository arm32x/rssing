#lang racket

(provide reddit-host)
(provide redlib-host)
(provide rewrite-reddit-urls)

; The host to request Reddit RSS feeds from
(define reddit-host (make-parameter "www.reddit.com"))
; The host of the redlib instance to use in generated feeds
(define redlib-host (make-parameter "redlib.catsarch.com"))

; TODO - Use a smarter implementation, possibly the one from Redlib
(define (rewrite-reddit-urls url-string)
  (string-replace url-string (reddit-host) (redlib-host) #:all? #t))
