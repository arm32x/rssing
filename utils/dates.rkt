#lang racket

(require srfi/19)  ; Date parsing

(provide date->string/rfc3339)

(define (date->string/rfc3339 date)
  ; ISO 8601 as implemented by Racket is missing the colon in the time zone
  (let* ([iso8601-string (date->string date "~4")]
         [rfc3339-string (regexp-replace #px"([-+]\\d{2})(\\d{2})$" iso8601-string "\\1:\\2")])
    rfc3339-string))
