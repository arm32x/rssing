#lang racket

(require net/http-client)

(require "./threading.rkt")

(provide ensure-success-status-code)
(provide exn:fail:network:http)
(provide exn:fail:network:http?)
(provide exn:fail:network:http-status-code)
(provide exn:fail:network:http-reason-phrase)
(provide header-ref)

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

; TODO - Would it be better to eagerly parse the headers into a hash table?
(define (header-ref headers header-name)
  (ormap
    (match-λ
      [(regexp #px"^(\\S+): (.*)$" (list _ name-bytes value-bytes))
       (let ([name  (bytes->string/latin-1 name-bytes)]
             [value (bytes->string/latin-1 value-bytes)])
         (if (string-ci=? name header-name)
           value
           #f))]
      [_ #f])
    headers))
