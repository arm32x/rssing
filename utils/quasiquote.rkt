#lang racket/base

(provide qq-when)

(define-syntax-rule (qq-when condition expr ...)
  (if condition
    (list (begin expr ...))
    (list)))
