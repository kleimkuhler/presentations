#lang racket

;; Slide: 3
(define (creal x)
  (cond [(number? x) x]
        [else (car x)]))
