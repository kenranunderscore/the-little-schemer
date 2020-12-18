#lang racket

(provide atom?)
(provide lat?)

(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))

(define lat?
  (λ (x)
    (cond [(null? x) #t]
          [(atom? (car x)) (lat? (cdr x))]
          [else #f])))
