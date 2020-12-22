#lang racket

(provide atom?)
(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))

(provide lat?)
(define lat?
  (λ (x)
    (cond [(null? x) #t]
          [(atom? (car x)) (lat? (cdr x))]
          [else #f])))
