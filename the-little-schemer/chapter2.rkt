#lang racket

(require "prelude.rkt")
(require rackunit)

(check-true (lat? '(Jack Sprat could eat no chicken fat)))

(check-false (lat? '((Jack) Sprat could eat no chicken fat)))

(check-false (lat? '(Jack (Sprat could) eat no chicken fat)))

(check-true (lat? '()))

(check-true (or (null? '()) (atom? '(d e f g))))

(check-true (or (null? '(a b c)) (null? '())))

(check-false (or (null? '(a b c)) (null? '(atom))))

(provide member?)
(define member?
  (λ (elem lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) elem)
                (member? elem (cdr lat)))])))

(check-false (member? 'poached '(fried eggs and scrambled eggs)))

(check-true (member? 'meat '(mashed potatoes and meat gravy)))

;; The First Commandment (preliminary): Always ask null? as the first question
;; in expressing any function.
