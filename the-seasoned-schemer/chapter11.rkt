#lang racket

(require "../the-little-schemer/prelude.rkt")
(require "../the-little-schemer/chapter2.rkt")
(require rackunit)

(check-true (member? 'sardines '(Italian sardines spaghetti parsley)))

(define car-eq?
  (λ (a lat)
    (cond
      [(null? lat) #f]
      [else (eq? a (car lat))])))

; Check if any element appears twice in a row in a list.
(define two-in-a-row?
  (λ (lat)
    (cond
      [(null? lat) #f]
      [(car-eq? (car lat) (cdr lat)) #t]
      [else (two-in-a-row? (cdr lat))])))

(check-false (two-in-a-row? '(Italian sardines spaghetti parsley)))
(check-true (two-in-a-row? '(Italian sardines sardines paghetti parsley)))
(check-false (two-in-a-row? '(Italian sardines spaghetti sardines parsley)))
