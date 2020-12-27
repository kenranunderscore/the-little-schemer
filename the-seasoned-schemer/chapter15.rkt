#lang racket

(require "../the-little-schemer/prelude.rkt")
(require rackunit)

(define x (cons 'chicago (cons 'pizza '())))

(define gourmet
  (λ (food)
    (cons food (cons x '()))))

(set! x 'skins)

(check-equal? (gourmet 'onion) '(onion skins))

(set! x 'rings)

(check-equal? (gourmet 'onion) '(onion rings))

(define gourmand
  (λ (food)
    (set! x food)
    (cons food
          (cons x '()))))

(check-equal? (gourmand 'potato) '(potato potato))
(check-equal? x 'potato)
(check-equal? (gourmand 'rice) '(rice rice))
(check-equal? x 'rice)

(define diner
  (λ (food)
    (cons 'milkshake
          (cons food '()))))

(define dinerR
  (λ (food)
    (set! x food)
    (cons 'milkshake
          (cons food '()))))

(check-equal? (dinerR 'onion) '(milkshake onion))
(check-equal? (dinerR 'pecanpie) '(milkshake pecanpie))
(check-equal? x 'pecanpie)
(check-equal? (gourmand 'onion) '(onion onion))
;; Now dinerR doesn't remember anymore what was eaten last.
(check-equal? x 'onion)

(define omnivore
  (let ([x 'minestrone])
    (λ (food)
      (set! x food)
      (cons food
            (cons x '())))))

(check-equal? (omnivore 'bouillabaisse) '(bouillabaisse bouillabaisse))

;;; The Sixteenth Commandment: Use (set! ...) only with names defined in
;;; (let ;;; ...)s.

;;; The Seventeenth Commandment (preliminary): Use (set! ...) for (let ((x ...))
;;; ...) only if there is at least one (lambda ... between it and the (let ((x
;;; ...)) ...).

(define food 'none)

(define glutton
  (λ (x)
    (set! food x)
    (cons 'more
          (cons x
                (cons 'more
                      (cons x '()))))))

(check-equal? (glutton 'garlic) '(more garlic more garlic))
(check-equal? x 'onion)
(check-equal? food 'garlic)

;; Swap the values of x and food.
(define chez-nous
  (λ ()
    (let ([old-x x])
      (set! x food)
      (set! food old-x))))

(chez-nous)
(check-equal? x 'garlic)
(check-equal? food 'onion)

;;; The Eighteenth Commandment: Use (set! ...) only when the value that x refers
;;; to is no longer needed.
