#lang racket

(require "../the-little-schemer/prelude.rkt")
(require "../the-little-schemer/chapter2.rkt")
(require rackunit)

(check-true (member? 'sardines '(Italian sardines spaghetti parsley)))

(define is-first?
  (λ (a lat)
    (cond
      [(null? lat) #f]
      [else (eq? a (car lat))])))

(define is-first-b?
  (λ (a lat)
    (cond
      [(null? lat) #f]
      [else
       (or (eq? a (car lat))
           (two-in-a-row? lat))])))

; Check if any element appears twice in a row in a list.
(define two-in-a-row?
  (λ (lat)
    (cond
      [(null? lat) #f]
      [else (is-first-b? (car lat) (cdr lat))])))

(define two-in-a-row-b?
  (λ (preceding lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) preceding)
                (two-in-a-row-b? (car lat) (cdr lat)))])))

; Check if any element appears twice in a row in a list.
(define two-in-a-row-final?
  (λ (lat)
    (cond
      [(null? lat) #f]
      [else (two-in-a-row-b? (car lat) (cdr lat))])))

(check-false (two-in-a-row? '(Italian sardines spaghetti parsley)))
(check-true (two-in-a-row? '(Italian sardines sardines paghetti parsley)))
(check-false (two-in-a-row? '(Italian sardines spaghetti sardines parsley)))
(check-false (two-in-a-row-final? '(Italian sardines spaghetti parsley)))
(check-true (two-in-a-row-final? '(Italian sardines sardines paghetti parsley)))
(check-false (two-in-a-row-final? '(Italian sardines spaghetti sardines parsley)))

(define sum-of-prefixes-b
  (λ (s tup)
    (cond
      [(null? tup) '()]
      [else
       (cons (+ s (car tup))
             (sum-of-prefixes-b (+ s (car tup)) (cdr tup)))])))

; Calculate the sum of the "sequence" that is a tup.
(define sum-of-prefixes
  (λ (tup)
    (sum-of-prefixes-b 0 tup)))

(check-equal? (sum-of-prefixes '(2 1 9 17 0)) '(2 3 12 29 29))
(check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5))

;; The Eleventh Commandment: Use additional arguments when a function needs to
;; know what other arguments to the function have been like so far.

; Get the element at index n in a lat.
(define pick
  (λ (n lat)
    (cond
      [(zero? (sub1 n))
       (car lat)]
      [else
       (pick (sub1 n) (cdr lat))])))

(define scramble-b
  (λ (pre tup)
    (cond
      [(null? tup) '()]
      [else
       (cons
        (pick
         (car tup)
         (cons (car tup) pre))
        (scramble-b
         ; We build the prefix in reverse by consing this way. This makes it
         ; even easier to pick the correct element, as we don't need to "count
         ; backwards".
         (cons (car tup) pre)
         (cdr tup)))])))

; Scramble the elements of a tup: Let tup(n) denote the number at position n in
; tup. For each position n in tup, the number at position n in the result is
; tup(n - tup(tup(n))). For instance, let tup be '(1 1 1 3 4 2), and let n be 6.
; Then tup(6) is 2, and tup(2) is 1. The final position to look at is hence 6-1
; = 5. tup(5) is 4, so the result has 4 at position 6. As seen in scramble-b
; above, the recursive version of this function is much easier to construct and
; read.
(define scramble
  (λ (tup)
    (scramble-b '() tup)))

(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2))
              '(1 1 1 1 1 4 1 1 1 9))
(check-equal? (scramble '(1 2 3 4 5 6 7 8 9))
              '(1 1 1 1 1 1 1 1 1))
(check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10))
              '(1 1 1 1 1 1 1 1 2 8 2))
