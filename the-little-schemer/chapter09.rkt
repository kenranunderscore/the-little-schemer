#lang racket

(require "prelude.rkt")
(require "chapter04.rkt")
(require "chapter07.rkt")
(require rackunit)

(define keep-looking
  (λ (a i lat)
    (cond
      [(number? (pick i lat))
       (keep-looking a (pick i lat) lat)]
      [else (eq? (pick i lat) a)])))

; Look for an atom in list by starting at the first element, then, if that's a
; number, intepreting it as the index for the next lookup.
(define looking
  (λ (a lat)
    (keep-looking a (car lat) lat)))

(check-true (looking 'caviar '(6 2 4 caviar 5 7 3)))
(check-false (looking 'caviar '(6 2 grits caviar 5 7 3)))

(define shift
  (λ (p)
    (build (first (first p))
           (build (second (first p))
                  (second p)))))

(check-equal? (shift '((a b) c))
              '(a (b c)))
(check-equal? (shift '((a b) (c d)))
              '(a (b (c d))))

(define align
  (λ (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora))
       (align (shift pora))]
      [else (build (first pora)
                   (align (second pora)))])))

; Count the atoms in a pair-or-atom.
(define length*
  (λ (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (length* (first pora))
               (length* (second pora)))])))

; Calculate the "weight" of a pora. Similar to length*, but it gives the first
; part of a pair a factor of 2.
(define weight*
  (λ (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (* 2 (weight* (first pora)))
               (weight* (second pora)))])))

(check-equal? (weight* '((a b) c))
              7)
(check-equal? (weight* '(a (b c)))
              5)

; This is another example for a partial function. It does not terminate for all
; arguments.
(define shuffle
  (λ (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora))
       (shuffle (revpair pora))]
      [else
       (build (first pora)
              (shuffle (second pora)))])))

; Collatz' function. His conjecture is that for every positive n, this always
; reaches 1 eventually.
(define C
  (λ (n)
    (cond
      [(= n 1) 1]
      [(even? n) (C (/ n 2))]
      [else (C (add1 (* 3 n)))])))

; The Ackermann function. It is total, but very hard to calculate as it produces
; enormous numbers.
(define A
  (λ (n m)
    (cond
      [(zero? n) (add1 m)]
      [(zero? m) (A (sub1 n) 1)]
      [else (A (sub1 n) (A n (sub1 m)))])))

; The applicative-order Y combinator.
(provide Y)
(define Y
  (λ (le)
    ((λ (f) (f f))
     (λ (f)
       (le (λ (x) ((f f) x)))))))

; I should read this at least one more time.
