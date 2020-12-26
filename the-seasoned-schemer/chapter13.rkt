#lang racket

(require "../the-little-schemer/prelude.rkt")
(require "chapter12.rkt")
(require rackunit)

;; Return the intersection of two sets.
(define intersect
  (λ (s1 s2)
    (letrec ([i (λ (s)
                  (cond
                    [(null? s) '()]
                    [(member? (car s) s2)
                     (cons (car s) (i (cdr s)))]
                    [else (i (cdr s))]))])
      (i s1))))

(define intersectall
  (λ (lset)
    (letrec ([a (λ (lset)
                  (cond
                    [(null? (cdr lset)) (car lset)]
                    [else (intersect (car lset) (a (cdr lset)))]))])
      (cond
        [(null? lset) '()]
        [else (a lset)]))))

(check-equal? (intersectall '((1 2 3) (4 2 3) (2 3 4) (3 5)))
              '(3))

(define intersectall-letcc
  (λ (lset)
    (let/cc hop
      (letrec ([a (λ (lset)
                    (cond
                      [(null? (car lset))
                       (hop '())]
                      [(null? (cdr lset))
                       (car lset)]
                      [else (intersect (car lset) (a (cdr lset)))]))]
               [i (λ (s1 s2)
                    (letrec ([j (λ (s)
                                  (cond
                                    [(null? s) '()]
                                    [(member? (car s) s2) (j (cdr s))]
                                    [else (cons (car s) (j (cdr s)))]))])
                      (cond
                        ; s2 might be empty "from the start", or because the
                        ; intersection of all the sets so far is already empty.
                        ; In any case, we hop out and stop execution/evaluation.
                        [(null? s2) (hop '())]
                        [else (j s1)])))])
        (cond
          [(null? lset) '()]
          [else (a lset)])))))

(check-equal? (intersectall-letcc '((1 2 3) (4 2 3) (2 3 4) (3 5)))
              '(3))
(check-equal? (intersectall-letcc '((1 2 3) () (2 3 4) (3 5)))
              '())

;;; The Fourteenth Commandment: Use (let/cc ...) to return values abruptly and
;;; promptly.

;; Our old friend rember, but implemented with letrec this time.
(define rember
  (λ (a lat)
    (letrec ([r (λ (lat)
                  (cond
                    [(null? lat) '()]
                    [(eq? (car lat) a) (cdr lat)]
                    [else (cons (car lat)
                                (r (cdr lat)))]))])
      (r lat))))

(check-equal? (rember 'a '(b c a a d))
              '(b c a d))

;; Remove the rest of a lat after encountering a for the first time.
(define rember-beyond-first
  (λ (a lat)
    (letrec ([r (λ (lat)
                  (cond
                    [(null? lat) '()]
                    [(eq? (car lat) a) '()]
                    [else (cons (car lat)
                                (r (cdr lat)))]))])
      (r lat))))

(check-equal?
 (rember-beyond-first
  'roots
  '(noodles
    spaghetti spaetzle bean-thread
    roots
    potatoes
    yam
    others
    rice))
 '(noodles spaghetti spaetzle bean-thread))
(check-equal?
 (rember-beyond-first 'not-there '(a b c))
 '(a b c))

;; Remove all elements of a lat up to and including the last occurrence of a.
(define rember-upto-last
  (λ (a lat)
    (let/cc skip
      (letrec ([r (λ (lat)
                    (cond
                      [(null? lat) '()]
                      [(eq? (car lat) a)
                       (skip (r (cdr lat)))]
                      [else (cons (car lat) (r (cdr lat)))]))])
        (r lat)))))

(check-equal?
 (rember-upto-last
  'roots
  '(noodles
    spaghetti spaetzle bean-thread
    roots
    potatoes
    yam
    others
    rice))
 '(potatoes yam others rice))
(check-equal?
 (rember-upto-last 'not-there '(a b c d))
 '(a b c d))
(check-equal?
 (rember-upto-last 'a '(a b a a c a d e))
 '(d e))
