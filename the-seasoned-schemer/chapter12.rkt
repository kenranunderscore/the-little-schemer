#lang racket

(require "../the-little-schemer/prelude.rkt")
(require "../the-little-schemer/chapter9.rkt")
(require rackunit)

;; (define multirember
;;   (λ (a lat)
;;     ((Y (λ (mr)
;;           (λ (lat)
;;             (cond
;;               [(null? lat) '()]
;;               [(eq? a (car lat))
;;                (mr (cdr lat))]
;;               [else
;;                (cons (car lat) (mr (cdr lat)))]))))
;;      lat)))

(define multirember
  (λ (a lat)
    (letrec
        ([mr (λ (lat)
               (cond
                 [(null? lat) '()]
                 [(eq? a (car lat))
                  (mr (cdr lat))]
                 [else
                  (cons
                   (car lat)
                   (mr (cdr lat)))]))])
      (mr lat))))

(check-equal? (multirember 'tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

;; The Twelfth Commandment: Use (letrec ...) to remove arguments that do not
;; change for recursive applications.

; Given a function to compare two S-expression with, return a function that
; removes all occurrences of a given atom in a list.
(define multirember-f
  (λ (test?)
    (λ (a lat)
      (letrec
          ([mr (λ (lat)
                 (cond
                   [(null? lat) '()]
                   [(test? a (car lat))
                    (mr (cdr lat))]
                   [else
                    (cons
                     (car lat)
                     (mr (cdr lat)))]))])
        (mr lat)))))

(check-equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

; Check whether an atom is contained in a lat.
(define member?
  (λ (a lat)
    (letrec
        ([yes? (λ (l)
                 (cond
                   [(null? l) #f]
                   [(eq? (car l) a) #t]
                   [else (yes? (cdr l))]))])
      (yes? lat))))

(check-true (member? 'a '(b c d a)))

; Return the union of two sets.
(define union
  (λ (s1 s2)
    (letrec
        ([m?
          (λ (a lat)
            (letrec ([n? (λ (lat)
                           (cond
                             [(null? lat) #f]
                             [(eq? a (car lat)) #t]
                             [else (n? (cdr lat))]))])
              (n? lat)))]
         [u
          (λ (s)
            (cond
              [(m? (car s) s2) s2]
              [else
               (cons
                (car s)
                (u (cdr s)))]))])
      (u s1))))

(check-equal? (union '(1 2 3) '(4 3))
              '(1 2 4 3))

;; The Thirteenth Commandment: Use (letrec ...) to hide and protect functions.

(define two-in-a-row?
  (letrec
      ([w? (λ (a lat)
             (cond
               [(null? lat) #f]
               [else
                (or (eq? (car lat) a)
                    (w? (car lat) (cdr lat)))]))])
    (λ (lat)
      (cond
        [(null? lat) #f]
        [else (w? (car lat) (cdr lat))]))))

(check-false (two-in-a-row? '(Italian sardines spaghetti parsley)))
(check-true (two-in-a-row? '(Italian sardines sardines paghetti parsley)))
(check-false (two-in-a-row? '(Italian sardines spaghetti sardines parsley)))

(define sum-of-prefixes
  (letrec
      ([r (λ (s tup)
            (cond
              [(null? tup) '()]
              [else
               (cons (+ s (car tup))
                     (r (+ s (car tup))
                        (cdr tup)))]))])
    (λ (tup)
      (r 0 tup))))

(check-equal? (sum-of-prefixes '(1 2 7 3))
              '(1 3 10 13))

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

(define pick
  (λ (n l)
    (cond
      [(zero? (sub1 n)) (car l)]
      [else (pick (sub1 n) (cdr l))])))

(define scramble
  (λ (tup)
    (letrec ([p (λ (pre tup)
                  (cond
                    [(null? tup) '()]
                    [else
                     (cons
                      (pick
                       (car tup)
                       (cons (car tup) pre))
                      (scramble-b
                       (cons (car tup) pre)
                       (cdr tup)))]))])
      (p '() tup))))

(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2))
              '(1 1 1 1 1 4 1 1 1 9))
