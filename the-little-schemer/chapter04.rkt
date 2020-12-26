#lang racket

(require "prelude.rkt")
(require rackunit)

(check-true (atom? 14))

; Add 1 to a number.
(define add1
  (λ (n)
    (+ n 1)))

(check-equal? (add1 67)
              68)

; Subtract 1 from n.
(define sub1
  (λ (n)
    (- n 1)))

(check-equal? (sub1 5)
              4)
(check-true (zero? 0))

; Add two numbers m and n.
(define my+
  (λ (n m)
    (cond [(zero? n) m]
          [else (my+ (sub1 n) (add1 m))])))

(check-equal? (my+ 46 12)
              58)

; Subtract the number m from n.
(define my-
  (λ (n m)
    (cond [(zero? m) n]
          [else (sub1 (my- n (sub1 m)))])))

(check-equal? (my- 14 3)
              11)

;; (check-pred tup? '(2 11 3 79 47 6))
;; (check-pred tup? '(8 55 5 555))
;; (check-false (tup? '(1 2 8 apple 4 3)))
;; (check-false (tup? '(3 (7 4) 13 9)))
;; (check-pred tup? '())

; Calculate the sum over all numbers in a tuple.
(define addtup
  (λ (tup)
    (cond [(null? tup) 0]
          [else (my+ (car tup) (addtup (cdr tup)))])))

(check-equal? (addtup '(15 6 7 12 3))
              43)

;; The First Commandment (first revision): When recurring on a list of atoms,
;; lat, ask two questions about it: (null? lat) and else. When recurring on a
;; number, n, ask two questions about it: (zero? n) and else.

; Multiply two numbers n and m.
(define my*
  (λ (n m)
    (cond [(zero? n) 0]
          [else (my+ m (my* (sub1 n) m))])))

(check-equal? (my* 5 3)
              15)

;; The Fifth Commandment
;;
;; When building a value with +, always use 0 for the value of the terminating
;; line, for adding 0 does not change the value of an addition.
;;
;; When building a value with *, always use 1 for the value of the terminating
;; line, for multiplying by 1 does not change the value of a multiplication.
;;
;; When building a value with cons, always consider () for the value of the
;; terminating line.

; Add two tups of the same length in a point-wise fashion.
(define tup+
  (λ (tup1 tup2)
    (cond [(null? tup1) tup2]
          [(null? tup2) tup1]
          [else (cons (my+ (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))])))

(check-equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
              '(11 11 11 11 11))
(check-equal? (tup+ '(2 3) '(4 6))
              '(6 9))
(check-equal? (tup+ '(1 3) '(2 4 6))
              '(3 7 6))
(check-equal? (tup+ '(1 3 2) '(2))
              '(3 3 2))

; Check whether n is greater than m.
(define my>
  (λ (n m)
    (cond [(zero? n) #f]
          [(zero? m) #t]
          [else (my> (sub1 n) (sub1 m))])))

(check-false (my> 12 133))
(check-true (my> 120 11))
(check-false (my> 3 3))

; Check whether n is smaller than m.
(define my<
  (λ (n m)
    (cond [(zero? m) #f]
          [(zero? n) #t]
          [else (my< (sub1 n) (sub1 m))])))

(check-true (my< 12 133))
(check-false (my< 120 11))
(check-false (my< 1 1))

; Check whether two integer numbers are equal.
(define my=
  (λ (n m)
    (cond [(or (my< n m) (my> n m)) #f]
          [else #t])))

(check-true (my= 2 2))
(check-false (my= 2 1))
(check-false (my= 4 41))

; Calculate n to the m-th power.
(provide pow)
(define pow
  (λ (n m)
    (cond [(zero? m) 1]
          [else (my* n (pow n(sub1 m)))])))

(check-equal? (pow 1 1)
              1)
(check-equal? (pow 2 3)
              8)
(check-equal? (pow 5 3)
              125)

; Divide the integer n by m (and forget about the remainder).
(define div
  (λ (n m)
    (cond [(my< n m) 0]
          [(add1 (div (- n m) m))])))

(check-equal? (div 15 4)
              3)

; Get the length of a list of atoms.
(define my-length
  (λ (lat)
    (cond [(null? lat) 0]
          [else (add1 (my-length (cdr lat)))])))

(check-equal? (my-length '(hotdogs with mustard sauerkraut and pickles))
              6)
(check-equal? (my-length '())
              0)

; Get the n-th element of a list.
(provide pick)
(define pick
  (λ (n elems)
    (cond [(zero? (sub1 n)) (car elems)]
          [else (pick (sub1 n) (cdr elems))])))

(check-equal? (pick 4 '(lasagna spaghetti ravioli macaroni meatball))
              'macaroni)
(check-exn exn:fail? (λ () (pick 0 '(a b c)))) ; we are working 1-indexed

; Remove the n-th element from a list.
(define rempick
  (λ (n elems)
    (cond [(zero? (sub1 n)) (cdr elems)]
          [else (cons (car elems) (rempick (sub1 n) (cdr elems)))])))

(check-equal? (rempick 3 '(hotdogs with hot mustard))
              '(hotdogs with mustard))

; Remove all numbers from a lat.
(define no-nums
  (λ (lat)
    (cond [(null? lat) '()]
          [else (if (number? (car lat))
                    (no-nums (cdr lat))
                    (cons (car lat)
                          (no-nums (cdr lat))))])))

(check-equal? (no-nums '(a 3 2 b 18 c d f 1))
              '(a b c d f))

; Return a tup consisting of all the numbers in a lat.
(define all-nums
  (λ (lat)
    (cond [(null? lat) '()]
          [else (if (number? (car lat))
                    (cons (car lat)
                          (all-nums (cdr lat)))
                    (all-nums (cdr lat)))])))

(check-equal? (all-nums '(a 3 2 b 1 c 1 1))
              '(3 2 1 1 1))

; Check if two atoms are the same.
(provide eqan?)
(define eqan?
  (λ (a b)
    (cond [(and (number? a) (number? b)) (= a b)]
          [(or (number? a) (number? b)) #f]
          [else (eq? a b)])))

(check-true (eqan? 'a 'a))
(check-false (eqan? 'a 'b))
(check-false (eqan? 2 'a))
(check-false (eqan? 'a 2))
(check-false (eqan? 1 2))
(check-true (eqan? 1 1))

; Count how often the atom a occurs in lat.
(define occur
  (λ (a lat)
    (cond [(null? lat) 0]
          [else
           (cond [(eqan? a (car lat))
                  (add1 (occur a (cdr lat)))]
                 [else (occur a (cdr lat))])])))

(check-equal? (occur 'a '(a b c a a a c))
              4)
(check-equal? (occur 3 '(a b c))
              0)

; Check if an atom is the number 1.
(define one?
  (λ (a)
    (eqan? a 1)))

(check-true (one? 1))
(check-false (one? 2))
(check-false (one? 'a))

; Remove the n-th element from a list.
(define rempick-revised
  (λ (n elems)
    (cond [(one? n) (cdr elems)]
          [else (cons (car elems)
                      (rempick (sub1 n) (cdr elems)))])))
