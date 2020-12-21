#lang racket

(require "prelude.rkt")
(require "chapter4.rkt")
(require "chapter6.rkt")
(require rackunit)

; Like rember, but we pass a function that knows how to compare two
; S-expressions.
;; (define rember-f
;;   (λ (test? a l)
;;     (cond
;;       [(null? l) '()]
;;       [(test? a (car l)) (cdr l)]
;;       [else (cons (car l)
;;                   (rember-f test? a (cdr l)))])))

; Slightly curried variant of the above rember-f.
(define rember-f
  (λ (test?)
    (λ (a l)
      (cond
        [(null? l) '()]
        [(test? a (car l)) (cdr l)]
        [else (cons (car l)
                    ((rember-f test?) a (cdr l)))]))))

(check-equal? ((rember-f =) 5 '(6 2 5 3))
              '(6 2 3))
(check-equal? ((rember-f eq?) 'jelly '(jelly beans are good))
              '(beans are good))
(check-equal? ((rember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake)))
              '(lemonade and (cake)))

; Insert new to the left of the first occurrence of old in lat.
(define insertL-f
  (λ (test?)
    (λ (new old lat)
      (cond
        [(null? lat) '()]
        [else (cond
                [(test? (car lat) old) (cons new lat)]
                [else (cons (car lat) ((insertL-f test?) new old (cdr lat)))])]))))

; Insert new to the right of the first occurrence of old in lat.
(define insertR-f
  (λ (test?)
    (λ (new old lat)
      (cond
        [(null? lat) '()]
        [else (cond
                [(test? (car lat) old) (cons old (cons new (cdr lat)))]
                [else (cons (car lat) (insertR-f new old (cdr lat)))])]))))

; Abstract over the function that decides where to insert stuff.
(define insert-g
  (λ (g)
    (λ (test?)
      (λ (new old l)
        (cond
          [(null? l) '()]
          [(test? (car l) old)
           (g new old (cdr l))]
          [else
           (cons (car l)
                 (((insert-g g) test?) new old (cdr l)))])))))

(define seqL
  (λ (new old l)
    (cons new (cons old l))))

(define seqR
  (λ (new old l)
    (cons old (cons new l))))

(define seqS
  (λ (new old l)
    (cons new l)))

; insertL
(check-equal?
 (((insert-g seqL) equal?) 'new 'old '(a b old c))
 '(a b new old c))
; insertR
(check-equal?
 (((insert-g seqR) equal?) 'new 'old '(a b old c))
 '(a b old new c))
; subst
(check-equal?
 (((insert-g seqS) equal?) 'new 'old '(a b old c))
 '(a b new c))

;; The Ninth Commandment: Abstract common patterns with a new function.

; Return for each aexp operation atom the corresponding function.
(define atom->function
  (λ (a)
    (cond [(eq? a '+) +]
          [(eq? a '*) *]
          [else pow])))

(check-equal? (atom->function '+)
              +)

; New version using atom->function.
(define value
  (λ (nexp)
    (cond
      [(atom? nexp) nexp]
      [else
       ((atom->function (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))])))

; Remove all occurrences of x in lat.
(define multirember-f
  (λ (test?)
    (λ (x lat)
      (cond [(null? lat) '()]
            [else (cond
                    [(test? (car lat) x) ((multirember-f test?) x (cdr lat))]
                    [else (cons (car lat) ((multirember-f test?) x (cdr lat)))])]))))

(check-equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

; Our old multirember defined in terms of multirember-f.
(define multirember-eq?
  (multirember-f eq?))

; A version of multirember that takes a unary predicate only.
(define multiremberT
  (λ (test? lat)
    (cond
      [(null? lat) '()]
      [(test? (car lat))
       (multiremberT test? (cdr lat))]
      [else (cons (car lat)
                  (multiremberT test? (cdr lat)))])))

(check-equal? (multiremberT (λ (x) (eq? 'tuna x))
                            '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

(define multirember&co
  (λ (a lat col)
    (cond
      [(null? lat)
       (col '() '())]
      [(eq? (car lat) a)
       (multirember&co
        a
        (cdr lat)
        (λ (newlat seen)
          (col newlat (cons (car lat) seen))))]
      [else
       (multirember&co
        a
        (cdr lat)
        (λ (newlat seen)
          (col (cons (car lat) newlat) seen)))])))

(define a-friend
  (λ (x y)
    (null? y)))

(check-equal? (multirember&co 'tuna '() a-friend)
              #t)
(check-equal? (multirember&co 'tuna '(tuna) a-friend)
              #f)
(check-equal? (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
              #f)
(check-equal? (multirember&co
               'tuna
               '(strawberries tuna and swordfish)
               (λ (x y)
                 (length x)))
              3)

;; The Tenth Commandment: Build functions to collect more than one value at a
;; time.

(define multiinsertL
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old (cdr lat))))]
      [else
       (cons (car lat)
             (multiinsertL new old (cdr lat)))])))

(check-equal? (multiinsertL 'tuna 'fish '(a fish is a fish))
              '(a tuna fish is a tuna fish))

(define multiinsertR
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old (cdr lat))))]
      [else
       (cons (car lat)
             (multiinsertR new old (cdr lat)))])))

(check-equal? (multiinsertR 'fish 'tuna '(a tuna is a tuna))
              '(a tuna fish is a tuna fish))

(define multiinsertLR
  (λ (new oldR oldL lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldR oldL (cdr lat))))]
      [(eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldR oldL (cdr lat))))]
      [else
       (cons (car lat)
             (multiinsertLR new oldR oldL (cdr lat)))])))

(check-equal? (multiinsertLR 'fish 'tuna 'guppy '(a tuna is not a guppy))
              '(a tuna fish is not a fish guppy))

; Like multiinsertLR, but with continuations.
(define multiinsertLR&co
  (λ (new oldR oldL lat col)
    (cond
      [(null? lat) (col '() 0 0)]
      [(eq? (car lat) oldR)
       (multiinsertLR
        new oldR oldL (cdr lat)
        (λ (newlat L R)
          (col
           (cons oldR (cons new lat))
           L
           (+ R 1))))]
      [(eq? (car lat) oldL)
       (multiinsertLR
        new oldR oldL (cdr lat)
        (λ (newlat L R)
          (col
           (cons new (cons oldL lat))
           (+ L 1)
           R)))]
      [else
       (multiinsertLR
        new oldR oldL (cdr lat)
        (λ (newlat L R)
          (col (cons (car lat) newlat) L R)))])))

; Keep only the even numbers in a list and all S-expressions it contains.
(define evens-only*
  (λ (l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(even? (car l))
          (cons (car l) (evens-only* (cdr l)))]
         [else
          (evens-only* (cdr l))])]
      [else
       (cons (evens-only* (car l))
             (evens-only* (cdr l)))])))

(check-equal? (evens-only* '((2 3 3 4) (((2) 1 4) 3) 1))
              '((2 4) (((2) 4))))

; Like evens-only*, but also calculates the product of the even and the sum of
; the odd numbers it finds and passes those to a continuation.
(define evens-only*&co
  (λ (l col)
    (cond
      [(null? l) (col '() 1 0)]
      [(atom? (car l))
       (cond
         [(even? (car l))
          (evens-only*&co
           (cdr l)
           (λ (new-l p s)
             (col
              (cons (car l) new-l)
              (* (car l) p)
              s)))]
         [else
          (evens-only*&co
           (cdr l)
           (λ (new-l p s)
             (col
              new-l
              p
              (+ (car l) s))))])]
      [else
       (evens-only*&co
        (car l)
        (λ (carl carp cars)
          (evens-only*&co
           (cdr l)
           (λ (new-l p s)
             (col
              (cons carl new-l)
              (* carp p)
              (+ cars s))))))])))

(check-equal?
 (evens-only*&co
  '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
  (λ (l p s)
    (cons s (cons p l))))
 '(38 1920 (2 8) 10 (() 6) 2))
