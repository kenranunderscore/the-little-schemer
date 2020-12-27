#lang racket

(require "../the-little-schemer/prelude.rkt")
(require "chapter12.rkt")
(require rackunit)

;; Find the leftmost item in a list. If the list is the empty list, it is
;; returned.
(define leftmost
  (λ (l)
    (cond
      [(null? l) '()]
      [(atom? (car l)) (car l)]
      [else
       (let ([a (leftmost (car l))])
         (cond
           [(atom? a) a]
           [else (leftmost (cdr l))]))])))

(check-equal? (leftmost '((() (c (a))) b)) 'c)
(check-equal? (leftmost '()) '())

;; Remove (only) the leftmost occurrence of a in a list l.
(define rember1*
  (λ (a l)
    (letrec ([r (λ (l)
                  (cond
                    [(null? l) '()]
                    [(atom? (car l))
                     (cond
                       [(eq? (car l) a) (cdr l)]
                       [else (cons (car l) (r (cdr l)))])]
                    [else
                     (let ([rc (r (car l))])
                       (cond
                         [(equal? (car l) rc)
                          (cons (car l) (r (cdr l)))]
                         [else (cons (r (car l)) (cdr l))]))]))])
      (r l))))

(check-equal? (rember1* 'salad '((Swedish rye)
                                 (French (mustard salad turkey))
                                 salad))
              '((Swedish rye)
                (French (mustard turkey))
                salad))

;;; The Fifteenth Commandment: Use (let ...) to name the values of repeated
;;; expressions.

;; Determine the depth of a (nested) list.
(define depth*
  (λ (l)
    (cond
      [(null? l) 1]
      [(atom? (car l)) (depth* (cdr l))]
      [else (max (add1 (depth* (car l)))
                 (depth* (cdr l)))])))

(check-equal? (depth* '((pickled) peppers (peppers pickled)))
              2)
(check-equal? (depth* '(margarine
                        ((bitter butter)
                         (makes)
                         (batter (bitter)))
                        butter))
              4)
(check-equal? (depth* '(()
                        ((bitter butter)
                         (makes)
                         (batter (bitter)))
                        butter))
              4)

;;; The Fifteenth Commandment (revised): Use (let ...) to name the values of
;;; repeated expressions in a function definition if they may be evaluated twice
;;; for one and the same use of the function.

;; New version of leftmost using let/cc.
(define leftmost%
  (λ (l)
    (let/cc skip
      (letrec ([lm (λ (l)
                     (cond
                       [(null? l) '()]
                       [(atom? (car l)) (skip (car l))]
                       [else
                        (let ()
                          (lm (car l))
                          (lm (cdr l)))]))])
        (lm l)))))

(check-equal? (leftmost% '((() (c (a))) b)) 'c)
(check-equal? (leftmost% '()) '())
(check-equal? (leftmost% '(a (b) ((c)))) 'a)

(define rm
  (λ (a l oh)
    (cond
      [(null? l) (oh 'no)]
      [(atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh)))]
      [else
       (let ([new-car (let/cc oh (rm a (car l) oh))])
         (if (atom? new-car)
             (cons (car l) (rm a (cdr l) oh))
             (cons new-car (cdr l))))])))

;; New, shorter, version of rember1*. Unfortunately there is no 'try' keyword in
;; Racket. We could us a macro, but we don't know about those yet.
(define rember1*%
  (λ (a l)
    (let ([new-l (let/cc oh (rm a l oh))])
      (if (atom? new-l)
          l
          new-l))))

(check-equal? (rember1*% 'salad '((Swedish rye)
                                  (French (mustard salad turkey))
                                  salad))
              '((Swedish rye)
                (French (mustard turkey))
                salad))
(check-equal? (rember1*% 'noodles '((food) more (food)))
              '((food) more (food)))
