#lang racket

(require "../the-little-schemer/prelude.rkt")
(require rackunit)

(define (deep n)
  (cond
    [(zero? n) 'pizza]
    [else (cons (deep (sub1 n)) '())]))

(define toppings 'dummy)

(define (deepB m)
  (cond
    [(zero? m)
     (let/cc jump
       (set! toppings jump)
       'pizza)]
    [else
     (cons (deepB (sub1 m))
           '())]))

(deepB 6)
(toppings 'pizza)
(cons (toppings 'cake) '())

;;; The Twentieth Commandment: When thinking about a value created with (let/cc
;;; ...), write down the function that is equivalent but does not forget. Then,
;;; when you use it, remember to forget.

(define (deep&co m k)
  (cond
    [(zero? m) (k 'pizza)]
    [else
     (deep&co (sub1 m)
              (lambda (x)
                (k (cons x '()))))]))

(check-equal? (deep&co 0 (lambda (x) x)) 'pizza)
(check-equal? (deep&co 6 (lambda (x) x)) '((((((pizza)))))))

;; The continuations "building up" look exactly like the n-layers functions in
;; the book. Here's two-layers for instance:

(define (two-layers p)
  (cons
   (cons p '())
   '()))

(define (deep&coB m k)
  (cond
    [(zero? m)
     (let ()
       (set! toppings k)
       (k 'pizza))]
    [else
     (deep&coB (sub1 m)
               (lambda (x)
                 (k (cons x '()))))]))
