#lang racket

(require "../the-little-schemer/prelude.rkt")
(require rackunit)

(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

;; We know: (kar (kons a b)) has to be equal to a. (kons a b) is now a
;; higher-order function that takes a selector function, so we can plug in a
;; function that selects the first of its arguments. Similarly for kdr, we
;; select the second argument.

(define kar
  (lambda (k)
    (k
     (lambda (x y) x))))

(define kdr
  (lambda (k)
    (k
     (lambda (x y) y))))

(check-equal? (kar (kons 'a 'b)) 'a)
(check-equal? (kdr (kons 'a 'b)) 'b)

(define lots
  (lambda (m)
    (cond
      [(zero? m) '()]
      [else (kons 'egg
                  (lots (sub1 m)))])))

(define lenkth
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (add1 (lenkth (kdr l)))])))

(define bons
  (lambda (kar)
    (let ([kdr '()])
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define bar
  (lambda (b)
    (b (lambda (x y z) y))))

(define bdr
  (lambda (b)
    (b (lambda (x y z) z))))

(define set-bdr
  (lambda (b x)
    ((b (lambda (x y z) x)) x)))

(define kons%
  (lambda (a d)
    (let ([c (bons a)])
      (set-bdr c d)
      c)))
