#lang racket

(require "../the-little-schemer/prelude.rkt")
(require "../the-little-schemer/chapter09.rkt")
(require "chapter12.rkt")
(require rackunit)

(define sweet-tooth
  (λ (food)
    (cons food '(cake))))

(define last 'angelfood)

(check-equal? (sweet-tooth 'chocolate) '(chocolate cake))
(check-equal? last 'angelfood)

(define sweet-toothL
  (λ (food)
    (set! last food)
    (cons food '(cake))))

(check-equal? (sweet-toothL 'chocolate) '(chocolate cake))
(check-equal? last 'chocolate)

;; Now we'd like to save all ingredients that went into sweet-toothR (in order).
(define ingredients '())

;; Like sweet-toothL, but remembers all ingredients that have been used so far.
(define sweet-toothR
  (λ (food)
    (set! ingredients (cons food ingredients))
    (cons food '(cake))))

(check-equal? (sweet-toothR 'chocolate) '(chocolate cake))
(check-equal? (sweet-toothR 'fruit) '(fruit cake))
(check-equal? (sweet-toothR 'cheese) '(cheese cake))
(check-equal? (sweet-toothR 'carrot) '(carrot cake))
(check-equal? ingredients '(carrot cheese fruit chocolate))

;; Embed 'pizza in a list of depth n.
(define deep
  (λ (n)
    (cond
      [(zero? n) 'pizza]
      [else (cons (deepM (sub1 n)) '())])))

;; We cannot run these tests at this point, since we now use deepM, which is
;; defined below.
;; (check-equal? (deep 3) '(((pizza))))
;; (check-equal? (deep 7) '(((((((pizza))))))))
;; (check-equal? (deep 0) 'pizza)

(define Ns '())
(define Rs '())

(define deepR
  (λ (n)
    (let ([result (deep n)])
      (set! Ns (cons n Ns))
      (set! Rs (cons result Rs))
      (deep n))))

;;; The Nineteenth Commandment: Use (set! ...) to remember valuable things
;;; between two distinct uses of a function.

;; Get the result of deep corresponding to the number n in ns (which is
;; guaranteed to be contained in there).
(define find
  (λ (n ns rs)
    (letrec ([f (λ (ns rs)
                  (cond
                    [(null? ns) #f]
                    [(= (car ns) n) (car rs)]
                    [else (f (cdr ns) (cdr rs))]))])
      (f ns rs))))

;; A variant of deep that uses memoization to store and receive already
;; calculated results.
(define deepM
  (let ([Rs '()]
        [Ns '()])
    (λ (n)
      (let ([exists (find n Ns Rs)])
        (if (atom? exists)
            (let ([result (deep n)])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(check-equal? (deep 3) '(((pizza))))
(check-equal? (deep 7) '(((((((pizza))))))))
(check-equal? (deep 0) 'pizza)
(check-equal? (deepM 3) '(((pizza))))
(check-equal? (deepM 7) '(((((((pizza))))))))
(check-equal? (deepM 0) 'pizza)

;;; The Seventeenth Commandment (final version): Use (set! ...) for (let ((x
;;; ...)) ...) only if there is at least one (lambda ... between it and the (let
;;; ...), or if the new value for x is a function that refers to x.

(define L
  (λ (length)
    (λ (l)
      (cond
        [(null? l) 0]
        [else (add1 (length (cdr l)))]))))

;; Get the length of a list.
(define length
  (let ([h (λ (l) 0)])
    (set! h (L (λ (arg) (h arg))))
    h))

(check-equal? (length '(1 2 3 4)) 4)
(check-equal? (length '()) 0)

(define Y!
  (λ (L)
    (let ([h (λ (l) '())])
      (set! h (L (λ (arg) (h arg))))
      h)))

;; This is the same as Y!. Together, they showcase how we can write our own
;; letrec if needed using let and set!.
(define Y-bang
  (λ (f)
    (letrec ([h (f (λ (arg) (h arg)))])
      h)))

(define length% (Y! L))

(check-equal? (length% '(a b c)) 3)
(check-equal? (length% '()) 0)

;;; We can use the applicative-order, imperative Y-combinator Y! to write
;;; recursive functions without using define to name them first.

(define D
  (λ (depth*)
    (λ (s)
      (cond
        [(null? s) 1]
        [(atom? (car s)) (depth* (cdr s))]
        [else (max (add1 (depth* (car s)))
                   (depth* (cdr s)))]))))

(define depth* (Y! D))

;; A bizarre function.
(define biz
  (let ([x 0])
    (λ (f)
      (set! x (add1 x))
      (λ (a)
        (if (= a x)
            0
            (f a))))))

(check-equal? ((Y biz) 5) 0)
;; (check-equal? ((Y! biz) 5) 0) ; this does not terminate
