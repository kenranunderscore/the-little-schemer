#lang racket

(require "prelude.rkt")
(require rackunit)

; Remove the first occurrence of an atom x from a lat.
(define rember
  (λ (x lat)
    (cond
      [(null? lat) '()]
      [else (if (eq? (car lat) x)
                (cdr lat)
                (cons (car lat) (rember x (cdr lat))))])))

(check-equal? (rember 'mint '(lamb chops and mint jelly))
              '(lamb chops and jelly))

(check-equal? (rember 'mint '(lamb chops and mint flavored mint jelly))
              '(lamb chops and flavored mint jelly))

(check-equal? (rember 'toast '(bacon lettuce and tomato))
              '(bacon lettuce and tomato))

(check-equal? (rember 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea cup and hick cup))

;; The Second Commandment: Use cons to build lists.

; Build a list out of the first elements of each list in lats.
(define firsts
  (λ (lats)
    (cond
      [(null? lats) '()]
      [else (cons (car (car lats))
                  (firsts (cdr lats)))])))

(check-equal? (firsts '((apple peach pumpkin)
                        (plum pear cherry)
                        (grape raisin pea)
                        (bean carrot eggplant)))
              '(apple plum grape bean))

(check-equal? (firsts '((a b) (c d) (e f)))
              '(a c e))

(check-equal? (firsts '())
              '())

(check-equal? (firsts '((five plums) (four) (eleven green oranges)))
              '(five four eleven))

(check-equal? (firsts '(((five plums) four)
                        (eleven green oranges)
                        ((no) more)))
              '((five plums) eleven (no)))

;; The Third Commandment: When building a list, describe the first typical
;; element, and then cons it onto the natural recursion.

; Insert new to the right of the first occurrence of old in lat.
(define insert-right
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [else (cond
              [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
              [else (cons (car lat) (insert-right new old (cdr lat)))])])))

(check-equal? (insert-right 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with fudge topping for dessert))

(check-equal? (insert-right 'jalapeno 'and '(tacos tamales and salsa))
              '(tacos tamales and jalapeno salsa))

(check-equal? (insert-right 'e 'd '(a b c d f g d h))
              '(a b c d e f g d h))

; Insert new to the left of the first occurrence of old in lat.
(define insert-left
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [else (cond
              [(eq? (car lat) old) (cons new lat)]
              [else (cons (car lat) (insert-left new old (cdr lat)))])])))

(check-equal? (insert-left 'a 'b '(x b c b d))
              '(x a b c b d))

; Substitute the first occurrence of old in lat with new.
(define subst
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [else (cond
              [(eq? (car lat) old) (cons new (cdr lat))]
              [else (cons (car lat) (subst new old (cdr lat)))])])))

(check-equal? (subst 'x 'b '(a b c b))
              '(a x c b))

; Substitute the first occurrence of either o1 or o2 in lat with new.
(define subst2
  (λ (new o1 o2 lat)
    (cond
      [(null? lat) '()]
      [else (cond
              [(or (eq? (car lat) o1)
                   (eq? (car lat) o2))
               (cons new (cdr lat))]
              [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))])])))

(check-equal? (subst2 'new 'o1 'o2 '(a o1 b))
              '(a new b))

(check-equal? (subst2 'new 'o1 'o2 '(o2 o1 o2))
              '(new o1 o2))

; Remove all occurrences of x in lat.
(define multirember
  (λ (x lat)
    (cond [(null? lat) '()]
          [else (cond
                  [(eq? (car lat) x) (multirember x (cdr lat))]
                  [else (cons (car lat) (multirember x (cdr lat)))])])))

(check-equal? (multirember 5 '(1 5 2 5 5 5 2 7 5 5))
              '(1 2 2 7))

; Insert new to the right of every occurrence of old in lat.
(define multiinsert-right
  (λ (new old lat)
    (cond [(null? lat) '()]
          [else (cond
                  [(eq? (car lat) old)
                   (cons (car lat)
                         (cons new (multiinsert-right new old (cdr lat))))]
                  [else (cons (car lat) (multiinsert-right new old (cdr lat)))])])))

(check-equal? (multiinsert-right 'new 'old '(old a b old old c))
              '(old new a b old new old new c))

;; The Fourth Commandment (preliminary): Always change at least one argument
;; while recurring. It must be changed to be closer to termination. The changing
;; argument must be tested in the termination condition: when using cdr, test
;; termination with null?.

; Replace every occurrence of old in lat with new.
(define multisubst
  (λ (new old lat)
    (cond [(null? lat) '()]
          [else (cond
                  [(eq? (car lat) old)
                   (cons new (multisubst new old (cdr lat)))]
                  [else (cons (car lat) (multisubst new old (cdr lat)))])])))

(check-equal? (multisubst 'new 'old '(a old b c old))
              '(a new b c new))
