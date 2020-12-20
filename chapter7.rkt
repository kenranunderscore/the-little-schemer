#lang racket

(require "prelude.rkt")
(require "chapter2.rkt")
(require "chapter3.rkt")
(require rackunit)

; Check if a lat is a set.
(define set?
  (λ (lat)
    (cond
      [(null? lat) #t]
      [(member? (car lat) (cdr lat)) #f]
      [else (set? (cdr lat))])))

(check-false (set? '(apple peaches apple plum)))
(check-true (set? '(apples peaches pears plums)))
(check-true (set? '()))

; Remove duplicates from a lat and thus make it a set.
; Uses member?.
(define make-set-1
  (λ (lat)
    (cond
      [(null? lat) '()]
      [(member? (car lat) (cdr lat))
       (make-set-1 (cdr lat))]
      [else (cons (car lat)
                  (make-set-1 (cdr lat)))])))

; Remove duplicates from a lat and thus make it a set.
; Uses multirember.
(define make-set
  (λ (lat)
    (cond
      [(null? lat) '()]
      [else (cons (car lat)
                  (multirember (car lat)
                               (make-set (cdr lat))))])))

(check-equal? (make-set-1 '(apple peach pear peach plum apple lemon peach))
              '(pear plum apple lemon peach))
(check-equal? (make-set '(apple peach pear peach plum apple lemon peach))
              '(apple peach pear plum lemon))
(check-equal? (make-set '(apple 3 pear 4 9 apple 3 4))
              '(apple 3 pear 4 9))

; Check whether every element of set1 is in set2 as well.
(define subset?
  (λ (set1 set2)
    (cond
      [(null? set1) #t]
      [else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))])))

(check-false
 (subset? '(4 pounds of horseradish)
          '(four pounds chicken and 5 ounces horseradish)))
(check-true
 (subset? '(5 chicken wings)
          '(5 hamburgers 2 pieces fried chicken and light duckling wings)))

; Check if two sets are equal.
(define eqset?
  (λ (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(check-true (eqset? '(6 large chickens with wings)
                    '(6 chickens with large wings)))

; Check if two sets have common elements.
(define intersect?
  (λ (set1 set2)
    (cond
      [(null? set1) #f]
      [else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))])))

(check-true (intersect? '(stewed tomatoes and macaroni)
                        '(macaroni and cheese)))

; Return the intersection of two sets.
(define intersect
  (λ (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))

(check-equal? (intersect '(stewed tomatoes and macaroni)
                         '(macaroni and cheese))
              '(and macaroni))

; Return the union of two sets.
(define union
  (λ (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2)
       (union (cdr set1) set2)]
      [else (cons (car set1)
                  (union (cdr set1) set2))])))

(check-true
 (eqset? (union '(stewed tomatoes and macaroni casserole)
                '(macaroni and cheese))
         '(stewed tomatoes casserole and macaroni cheese)))

; Return the intersection of all sets in a list.
(define intersectall
  (λ (l-set)
    (cond
      [(null? (cdr l-set))
       (car l-set)]
      [else (intersect (car l-set)
                       (intersectall (cdr l-set)))])))

(check-equal?
 (intersectall
  '((6 pears and)
    (3 peaches and 6 peppers)
    (8 pears and 6 plums)
    (and 6 prunes with some apples)))
 '(6 and))

; Check whether an S-expression is a pair, that is, it is a list containing
; exactly two S-expression.
(define a-pair?
  (λ (l)
    (cond
      [(atom? l) #f]
      [(null? l) #f]
      [(null? (cdr l)) #f]
      [else (null? (cdr (cdr l)))])))

(check-true (a-pair? '(pear pear)))
(check-true (a-pair? '(3 7)))
(check-true (a-pair? '((2) (pair))))
(check-true (a-pair? '(full (house))))

; Get the first item of a pair.
(define first
  (λ (p)
    (car p)))

; Get the second item of a pair.
(define second
  (λ (p)
    (car (cdr p))))

; Build a pair out of two S-expressions.
(define build
  (λ (s1 s2)
    (cons s1 (cons s2 '()))))

;; A rel is a set of pairs.

; Check whether a rel is a fun.
(define fun?
  (λ (rel)
    (set? (firsts rel))))

(check-false (fun? '((4 3) (4 2) (3 4))))
(check-true (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(check-false (fun? '((d 4) (b 0) (b 9) (e 5) (g 4))))

; Reverse a pair.
(define revpair
  (λ (p)
    (build (second p)
           (first p))))

; Reverse a relation. That is, reverse all its pairs.
(define revrel
  (λ (rel)
    (cond
      [(null? rel) '()]
      [else (cons (revpair (car rel))
                  (revrel (cdr rel)))])))

(check-equal?
 (revrel '((8 a) (pumpkin pie) (got sick)))
 '((a 8) (pie pumpkin) (sick got)))

; Get all second elements of a rel.
(define seconds
  (λ (rel)
    (cond
      [(null? rel) '()]
      [else (cons (second (car rel))
                  (seconds (cdr rel)))])))

; Check whether a fun is a fullfun.
(define fullfun?
  (λ (fun)
    (set? (seconds fun))))

(check-false
 (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(check-true
 (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))))
(check-false
 (fullfun? '((grape raisin) (plum prune) (stewed prune))))
(check-true
 (fullfun? '((grape raisin) (plum prune) (stewed grape))))
