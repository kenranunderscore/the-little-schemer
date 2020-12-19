#lang racket

(require "prelude.rkt")
(require "chapter4.rkt")
(require rackunit)

; Remove all occurrences of an atom from a list of s-expressions and from all
; s-expressions therein.
(define rember*
  (λ (a l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(eq? a (car l))
                  (rember* a (cdr l))]
                 [else (cons (car l) (rember* a (cdr l)))])]
          ; (car l) is now a list as well, so we have to recurse.
          [else (cons (rember* a (car l))
                      (rember* a (cdr l)))])))

(check-equal? (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
              '((coffee) ((tea)) (and (hick))))
(check-equal? (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
              '(((tomato)) ((bean)) (and ((flying)))))

; Insert new to the right of old for every occurence of old in l.
(define insertR*
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? old (car l))
          (cons old (cons new (insertR* new old (cdr l))))]
         [else (cons (car l) (insertR* new old (cdr l)))])]
      [else
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l)))])))

(check-equal? (insertR* 'roast 'chuck '((how much (wood))
                                        could
                                        ((a (wood) chuck))
                                        (((chuck)))
                                        (if (a) ((wood chuck)))
                                        could chuck wood))
              '((how much (wood))
                could
                ((a (wood) chuck roast))
                (((chuck roast)))
                (if (a) ((wood chuck roast)))
                could chuck roast wood))

;; The First Commandment (final version)
;;
;; When recurring on a list of atoms, lat, ask two questions about it: (null?
;; lat) and else.
;;
;; When recurring on a number, n, ask two questions about it: (zero? n) and
;; else.
;;
;; When recurring on a list of S-expressions, l, ask three questions about it:
;; (null? l), (atom? (car l)), and else.

;; The Fourth Commandment (final version)
;;
;; Always change at least one argument while recurring. When recurring on a list
;; of atims, lat, use (cdr lat). When recurring on a number, n, use (sub1 n).
;; And when recurring on a list of S-expressions, l, use (car l) and (cdr l) if
;; neither (null? l) nor (atom? (car l)) are true.
;;
;; It must be changed to be closer to termination. The changing argument must be
;; tested in the termination condition:

; Cound how many times an atom occurs in a list and in all S-expressions
; therein.
(define occur*
  (λ (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (cond
         [(eq? a (car l))
          (add1 (occur* a (cdr l)))]
         [else
          (occur* a (cdr l))])]
      [else
       (+ (occur* a (car l))
            (occur* a (cdr l)))])))

(check-equal? (occur* 'banana '((banana)
                                (split ((((banana ice))) (cream (banana)) sherbet))
                                (banana)
                                (bread)
                                (banana brandy)))
              5)

; Replace all occurrences of an atom a in a list and in all S-expressions
; therein.
(define subst*
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? old (car l))
          (cons new (subst* new old (cdr l)))]
         [else
          (cons (car l) (subst* new old (cdr l)))])]
      [else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))])))

(check-equal?
 (subst*
  'orange
  'banana
  '((banana)
    (split ((((banana ice))) (cream (banana)) sherbet))
    (banana)
    (bread)
    (banana brandy)))
 '((orange)
   (split ((((orange ice))) (cream (orange)) sherbet))
   (orange)
   (bread)
   (orange brandy)))

; Insert new to the left of old for every occurence of old in l and in any
; contained S-expressions.
(define insertL*
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? old (car l))
          (cons new (cons old (insertL* new old (cdr l))))]
         [else (cons (car l) (insertL* new old (cdr l)))])]
      [else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))])))

(check-equal?
 (insertL*
  'roast
  'chuck
  '((how much (wood))
    could
    ((a (wood) chuck))
    (((chuck)))
    (if (a) ((wood chuck)))
    could chuck wood))
 '((how much (wood))
   could
   ((a (wood) roast chuck))
   (((roast chuck)))
   (if (a) ((wood roast chuck)))
   could roast chuck wood))

; Check whether a is contained in the list l or any S-expression therein.
(define member*
  (λ (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l)))]
      [else (or (member* a (car l))
                (member* a (cdr l)))])))

(check-true (member* 'chips '((potato) (chips ((with) fish) (chips)))))

; Find the leftmost atom in a non-empty list of S-expressions (with none of the
; S-expressions being the empty list).
(define leftmost
  (λ (l)
    (cond
      [(atom? (car l)) (car l)]
      [else (leftmost (car l))])))

(check-equal? (leftmost '((potato) (chips ((with) fish) (chips))))
              'potato)

; Check whether two lists are equal.
(define eqlist?
  (λ (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [(and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))]
      [(or (atom? (car l1))
           (atom? (car l2))) #f]
      [else (and (eqlist? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))])))

(check-false (eqlist? '(strawberry ice cream) '(strawberry cream ice)))
(check-false (eqlist? '(banana (split)) '((banana) (split))))
(check-true (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))))

; Check whether two S-expressions are equal.
(define my-equal?
  (λ (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2))
       (eqan? s1 s2)]
      [(or (atom? s1) (atom? s2)) #f]
      [else (eqlist? s1 s2)])))

; Rewritten eqlist? by using my-equal?.
(define eqlist?-revised
  (λ (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [else (and (my-equal? (car l1) (car l2))
                 (eqlist?-revised (cdr l1) (cdr l2)))])))

(check-false (eqlist?-revised '(strawberry ice cream) '(strawberry cream ice)))
(check-false (eqlist?-revised '(banana (split)) '((banana) (split))))
(check-true (eqlist?-revised '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))))

;; The Sixth Commandment: Simplify only after the function is correct.

; Rember revised to work with any S-expression instead of just atoms.
(define rember-revised
  (λ (s l)
    (cond
      [(null? l) '()]
      [(my-equal? (car l) s)
       (cdr l)]
      [else (cons (car l)
                  (rember-revised s (cdr l)))])))
