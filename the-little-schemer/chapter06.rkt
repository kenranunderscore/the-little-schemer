#lang racket

(require "prelude.rkt")
(require "chapter04.rkt")
(require rackunit)

; Check whether an arithmetic expression contains only numbers and arithmetic
; operators.
(define numbered?
  (λ (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))])))

(check-true (numbered? 1))
(check-true (numbered? '(3 + (4 ^ 5))))
(check-false (numbered? '(4 * sausage)))

;; The Seventh Commandment: Recur on the subparts that are of the same nature:
;; - On the sublists of lists.
;; - On the subexpressions of an arithmetic expression.

; Get the first sub-expression of a numbered arithmetic expression.
(provide 1st-sub-exp)
(define 1st-sub-exp
  (λ (nexp)
    (car nexp)))

; Get the second sub-expression of a numbered arithmetic expression.
(provide 2nd-sub-exp)
(define 2nd-sub-exp
  (λ (nexp)
    (car (cdr (cdr nexp)))))

; Return the operator of a numbered arithmetic expression.
(provide operator)
(define operator
  (λ (nexp)
    (car (cdr nexp))))

; Return the value of a numbered arithmetic expression.
(provide value)
(define value
  (λ (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (operator nexp) '+)
       (+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) '*)
       (* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) '^)
       (pow (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))])))

(check-equal? (value 13)
              13)
(check-equal? (value '(1 + 3))
              4)
(check-equal? (value '(1 + (3 ^ 4)))
              82)

;; The Eighth Commandment: Use help functions to abstract from representations.
