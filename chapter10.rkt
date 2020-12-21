#lang racket

(require "prelude.rkt")
(require "chapter7.rkt")
(require rackunit)

; Make a new entry, that is, a pair of two lists where the first list is a set
; and both lists are of the same length.
(define new-entry build)

(define lookup-in-entry-help
  (λ (name names values entry-f)
    (cond
      [(null? names)
       (entry-f name)]
      [(eq? (car names) name)
       (car values)]
      [else
       (lookup-in-entry-help
        name
        (cdr names)
        (cdr values)
        entry-f)])))

; Try to find the value of a given name in an entry.
(define lookup-in-entry
  (λ (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(check-equal? (lookup-in-entry
               'entree
               '((appetizer entree beverage)
                 (food tastes good))
               (λ (name) name))
              'tastes)
(check-equal? (lookup-in-entry
               'dessert
               '((appetizer entree beverage)
                 (food tastes good))
               (λ (name) (list 'not-found name)))
              '(not-found dessert))

;; A table is a list of entries.

; Add an entry to a table.
(define extend-table cons)

; Try to find the value corresponding to some name in a table.
(define lookup-in-table
  (λ (name table table-f)
    (cond
      [(null? table)
       (table-f name)]
      [else
       (lookup-in-entry
        name
        (car table)
        (λ (name)
          (lookup-in-table
           name
           (cdr table)
           table-f)))])))

(check-equal? (lookup-in-table
               'entree
               '(((entree dessert)
                  (spaghetti spumoni))
                 ((appetizer entree beverage)
                  (food tastes great)))
               (λ (name) name))
              'spaghetti)

; Action that handles constants.
(define *const
  (λ (e table)
    (cond
      [(number? e) e]
      [(eq? e #f) #f]
      [(eq? e #t) #t]
      [else (build 'primitive e)])))

; Return the text that is being quoted.
(define text-of second)

; Action that handles quotings.
(define *quote
  (λ (e table)
    (text-of e)))

; The initial (empty) table (that should never be used).
(define initial-table
  (λ (name)
    (car '())))

; Action that handles lambda expressions.
(define *lambda
  (λ (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define question-of first)
(define answer-of second)

(define else?
  (λ (e)
    (cond
      [(atom? e) (eq? e 'else)]
      [else #f])))

; Evaluate the lines of a conditional expression.
(define evcon
  (λ (lines table)
    (cond
      [(else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table)]
      [(meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table)]
      [else (evcon (cdr lines) table)])))

(define cond-lines-of cdr)

; Build a list of the meanings of the S-expressions in a list.
(define evlis
  (λ (args table)
    (cond
      [(null? args) '()]
      [else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))])))

; Action that handles function application.
(define *application
  (λ (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)
(define primitive?
  (λ (l)
    (eq? (car l) 'primitive)))
(define non-primitive?
  (λ (l)
    (eq? (car l) 'non-primitive)))

; Apply a function (given via its representation) to a list of values.
(define apply
  (λ (fun vals)
    (cond
      [(primitive? fun)
       (apply-primitive (second fun) vals)]
      [(non-primitive? fun)
       (apply-closure (second fun) vals)])))

; Apply one of the primitive functions to a list of values.
(define apply-primitive
  (λ (name vals)
    (cond
      [(eq? name 'car) (car (first vals))]
      [(eq? name 'cdr) (cdr (first vals))]
      [(eq? name 'cons) (cons (first vals) (second vals))]
      [(eq? name 'null?) (null? (first vals))]
      [(eq? name 'eq?) (eq? (first vals) (second vals))]
      [(eq? name 'atom?) (atom? (first vals))]
      [(eq? name 'zero?) (zero? (first vals))]
      [(eq? name 'add1) (add1 (first vals))]
      [(eq? name 'sub1) (sub1 (first vals))]
      [(eq? name 'number?) (number? (first vals))])))

(define apply-closure
  (λ (name vals)
    (meaning
     (body-of name)
     (extend-table
      (new-entry
       (formals-of name)
       vals)
      (table-of name)))))

; Action that handles conditional expressions.
(define *cond
  (λ (e table)
    (evcon (cond-lines-of e) table)))

; Action that handles identifiers.
(define *identifier
  (λ (e table)
    (lookup-in-table e table initial-table)))

; Produce the correct action for a given atom.
(define atom->action
  (λ (a)
    (cond
      [(or
        (number? a)
        (eq? a #f)
        (eq? a #t)
        (eq? a 'cons)
        (eq? a 'car)
        (eq? a 'cdr)
        (eq? a 'null?)
        (eq? a 'zero?)
        (eq? a 'atom?)
        (eq? a 'number?)
        (eq? a 'eq?)
        (eq? a 'add1)
        (eq? a 'sub1))
       *const]
      [else *identifier])))

; Produce the correct action for a given list.
(define list->action
  (λ (l)
    (cond
      [(atom? (car l))
       (cond
         [(eq? (car l) 'lambda) *lambda]
         [(eq? (car l) 'cond) *cond]
         [(eq? (car l) 'quote) *quote]
         [else *application])]
      [else *application])))

; Produce the correct action for a given S-expression.
(define expr->action
  (λ (e)
    (cond
      [(atom? e) (atom->action e)]
      [else (list->action e)])))

(define meaning
  (λ (e table)
    ((expr->action e) e table)))

(define value
  (λ (e)
    (meaning e '())))

(check-equal?
 (cons 'car
       (cons (cons 'quote
                   (cons (cons 'a
                               (cons 'b
                                     (cons 'c
                                           '())))
                         '()))
             '()))
 '(car (quote (a b c))))
(check-equal? (value '(car (quote (a b c)))) 'a)
(check-equal? (value '(quote (car (quote (a b c)))))
              '(car (quote (a b c))))
(check-equal? (value '(add1 6)) 7)
(check-equal? (value 6) 6)
(check-equal? (value '(quote nothing)) 'nothing)
(check-equal?
 (value '((lambda (nothing)
            (cons nothing (quote ())))
          (quote (from nothing comes something))))
 '((from nothing comes something)))
(check-equal?
 (value '((lambda (nothing)
            (cond
              (nothing (quote something))
              (else (quote nothing))))
          #t))
 'something)
(check-equal? (value #f) #f)
(check-equal? (value 'car) '(primitive car))
