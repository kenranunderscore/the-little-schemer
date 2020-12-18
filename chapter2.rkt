#lang racket

(require "prelude.rkt")

(lat? '(Jack Sprat could eat no chicken fat)) ; #t

(lat? '((Jack) Sprat could eat no chicken fat)) ; #f

(lat? '(Jack (Sprat could) eat no chicken fat)) ; #t

(lat? '()) ; #t

(or (null? '()) (atom? '(d e f g))) ; #t, since the first question is true

(or (null? '(a b c)) (null? '())) ; #t, since the second question is true

(or (null? '(a b c)) (null? '(atom))) ; #f, since neither list is empty

(define member?
  (λ (elem lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) elem)
                (member? elem (cdr lat)))])))

(member? 'poached '(fried eggs and scrambled eggs)) ; #f, since the list does not contain 'poached

(member? 'meat '(mashed potatoes and meat gravy)) ; #t, as 'meat is in the list

;; The First Commandment (preliminary): Always ask null? as the first question
;; in expressing any function.
