#lang racket

(require "prelude.rkt")
(require rackunit)

(check-false (atom? '()))

(check-true (atom? 'atom))

(check-true (atom? 'turkey))

(check-true (atom? 1492))

(check-true (atom? 'u))

(check-true (atom? '*abc$))

(check-pred list? '(atom))

(check-pred list? '(atom turkey or))

(check-pred list? '((atom turkey) or))

(check-pred list? '())

(check-false (atom? '()))

(check-pred list? '(() () () ()))

(check-equal? (car '(a b c))
              'a)

(check-equal? (car '((a b c) x y z))
              '(a b c))

(check-exn exn:fail? (λ () (car 'hotdog))) ; error: an atom is not a list

(check-exn exn:fail? (λ () (car '()))) ; error: cannot ask car of empty list

;; The Law of Car: The primitive car is defined only for non-empty lists.

(check-equal? (car '(((hotdogs)) (and) (pickle) relish))
              '((hotdogs)))

(check-equal? (car (car '(((hotdogs)) (and) (pickle) relish)))
              '(hotdogs))

(check-equal? (cdr '(a b c))
              '(b c))

(check-equal? (cdr '((a b c) x y z))
              '(x y z))

(check-pred null? (cdr '(hamburger)))

(check-equal? (cdr '((x) t r))
              '(t r))

(check-exn exn:fail? (λ () (cdr 'hotdogs))) ; error: can only ask cdr of lists

(check-exn exn:fail? (λ () (cdr '()))) ; error: cannot ask cdr of empty list

;; The Law of Cdr: The primitive cdr is only defined for non-empty lists. The
;; cdr of any non-empty list is always another list.

(check-equal? (car (cdr '((b) (x y) ((c)))))
              '(x y))

(check-equal? (cdr (cdr '((b) (x y) ((c)))))
              '(((c))))

(check-exn exn:fail? (λ () (cdr (car '(a (b (c)) d))))) ; error: evaluates to (cdr 'a), and 'a is an atom

(check-equal? (cons 'peanut '(butter and jelly))
              '(peanut butter and jelly))

(check-equal? (cons '(banana and) '(peanut butter and jelly))
              '((banana and) peanut butter and jelly))

(check-equal? (cons '((help) this) '(is very ((hard) to learn)))
              '(((help) this) is very ((hard) to learn)))

(check-equal? (cons '(a b (c)) '())
              '((a b (c))))

(check-equal? (cons 'a '())
              '(a))

(check-equal? (cons 'a 'b)
              '(a . b))
; 'b is not a list, but this does not raise an error nevertheless but
; rather it creates a dotted pair '(a . b).

;; The Law of Cons: The primitive cons takes two arguments. The second argument
;; to cons must be a list. The result is a list.

(check-equal? (cons 'a (car '((b) c d)))
              '(a b))

(check-equal? (cons 'a (cdr '((b) c d)))
              '(a c d))

(check-pred null? '())

(check-pred null? (quote ()))

(check-false (null? '(a b c)))

(check-false (null? 'spaghetti))

;; The Law of Null: The primitive null is defined only for lists.

(check-pred atom? 'harry)

(check-false (atom? '(Harry had a heap of apples)))

; atom? may take any s-exp as argument

(check-pred atom? (car '(Harry had a heap of apples)))

(check-false (atom? (cdr '(Harry had a heap of apples))))

(check-false (atom? (cdr '(Harry))))

(check-pred atom? (car (cdr '(swing low sweet cherry oat))))

(check-false (atom? (car (cdr '(swing (low sweet) cherry oat)))))

(check-true (eq? 'Harry 'Harry))

(check-false (eq? 'margarine 'butter))

(check-false (eq? '() '(strawberry)))
; These are not non-numeric atoms but lists. Nevertheless Racket evaluates this
; to false. (eq? '(a) '(a)) is #f though, too.

(check-false (eq? 6 7))
; Racket just returns #f and is also able to correctly compare integers with
; eq?.

;; The  Law of  Eq?: The  primitive  eq? takes  two  arguments. Each  must be  a
;; non-numeric atom.

(check-true (eq? (car '(Mary had a little lamb chop)) 'Mary))

(check-false (eq? (cdr '(soured milk)) 'milk))

(check-true
 (eq? (car '(beans beans we need jelly beans))
      (car (cdr '(beans beans we need jelly beans)))))
