#lang racket

(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))

(atom? '()) ; #f

(atom? 'atom) ; #t

(atom? 'turkey) ; #t

(atom? 1492) ; #t

(atom? 'u) ; #t

(atom? '*abc$) ; #t

(list? '(atom)) ; #t

(list? '(atom turkey or)) ; #t

(list? '((atom turkey) or)) ; #t

(list? '()) ; #t

(atom? '()) ; #f

(list? '(() () () ())) ; #t

(car '(a b c)) ; 'a

(car '((a b c) x y z)) ; '(a b c)

(car 'hotdog) ; error: atom is not a list

(car '()) ; error: cannot ask car of empty list

;; The Law of Car: The primitive car is defined only for non-empty lists.

(car '(((hotdogs)) (and) (pickle) relish)) ; '((hotdogs))

(car '(((hotdogs)) (and) (pickle) relish)) ; '((hotdogs))

(car (car '(((hotdogs)) (and) (pickle) relish))) ; '(hotdogs)

(cdr '(a b c)) ; '(b c)

(cdr '((a b c) x y z)) ; '(x y z)

(cdr '(hamburger)) ; '()

(cdr '((x) t r)) ; '(t r)

(cdr 'hotdogs) ; error: can only ask cdr of lists

(cdr '()) ; error: cannot ask cdr of empty list

;; The Law of Cdr: The primitive cdr is only defined for non-empty lists. The
;; cdr of any non-empty list is always another list.

(car (cdr '((b) (x y) ((c))))) ; '(x y)

(cdr (cdr '((b) (x y) ((c))))) ; '(((c)))

(cdr (car '(a (b (c)) d))) ; error: evaluates to (cdr 'a), and 'a is an atom

(cons 'peanut '(butter and jelly)) ; '(peanut butter and jelly)

(cons '(banana and) '(peanut butter and jelly)) ; '((banana and) peanut butter and jelly)

(cons '((help) this) '(is very ((hard) to learn))) ; '(((help) this) is very ((hard) to learn))

(cons '(a b (c)) '()) ; '((a b (c)))

(cons 'a '()) ; '(a)

(cons 'a 'b) ; error: 'b is not a list

;; The Law of Cons: The primitive cons takes two arguments. The second argument
;; to cons must be a list. The result is a list.

(cons 'a (car '((b) c d))) ; '(a b),
; (car '((b) c d)) is '(b), which is the list we cons 'a to

(cons 'a (cdr '((b) c d))) ; '(a c d)
; (cdr '((b) c d)) is the list '(c d), then we prepend 'a

(null? '()) ; #t

(null? (quote ())) ; #t, same as above

(null? '(a b c)) ; #f, as the list is not empty

(null? 'spaghetti) ; error: 'spaghetti is not a list

;; The Law of Null: The primitive null is defined only for lists.

(atom? 'harry) ; #t

(atom? '(Harry had a heap of apples)) ; #f

; atom? may take any s-exp as argument

(atom? (car '(Harry had a heap of apples))) ; #t, since it evaluates to (atom? 'Harry)

(atom? (cdr '(Harry had a heap of apples))) ; #f, the cdr is a list

(atom? (cdr '(Harry))) ; #f

(atom? (car (cdr '(swing low sweet cherry oat)))) ; #t => (atom? 'low)

(atom? (car (cdr '(swing (low sweet) cherry oat)))) ; #f, => (atom? '(low sweet))

(eq? 'Harry 'Harry) ; #t

(eq? 'margarine 'butter) ; #f

(eq? '() '(strawberry)) ; error: these are not non-numeric atoms but lists

(eq? 6 7) ; error: these are numeric atoms

;; The  Law of  Eq?: The  primitive  eq? takes  two  arguments. Each  must be  a
;; non-numeric atom.

(eq? (car '(Mary had a little lamb chop) 'Mary)) ; #t

(eq? (cdr '(soured milk)) 'milk) ; #f, the cdr is a list

(eq? (car '(beans beans we need jelly beans))
     (car (cdr '(beans beans we need jelly beans)))) ; #t
