#lang racket

(require "../the-little-schemer/prelude.rkt")
(require "chapter16.rkt")
(require rackunit)

;; Used to return the value of the N in consC below. The Seasoned Schemer uses
;; (define counter), but Racket doesn't like that.
(define counter #f)
(define set-counter #f)

;; Like cons, but we remember the number of arguments it has seen.
(define consC
  (let ([N 0])
    (set! counter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define deep
  (lambda (n)
    (cond
      [(zero? n) 'pizza]
      [else (consC (deep (sub1 n)) '())])))

(check-equal? (deep 5) '(((((pizza))))))
(check-equal? (counter) 5)
(check-equal? (deep 7) '(((((((pizza))))))))
(check-equal? (counter) 12)

(define supercounter
  (lambda (f)
    (letrec ([S (lambda (n)
                  (if (zero? n)
                      (f n)
                      (let ()
                        (f n)
                        (S (sub1 n)))))])
      (S 1000)
      (counter))))

(check-equal? (supercounter deep) 500512)
(set-counter 0)
(check-pred zero? (counter))
(check-equal? (supercounter deep) 500500)

(define deepM
  (let ([Rs '()]
        [Ns '()])
    (lambda (n)
      (let ([exists (find n Ns Rs)])
        (if (atom? exists)
            (let ([result (if (zero? n)
                              'pizza
                              (consC (deepM (sub1 n)) '()))])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(set-counter 0)
(deepM 5)
(check-equal? (counter) 5)
(deepM 7)
(check-equal? (counter) 7)
(check-equal? (supercounter deepM) 1000)
