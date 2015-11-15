(define (++ a) (+ a 1))


(define (filter-iter pred? op null-value term a next b)
  (define (helper current result)
    (cond ((> current b) result)
          ((pred? current) (helper (next current) (op (term current) result)))
          (else (helper (next current) result))))
  (helper a null-value))

(define (accumulate op null-value term a next b)
  (if (> a b) null-value
      (op (term a) (accumulate op null-value term (next a) next b))))

;1 zad
(define (squares-product a b)
  (define (isSqr x) (integer? (sqrt x)))
  (define (term k) k)
  (filter-iter isSqr * 1 term a ++ b))

(squares-product 1 10)
  
;2 zad
(define (sum-row x n)
  (define (term k) (expt x (expt 2 k)))
  (accumulate + 0 term 0 ++ (- n 1)))

(sum-row 1/10 4)
(sum-row 2 6)
(sum-row 2 5)

;3 zad
(define (const? f a b)
  (define (term x) (= (f x) (f (+ x 1))))
  (accumulate (lambda (p q) (and q p)) #t term a ++ b))

(const? (lambda (x) 5) 1 100)

;bonus
(define (max-fun f a b)
  (define (term x) (f x))
  (accumulate max -inf.0 term a ++ b))