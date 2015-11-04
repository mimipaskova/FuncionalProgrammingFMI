(define (1+ x) (+ x 1))

(define (accumulate op null-value term a next b)
  (if (> a b) null-value
      (op (term a) (accumulate op null-value term (next a) next  b))))

(define (expt-accum x n)
  (define (term k) x)
  (accumulate * 1 term 1 1+ n))
