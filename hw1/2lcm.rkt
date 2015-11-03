(define (gcd! a b)
  (cond ((> a b) (gcd! (- a b) b))
        ((= a b) a)
        (else (gcd! a (- b a)))))


(define (lcm! a b)
  (/ (* a b) (gcd! a b)))
