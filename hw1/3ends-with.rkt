(define (ends-with? a b)
  (cond ((= 0 (quotient b 10))  (= (remainder a 10) b))
        ((= (remainder a 10) (remainder b 10)) (ends-with? (quotient a 10) (quotient b 10) ))
        (else #f)))