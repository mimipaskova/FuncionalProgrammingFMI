(define (ends-with? a b)
  (cond ((= 0 (quotient b 10))  (= (remainder a 10) b))
        ((= (remainder a 10) (remainder b 10)) (ends-with? (quotient a 10) (quotient b 10) ))
        (else #f)))


(define (substr? a b)
  (define (helper number)
    (cond ((= 0 (quotient number 10)) (if (ends-with? number b) #t #f ))
          ((ends-with? number b) #t)
          (else (helper (quotient number 10)))))
  (if (< a 10) (ends-with? a b)
        (helper a)))