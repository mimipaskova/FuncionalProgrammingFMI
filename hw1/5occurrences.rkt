(define (occurrences a n)
  (define (helper sum number)
    (cond ((= 0 (quotient number 10)) (if (= a (remainder number 10)) (+ 1 sum) sum))
          ((= a (remainder number 10)) (helper (+ 1 sum) (quotient number 10)))
          (else (helper sum (quotient number 10)))))
    
    (if (< n 10) (if (= a n) 1 0)
        (helper 0 n)))

