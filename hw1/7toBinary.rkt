(define (toBinary n)
  (define (helper number binnumber i)
    (cond ((= number 1)  (+ (expt 10 i) binnumber))
          ((= 0 (remainder number 2)) (helper (quotient number 2) binnumber (+ 1 i)))
          (else (helper (quotient number 2) (+ (expt 10 i) binnumber) (+ i 1)))))
  
   (if (= n 1) 1 (helper n 0 0)))
