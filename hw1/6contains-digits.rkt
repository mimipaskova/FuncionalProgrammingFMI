(define (occurrences a n)
  (define (helper sum number)
    (cond ((= 0 (quotient number 10)) (if (= a (remainder number 10)) (+ 1 sum) sum))
          ((= a (remainder number 10)) (helper (+ 1 sum) (quotient number 10)))
          (else (helper sum (quotient number 10)))))
    
    (if (< n 10) (if (= a n) 1 0)
        (helper 0 n)))


(define (contains-digits? b a)
  (define (helper number)
    (cond ((= 0 (quotient number 10)) (if (< 0 (occurrences number b)) #t #f ))
          ((< 0 (occurrences (remainder number 10) b)) (helper (quotient number 10)))
          (else #f)))
  (if (< a 10) (if (< 0 (occurrences a b)) #t #f )
        (helper a)))
