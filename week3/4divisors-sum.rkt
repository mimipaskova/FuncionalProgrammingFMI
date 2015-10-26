(define (divisors-sum n )
  (define (helper sum i)
    (if (> i n) sum
        (if (= (remainder n i) 0) (helper (+ sum i) (+ i 1))
        (helper sum (+ i 1))))
    )
  (if (< n 2) n
      (helper 0 1))
    
  )