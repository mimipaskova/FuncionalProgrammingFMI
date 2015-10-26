(define (reverse-int n)
  (define (helper i result x)
    (if (= 0 x) result
        (helper (+ i 1) (+ (* result 10) (remainder x 10) ) (quotient x 10) ))
   )
  (if (< n 10) n
      (helper 0 0 n))

  )


(define (palindrome? n)
  (define first (reverse-int n))
   (= first n)
  )