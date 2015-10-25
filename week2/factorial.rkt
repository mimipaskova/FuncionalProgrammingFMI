(define (fact n)
  (if (< n 2) 1
      (* n (fact(- n 1))))
)


(define (factt n)
  (define (helper i result)
    (if (> i n) result
        (helper (+ i 1) (* result i)))
    )
   (if (< n 2) 1
       (helper 1 1))
)