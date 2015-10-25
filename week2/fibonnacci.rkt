(define (fib n)
  (if(< n 2) n
    (+ (fib(- n 1))
       (fib(- n 2)) ))
)

(define (fibb n)
  (define (helper a b i)
    (if (= n i) b
    (helper b (+ b a) (+ i 1)))
  )
  (if(< n 2) n
  (helper 0 1 1))
)