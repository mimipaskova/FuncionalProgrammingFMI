;Tema 1

(define (sum_direves x k)
  (define (helper sum number)
    (cond ((= 0 (quotient number 10)) (if (= 0 (remainder (+ sum (remainder number 10)) k)) x +inf.0))
          (else (helper (+ sum (remainder number 10)) (quotient number 10)))))
  (if (< x 10) (if (= 0 (remainder x k)) x +inf.0)
      (helper 0 x)))

;(sum_direves 8 4)
;(sum_direves 83 11)
;(sum_direves 123456789 11)
;(sum_direves 123456789 9)
;(sum_direves 123456789 5)
;(sum_direves 123456789 7)

;1 zad
(define (min-sum-digitt a b k)
  (define (min-sum-digit-iter i)
    (if (= i (- b 1)) (sum_direves b k)
        (min (sum_direves (+ i 1) k) (min-sum-digit-iter (+ i 1)))))
  (if (= a b) #f
      (min-sum-digit-iter (- a 1))))

(min-sum-digitt 15 100 3)
(min-sum-digitt 1 100 3)
(min-sum-digitt 4 100 3)


(define (count-find x lst)
  (define (helper times lst)
    (cond ((null? lst) (list times))
          ((= x (car lst)) (helper (+ 1 times) (cdr lst)) )
          (else (helper times (cdr lst)))))
  (helper 0 lst))

(count-find 1 '(1 2 3 4 1 5 1))
(count-find 2 '(1 2 3 4 1 5 1))
(count-find 5 '(1 2 5 3 4 1 5 1))

;3 zad
(define (occurrences l1 l2)
  (define (helper l1 l2 res)
    (cond ((null? l1) res)
          (else  (append res (helper (cdr l1) l2 (count-find (car l1) l2))))))
  (helper l1 l2 '()))

 (occurrences '(1 2 3) '( 1 2 4 1 ))



;Tema 2

(define (divisors-sum n )
  (define (helper sum i)
    (if (> i n) sum
        (if (= (remainder n i) 0) (helper (+ sum i) (+ i 1))
        (helper sum (+ i 1))))
    )
  (if (< n 2) n
      (helper 0 1))
    
  )

(divisors-sum 10)
(divisors-sum 28)
(divisors-sum 13)
(divisors-sum 4)


(define (prod-sum-div a b k)
  (define (helper i lst)
    (cond ((= i b) (if (= (divisors-sum b) k) (list b) '()))
          ((= i (divisors-sum k)) (helper (+ i 1) (cons i lst)))
          (else (helper (+ i 1) lst))))
  (helper a '()))

(prod-sum-div 1 100 7)
    