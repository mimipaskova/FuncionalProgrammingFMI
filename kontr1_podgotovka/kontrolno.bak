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