;2 kontrolno 2 zad
(define (filterLen lst1 lst2)
  (length (filter (lambda (x) (= (string-length lst2) (string-length x))) lst1)))


(define (qsort lst f)
  (if (null? lst) '()
      (append (qsort (filter (lambda (x) (< (f x) (f (car lst)))) (cdr lst)) f)
              (cons (car lst)
                    (qsort (filter (lambda (x) (>= (f x) (f (car lst)))) (cdr lst)) f)))))

(define (lfsort lst)
  (qsort lst (lambda (x) (filterLen lst x))))
(lfsort '("mii" "dw" "dwedw" "dsa" "de" "ww" "eqwe"))
