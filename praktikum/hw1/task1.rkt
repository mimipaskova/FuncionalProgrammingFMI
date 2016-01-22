;1 zad
(define (sum-negative-list lst)
  (apply + (filter (lambda (x) (> 0 x)) lst)))

(sum-negative-list '(1 2 -7 3 4 -2 6 7))
(sum-negative-list '(1 2 -3 -4 5))
