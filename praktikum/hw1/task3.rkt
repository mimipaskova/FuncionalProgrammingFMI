;3 zad
(define (remove-element elem lst)
  (filter (lambda (x) (not (equal? elem x))) lst))

(remove-element "hi" '("hello" "hi" "world" "hi"))