;5 zad
(define (insertOne elem lst pred?)
  (define (helper result lst)
    (cond ((null? lst) (append result (list elem)))
          ((pred? elem (car lst)) (append result (cons elem lst)))
          (else (helper (append result (list (car lst))) (cdr lst)))))
  (helper '() lst))

(define (merge-sorted l1 l2 pred?)
  (define (helper lst mas)
    (cond ((null? lst) mas)
          ((null? (cdr lst))(insertOne (car lst) mas pred?))
          (else (helper (cdr lst) (append (insertOne (car lst) mas pred?))))))
  (helper l1 l2))

(merge-sorted '(1 3 5 7) '(2 4 6) <)