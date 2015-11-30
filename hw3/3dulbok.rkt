(define (atom? x) (not (pair? x)))


(define (deep-fold nv term op l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else (op (deep-fold nv term op (car l))
                  (deep-fold nv term op (cdr l))))))

;wrong
(define (insert-after-deep x lst)
  (cond ((null? lst) '())
        ((atom? (car lst)) (if (even? (car lst)) (append (insert-after-deep x (car lst)) (list x))))
        (else (insert-after-deep x (car lst)) (insert-after-deep x (cdr lst)))))
        

(insert-after-deep 10 '(1 ((2) 3 4) (((5))) 6 7))


;right
(define (insert-after-deep x lst)
  (cond ((null? lst) '())
        ((atom? (car lst)) (if (even? (car lst)) (append (list (car lst) x) (insert-after-deep x (cdr lst))) 
                                                 (append (list (car lst)) (insert-after-deep x (cdr lst))) ))
        (else (cons (insert-after-deep x (car lst)) (insert-after-deep x (cdr lst))))))
        

(insert-after-deep 10 '(1 ((2) 3 4) (((5))) 6 7))
