(define (atom? x)
  (not (or (pair? x)
           (null? x)
           (vector? x))))

;1 zad
(define (mash f g lst)
  (if (null? lst) '()
      (cons (f (car lst))
            (mash g f (cdr lst)))))

(define (prime? n)
  (define (helper i)
    (if (> i (sqrt n)) #t
        (if  (= (remainder n i) 0) #f
             (helper (+ 1 i))))    
    )
  (if (= n 1) #f
      (helper 2)))

(define (f1 x) (* x 10))
(define f2 prime?)
(mash f1 f2 '(1 2 3 4 5))

;2 zad
(define (insert-after-evens x lst)
  (cond ((null? lst) '())
        ((even? (car lst)) (cons (car lst) (cons x (insert-after-evens x (cdr lst)))))
        (else (cons (car lst) (insert-after-evens x (cdr lst))))))
(insert-after-evens 10 '(1 2 3 4 5))

;3zad
(define (insert-after-deep x lst)
  (cond ((null? lst) '())
        ((atom? (car lst)) (if (even? (car lst)) (append (list (car lst) x) (insert-after-deep x (cdr lst))) 
                                                 (append (list (car lst)) (insert-after-deep x (cdr lst))) ))
        (else (cons (insert-after-deep x (car lst)) (insert-after-deep x (cdr lst))))))
        

(insert-after-deep 10 '(1 ((2) 3 4) (((5))) 6 7))

;4 zad
(define (begins-with? lst1 lst2)
  (cond ((null? lst1) #t)
        ((null? lst2) #f)
        ((equal? (car lst1) (car lst2)) (begins-with? (cdr lst1) (cdr lst2)))
        (else #f)))

;(begins-with? '(2 3 4) '(2 3 4 5))
;(begins-with? '(2 3 4) '(2 3))

(define (sublist? lst1 lst2)
  (define (helper lst3)
    (cond ((null? lst1) #t)
          ((null? lst3) #f)
          ((begins-with? lst1 lst3) #t)
          (else (helper (cdr lst3)))))
  (helper lst2))

(sublist? '(2 3 4) '(1 2 3 4 5))
(sublist? '(2 3 5) '(1 2 3 4 5))
(sublist? '(4 3 5) '(1 2 3 4 5))

;5 zad
(define (elem? el lst)
  (cond ((null? lst) #f)
        ((equal? el (car lst)) #t)
        (else (elem? el (cdr lst)))))

(define (elem? elem lst)
  (if (> (counts elem lst) 0) #t #f))

(define (make-set lst)
  (define (helper lst res)
    (cond ((null? lst) res)
          ((elem? (car lst) res) (helper (cdr lst) res))
          (else (helper (cdr lst) (cons (car lst) res)))))
  (helper lst '()))
(make-set '(1 2 2 4 3 2 1 3 4))
        
        
;bonus

(define (str l elem)
  (map (lambda (x) (cons x elem)) l))

;(str '(1 2 3) 4)

(define (descartes l1 l2)
  (define (helper lst mas)
    (cond ((null? lst) mas)
          ((null? (cdr lst)) (append mas (str l1 (car lst))))
          (else (helper (cdr lst) (append (str l1 (car lst)))))))
  (helper l2 '()))

(descartes '(1 2 3) '(a b))
