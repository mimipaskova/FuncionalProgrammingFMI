(define (atom? x)
  (not (or (pair? x)
           (null? x)
           (vector? x))))

(define (elem? el lst)
  (cond ((null? lst) #f)
        ((equal? el (car lst)) #t)
        (else (elem? el (cdr lst)))))

(elem? 3 '(1 2 3 4 5))
(elem? "oops" '(1 2 3 4 5))

(define (my-reverse lst)
  (define (helper lst res)
    (if (null? lst) res
        (helper (cdr lst) (cons (car lst) res))))
  (helper lst '()))

(my-reverse '(1 2 3 4 5))

(define (remove-first el lst)
  (cond ((null? lst) #f)
        ((equal? el (car lst)) (cdr lst))
        (else (cons (car lst) (remove-first el (cdr lst))))))

(remove-first 2 '(1 2 3 4 1 2 3 4 5))


(define (remove-all el lst)
  (cond ((null? lst) #f)
        ((equal? el (car lst)) (remove-first el (cdr lst)))
        (else (cons (car lst) (remove-all el (cdr lst))))))

(remove-all 2 '(1 2 3 4 1 2 3 4 5))

(define (my-map func lst)
  (if (null? lst) '()
      (cons (func (car lst))
            (my-map func (cdr lst)))))

(my-map (lambda (x) (* x 2)) '(1 2 3))
(my-map even? '(1 2 3 4 5))

(define (my-filter pred? lst)
  (cond ((null? lst) '())
        ((pred? (car lst)) (cons (car lst) (my-filter pred? (cdr lst))))
        (else (my-filter pred? (cdr lst)))))

(my-filter even? '(1 2 3 4 5))

(define (extract-ints lst)
  (my-filter (lambda (x) (integer? x)) lst))

(extract-ints '(1 2 #t "Hello" (3 "no") #f 4 (5)))
      


        