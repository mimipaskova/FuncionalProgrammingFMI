(define (atom? x)
  (not (or (pair? x)
           (null? x)
           (vector? x))))


(define (count-atoms l)
  (cond ((null? l) 0)
        ((atom? (car l)) (+ 1 (count-atoms (cdr l))))
        (else (+ (count-atoms (car l)) (count-atoms (cdr l))))))

(count-atoms '(1 3 54 (3 1 5 1) (3 1 4)))
(count-atoms '(1 3 (3 1 1) (3 1 4)))

(define (sum-atoms l)
  (cond ((null? l) 0)
        ((atom? (car l)) (+ (car l) (sum-atoms (cdr l))))
        (else (+ (sum-atoms (car l)) (sum-atoms (cdr l))))))

(sum-atoms  '(1 3 (3 1 1) (3 1 4)))



(define (deep-map f l)
  (cond ((null? l) (list))
        ((atom? (car l)) (cons (f (car l))
                               (deep-map f (cdr l))))
        (else (cons (deep-map f (car l))
                    (deep-map f (cdr l))))))

(deep-map add1 '(1 3 (3 1 1) (3 1 4)))

;greshno e
(define (flatten f l)
  (cond ((null? l) (list))
        ((atom? (car l)) (append (f (car l))
                               (flatten f (cdr l))))
        (else (append (flatten f (car l))
                    (flatten f (cdr l))))))


(define t '(1
            (2 
             (4 () ())
             ())
            (3
             ()
             (5 ()
                (()
                 (6 () ()))))))

(define tt
  (mkt 1
       (mkt 2 (mkl 4) '())
       (mkt 3 '() (mkt 5 '() (mkl 6)))))
            
