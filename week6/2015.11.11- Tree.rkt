(define t '(1
            (2 
             (4 () ())
             ())
            (3
             ()
             (5 ()
                (()
                 (6 () ()))))))




(define (mkt value left right)
  (list value left right))

(define (mkl x)
  (mkt x '() '()))

(define tt
  (mkt 1
       (mkt 2 (mkl 4) '())
       (mkt 3 '() (mkt 5 '() (mkl 6)))))


(define (empty-tree? tree)
  (null? tree))

(define (root tree)
  (car tree))

(define (left tree)
  (car (cdr tree)))

(define (right tree)
  (car (cdr (cdr tree))))


(define (count-nodes tree)
  (cond ((empty-tree? tree) 0)
        (else (+ 1 (count-nodes (left tree)) 
                 (count-nodes (right tree))))))

(define (height tree)
  (cond ((empty-tree? tree) 0)
        (else (max (+ 1 
                      (height (left tree)))
                   (+ 1 
                      (height (right tree)))))))

(height tt)

(define (tree-map f tree)
  (cond ((empty-tree? tree) tree)
        (else (mkt (f (root tree))
                   (tree-map f left-tree)
                   (tree-map f right-tree)))))