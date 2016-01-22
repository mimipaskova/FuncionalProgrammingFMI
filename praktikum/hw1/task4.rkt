;4 zad
(define (counts elem lst)
  (length (filter (lambda (x) (equal? x elem)) lst)))

;remove repeated elements
(define (elem? elem lst)
  (if (> (counts elem lst) 0) #t #f))

(define (make-set lst)
  (define (helper lst res)
    (cond ((null? lst) res)
          ((elem? (car lst) res) (helper (cdr lst) res))
          (else (helper (cdr lst) (append res (list (car lst)) )))))
  (helper lst '()))
;(make-set '(1 2 2 4 3 2 1 3 4))

(define (histogram lst)
  (make-set (map (lambda (x) (cons x (counts x lst))) lst)))

(histogram '(1 2 1 1 2 2 3 2))
(histogram '(4 8 2 3 3 4 5 6 4))
(histogram '(#\a #\b #\c #\d #\b #\e #\b))
