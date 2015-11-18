;Primerni zadachi ot kontrolno

(define (accumulate op null-value term a next b)
  (if (> a b)
      null-value
      (op (term a) (accumulate op null-value term (next a) next b))))


(define (sum-digits n)
  (accumulate + 0 (lambda (x) (remainder x 10)) n (lambda (x) (quotient x 10)) 0))

(define (filter-accumulate pred? op null-value term a next b)
  (cond [(> a b) null-value]
        [(pred? a) (op (term a)
                       (filter-accum pred? op null-value term (next a) next b))]
        [else          (filter-accum pred? op null-value term (next a) next b)]))
  
(define (++ a) (+ 1 a))
  
;Tema 2
;1 zad tema 2
(define (prod-sum-digitss a b k)
  (filter-accumulate (lambda (x)
                       (zero? (remainder (sum-digits x) k))) * 1 (lambda (x) x) a ++ b))

;Tema 1
;2 zad tema 1
(define (average f g)
  (lambda (x) (/ (+ (f x) (g x)) 2 )))

(define (calcProd f n)
  (accumulate * 1 
              (lambda (i) ((average f (lambda (x) (expt i x))) i))
                1 ++ n))

;3 zad
(define (count x lst)
  (length (filter (lambda (el) (equal? x el)) lst)))
;vzemame duljinata na masiva, koito e samo s elementite, ravni na x

;(define (length lst)
 ; (apply + (map (lambda (el) 1) lst)))

(define (occurrences l1 l2)
  (define worker (lambda (el)
                   (count el l2)))
  (map worker l1))

;Tema 2
;3 zad
(define (duplicates l1 l2)
  (define worker (lambda (el)
                   (> (count el l2) 1)))
  (filter worker l1))

;Tema 1
;4 zad
(define (match-lengths? l1 l2)
  (let* [(d1 (map length l1))
        (d2 (map length l2))
        (length-diffs (map (lambda (x y) (abs (- x y)))))]
    ;vsichki chisla v tozi spisuk sa ednakvi
    ;vzemi vsichki chisla, razlichni ot purviq element i ako vurne prazen -> istina vsichki sa ravni
    ; ako vurne neprazen spisuk, to vrushta false
    (null? (filter  (lambda (el) (not (equal? el (car length-diffs))))))))

;alternativa na posledniq red
(foldr (lambda (x y) (and x y)) (map (lambda (el) (equal? el (car length-diffs))) length diffs))

              
              