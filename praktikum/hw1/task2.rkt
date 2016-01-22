;2 zad
(define (average lst)
  (/ (apply + (map (lambda (x) (car (cdr (cdr x)))) lst)) (length lst)))

(define (grades lst)
  (map (lambda (x) (car (cdr (cdr x)))) lst))

;izkarva ocenkite, a ne fakultetnite nomera
;ne moga da go izmislq kak da vzema fakultetnite im nomera
(define (maxx lst)
  (filter (lambda (x) (> x (average lst))) (grades lst)))


(define class '((77777 "Ivan" 5) (88888 "Stoyan" 3) (99999 "Penka" 6)))
(average class)
