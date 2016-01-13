(define lstt '(("mimi" "s" "m") ("mimi" "s" "mn") ("mimi" "sr" "m")))

; Input - list with words where third is label for the kind of word (m,j,sr,mn)
; Output - list with words with equal third value
(define (filterBySecondLabel lst kind)
  (filter (lambda (x) (equal? kind (car (cdr (cdr x))))) lst))

(define (filterByFirstLabel lst kind)
  (filter (lambda (x) (equal? kind (car (cdr x)))) lst))

;(filterBySecondLabel lstt "m")
;(filterByFirstLabel (filterBySecondLabel lstt "m") "s")

;(define example '(("golqmoto" "o" "sr") ("kuche" "p" "sr") ("lae" "s" "ed") ("pticite" "d")
;                                        ("zelenite" "o" "mn") ("jabi" "p" "mn") ("gledat" "s" "mn")
;                                        ("kum nebeto" "d") ))

(define in (open-input-file "data.txt"))
(define example (read in))


(define (filterDupulnenia lst)
  (filter (lambda (x) (not (equal? "d" (car (cdr x))))) lst))

;(filterDupulnenia example)

;Get all dopulnenia
(define (getAllDopulnenia lst)
  (map (lambda (x) (car x)) (filterByFirstLabel lst "d")))

;(getAllDopulnenia example)

;from 0 to length-1
;(random (length (getAllDopulnenia example)))

;first element is with index 0
(define (getNelement lst n)
  (if (= n 0) (car lst)
      (getNelement (cdr lst) (- n 1))))

;get random word which is filtered by f (function)
(define (getRandom f lst)
  (getNelement (f lst) (random (length (f lst)))))

;(getRandom getAllDopulnenia example)


;;;Podlog i opredelenie
;(define (getAllPodlog lst)
;  (map (lambda (x) (car x)) (filterByFirstLabel lst "p")))

(define (getAllPodlog lst)
  (filter (lambda (x) (equal? "p" (car (cdr x)))) lst))

(define (getAllOpredelenia lst)
  (filter (lambda (x) (equal? "o" (car (cdr x)))) lst))

(define (getAllSkazuemi lst)
  (filter (lambda (x) (equal? "s" (car (cdr x)))) lst))

;(getRandom getAllPodlog example)
;Problem - if there isn't kind mn
(define (findOpredelenieByPodlogAndPodlog lst)
  (let* [(podlog (getRandom getAllPodlog example))
         (kind (caddr podlog))
         ;(skazuemo (if (> 1 0) 1 0))
         (skazuemo (if (equal? kind "mn") (getRandom getAllSkazuemi (filterBySecondLabel (getAllSkazuemi example) "mn"))
        (getRandom getAllSkazuemi (filterBySecondLabel (getAllSkazuemi example) "ed"))))
         ]
    ;(if (equal? kind "mn") (car (getRandom getAllSkazuemi (filterBySecondLabel (getAllSkazuemi example) "mn")))
     ;   (car (getRandom getAllSkazuemi (filterBySecondLabel (getAllSkazuemi example) "ed"))))
    ;(cons (car (filterBySecondLabel (getAllOpredelenia lst) kind)) (list podlog skazuemo))))
(map (lambda (x) (car x)) (cons (car (filterBySecondLabel (getAllOpredelenia lst) kind)) (list podlog skazuemo)))))

(findOpredelenieByPodlogAndPodlog example)

(define (getOPD lst)
  (append (findOpredelenieByPodlogAndPodlog lst) (list (getRandom getAllDopulnenia lst))))

(define output (getOPD example))

(call-with-output-file "output.txt"
                    (lambda (out)
                      (write output out)))


