;Филтрира по втория етикет (род, число)
(define (filterBySecondLabel lst kind)
  (filter (lambda (x) (equal? kind (car (cdr (cdr x))))) lst))

;Филтрира по пътвия етикет вид на думата (п,о,с,д)
(define (filterByFirstLabel lst kind)
  (filter (lambda (x) (equal? kind (car (cdr x)))) lst))

;Четене на данни от файл data.txt
(define in (open-input-file "data.txt"))
(define example (read in))

;намира всички определения и прави първата буква главна!
(define (getAllAttributes lst)
  (map (lambda (x) (cons (string-titlecase (car x)) (cdr x))) (filter (lambda (x) (equal? "o" (car (cdr x)))) lst)))

;намира всички подлози
(define (getAllSubjects lst)
  (filter (lambda (x) (equal? "p" (car (cdr x)))) lst))

;намира всички сказуеми
(define (getAllPredicates lst)
  (filter (lambda (x) (equal? "s" (car (cdr x)))) lst))

;Намира всички допълнения
(define (getAllObjects lst)
  (map (lambda (x) (car x)) (filterByFirstLabel lst "d")))

;Взема n елемент на лист, като индексирането започва от 0
(define (getNelement lst n)
  (if (= n 0) (car lst)
      (getNelement (cdr lst) (- n 1))))

;Намира произволна дума, която е филтрирана чрез f (функция)
(define (getRandom f lst)
  (getNelement (f lst) (random (length (f lst)))))

;Problem - if there isn't kind mn
(define (generateSentenceWithoutObject lst)
  (let* [(podlog (getRandom getAllSubjects example))
         (kind (caddr podlog))
         ;(skazuemo (if (> 1 0) 1 0))
         (skazuemo (if (equal? kind "mn") (getRandom getAllPredicates (filterBySecondLabel (getAllPredicates example) "mn"))
        (getRandom getAllPredicates (filterBySecondLabel (getAllPredicates example) "ed"))))
         ]
  (map car (list (car (filterBySecondLabel (getAllAttributes lst) kind)) podlog skazuemo))))

(define (generateRandomSentence lst)
  (append (generateSentenceWithoutObject lst) (list (getRandom getAllObjects lst) '".")))

(define output (generateRandomSentence example))

output

(define out (open-output-file "outputt.txt" 'append))

 

(define (generateNSentences n)
  (cond ((> n 0)
         (write (apply string-append (map (lambda (x) (string-append x " ")) (generateRandomSentence example))) out)
         (newline out)
         (lqlq (- n 1)))
        (else
         (close-output-port out))))

(generateNSentences 10)

                      


