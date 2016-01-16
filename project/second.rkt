;Филтрира по втория етикет (род, число)
(define (filterBySecondLabel lst kind)
  (filter (lambda (x) (equal? kind (car (cdr (cdr x))))) lst))

;Филтрира по пътвия етикет вид на думата (п,о,с,д)
(define (filterByFirstLabel lst kind)
  (filter (lambda (x) (equal? kind (car (cdr x)))) lst))

;Четене на данни от файл data.txt
(define in (open-input-file "dataWithoutSubject.txt"))
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
  (if (= 0 (length (f lst))) -1
      (getNelement (f lst) (random (length (f lst))))))

;Problem - if there isn't kind mn
(define (generateSentenceWithoutObject)
  (cond ((or (equal? -1 (getRandom getAllSubjects example)) (equal? -1 (getRandom getAllPredicates example)))
         -1)
        (else
         (let* [(subject (getRandom getAllSubjects example))
                (kind (caddr subject))
                (predicate (if (equal? kind "mn") (getRandom getAllPredicates (filterBySecondLabel (getAllPredicates example) "mn"))
        (getRandom getAllPredicates (filterBySecondLabel (getAllPredicates example) "ed"))))
                ]
           (map car (list (car (filterBySecondLabel (getAllAttributes example) kind)) subject predicate))))))

(define (generateRandomSentence)
  (if (equal? -1 (generateSentenceWithoutObject)) -1
  (append (generateSentenceWithoutObject) (list (getRandom getAllObjects example) '"."))))

(define output (generateRandomSentence))

(define out (open-output-file "output.txt" 'append))

 (define (generateNSentences n)
  (cond ((equal? -1 (generateRandomSentence)) -1)
        ((> n 0)
         (write (apply string-append (map (lambda (x) (string-append x " ")) (generateRandomSentence))) out)
         (newline out)
         (generateNSentences (- n 1)))
        (else
         (close-output-port out))))

(generateNSentences 10)