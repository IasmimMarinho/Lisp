#lang racket

;Q1

(define conta-rest
  (lambda (x y . rest)
    (length rest)))

(conta-rest 1 2 3 4 5)

;Q2

(define soma-rest
  (lambda (x y . rest)
    (apply + rest)))


(soma-rest 1 2 3 4 5)

;Q3

((lambda args (length args)) 1 2 3 4)


;Q4

(remove* '(3) '(1 2 3 4 5 6))
  
;Q5

(remove* '(10 20 30 40 50) '(10 20 30 40 50)
  (lambda (a b)
    (zero? (remainder b 20))))

;Q6

(define (multiplo-de-tres? n)
  (zero? (remainder n 3)))
(remove* '(3 4 6 9 10) '(3 4 6 9 10)
  (lambda (a b)
    (multiplo-de-tres? b)))

;Q7

(map (lambda (x) (* 3 x)) '(1 2 3 4))

;Q8

(define produtos '(("banana" 3) ("maçã" 5) ("laranja" 4)))

(map (lambda (item)
       (list (first item)
             (* (second item) 0.8)))
     produtos)
     
;Q9

(andmap (lambda (palavra)
          (= (string-length palavra) 4))
        '("gato" "rato" "pato"))
        
;Q10

(build-list 5 (lambda (x) x))


;Q11

(build-list 5 (lambda (x) (* x x)))


;Q12

(filter (lambda (x) (> x 10)) '(3 12 8 20 5))

;Q13

(filter (lambda (x) (even? x)) '(1 2 3 4 5 6))


;Q14

(sort '(5 1 8 3 2) <)

;Q15

(sort '("uva" "banana" "kiwi" "abacaxi")
      (lambda (a b)
        (< (string-length a) (string-length b))))

