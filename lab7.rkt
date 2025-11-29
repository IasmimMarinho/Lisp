#lang racket/base
(require racket/contract)


;Q1

(define (media . nums)
  (if (null? nums)
      0
      (/ (apply + nums) (length nums))))
(media 4 6 8)   ; => 6
(media 10)      ; => 10
(media)         ; => 0

;Q2

(define (filtrar-pares . nums)
  (filter even? nums))

(filtrar-pares 1 2 3 4 5 6)
;; ⇒ '(2 4 6)

;Q3
(define (contar-elementos . args)
  (length args))

(contar-elementos 10 20 30)  ; ⇒ 3

;Q4

(define (soma-aninhada . listas)
  (apply + (apply append listas)))
  
(soma-aninhada '(1 2) '(3 4) '(5))  ; ⇒ 15

;Q5

(define (produto . numeros)
  (apply * numeros))

(produto 2 3 4)  ; ⇒ 24

;Q6

(define/contract (soma2 a b)
  (-> number? number? number?)
  (+ a b))

(soma2 3 5)  ; ⇒ 8

;Q7

(define/contract (eh-par? n)
  (-> number? boolean?)
  (even? n))

(eh-par? 4)  

