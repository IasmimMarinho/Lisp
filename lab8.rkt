#lang racket/base
(require racket/function)

;Q1

(define (conta-se l #:teste [teste number?])
  (length (filter teste l)))

(conta-se '(1 2 "a" 3 "b" 4 "1"))
(conta-se '(1 2 3 4 5 6) #:teste even?)


;Q2

(define (remova-cond alvo l
                     #:test [test equal?]
                     #:chave [chave identity])
  (filter (lambda (elem)
            (not (test (chave elem) alvo)))
          l))

(remova-cond 3 '(1 2 3 4 3 5 3 6))


;Q3


(define (map-dobro-enc lst)
  (define resultados (map (lambda (x) (* 2 x)) lst))
  (lambda (p)
    (cond
      [(eq? p 'r) resultados]
      [(eq? p 'l) (length resultados)]
      [(and (integer? p) (>= p 0) (< p (length resultados)))
       (list-ref resultados p)]
      [else
       (error "Parâmetro inválido para map-dobro-enc:" p)])))
       
(define f (map-dobro-enc '(1 2 3 4)))


(f 'r)
(f 'l)
(f 2)
;(f -1)


