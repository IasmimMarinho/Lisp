#lang racket

;  Versão com auxiliar
(define (inverte l)
  (inv-aux l '()))

(define (inv-aux l a)
  (if (null? l)
      a
      (inv-aux (rest l) (cons (first l) a))
      ))

; Versão sem auxiliar
(define (inv l [a '()])
  (if (null? l)
      a
      (inv (rest l) (cons (first l) a))
      ))
  

; Versão com auxiliar
  
(define (inverte* lg)
  (inv-aux* lg '()))

(define (inv-aux* lg lga)
  (cond
    [(null? lg) lga]
    [(list? (first lg))
     (inv-aux* (rest lg) (cons (inv-aux* (first lg) '()) lga))]
    [else
     (inv-aux* (rest lg) (cons (first lg) lga))]
    ))


; Versão sem auxiliar
(define (inv* lg [lga '()])
  (cond
    [(null? lg) lga]
    [(list? (first lg))
     (inv* (rest lg) (cons (inv* (first lg) '()) lga))]
    [else
     (inv* (rest lg) (cons (first lg) lga))]
    ))

; Funções Variádicas
(define (soma . n)
  (if (null? n)
      0
      (+ (first n)
         (apply soma (rest n)))
      ))

; map com uma lista parâmetro
(define (meu-map f l)
  (if (null? l)
      '()
      (cons (f (first l))
            (meu-map f (rest l)))
      ))

; map variádico
(define (meu-map-v f . ll)
  (if (null? (first ll))
      '()
      (cons (apply f (meu-map first ll))
            (apply meu-map-v f (meu-map rest ll)))
      ))


; remove todos sem parâmetros nomeados
(define (rem-todos a l)
  (cond
    [(null? l) l]
    [(eqv? a (first l)) (rem-todos a (rest l))]
    [else
       (cons (first l) (rem-todos a (rest l)))]
    ))


(define (i a) a)

; remove todos com parâmetros nomeados opcionais
(define (rem-todos2 a l #:fc [f eqv?] #:ec [k identity])
  (cond
    [(null? l) l]
    [(f a (k (first l))) (rem-todos2 a (rest l) #:fc f #:ec k)]
    [else
       (cons (first l) (rem-todos2 a (rest l) #:fc f #:ec k))]
    ))


