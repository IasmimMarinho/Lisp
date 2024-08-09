#lang racket

(define lg1
  '(((ra a) b d a) (fd s (www b)) (d (a))))

(define lg2 
  '(a (b d) c b (d) d e))

(define (membro? a l)
  (cond
    [(null? l) #f]
    [(eqv? a (first l)) #t]
    [else (membro? a (rest l))]
    ))

(define (membro*? a lg)
  (cond
    [(null? lg) #f]
    [(list? (first lg))
     (or (membro*? a (first lg))
         (membro*? a (rest lg)))
     ]
    [(eqv? a (first lg)) #t]
    [else (membro*? a (rest lg))]
    ))

(define (rmembro* a lg)
  (cond
    [(null? lg) '()]
    [(list? (first lg))
     (cons (rmembro* a (first lg))
           (rmembro* a (rest lg)))]
    [(eqv? a (first lg))
     (rmembro* a (rest lg))]
    [else
     (cons (first lg)
           (rmembro* a (rest lg)))]
    ))


(define (insere-dir2* n o lg)
  (cond
    [(null? lg) '()]
    [(list? (first lg))
     (cons (insere-dir2* n o (first lg))
           (insere-dir2* n o (rest lg)))]
    [(eqv? o (first lg))
     (cons (first lg)
           (cons n
                 (insere-dir2* n o (rest lg))))]
    [else
     (cons (first lg)
           (insere-dir2* n o (rest lg)))]
    ))

(define (substitui* o n lg)
  (cond
    [(null? lg) '()]
    [(list? (first lg))
     (cons (substitui* o n (first lg))
           (substitui* o n (rest lg)))]
    [(eqv? o (first lg))
     (cons n
           (substitui* o n (rest lg)))]
    [else
     (cons (first lg)
           (substitui* o n (rest lg)))]
    ))

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

; olhe rmembro*
; remove-uma-ocorrencia* remove somente a primeira
; ocorrência encontrada
(define (remove-uma-ocorrencia* a lg)
  (rest (r-uma-aux a lg)))
  
(define (r-uma-aux a lg)
  (cond
    [(null? lg) (list #f)]
    [(list? (first lg))
       (let [(resp (r-uma-aux a (first lg)))]
         (if (first resp)
             (cons #t
                   (cons (rest resp)
                         (rest lg)))
             (let [(resp2 (r-uma-aux a (rest lg)))]
               (cons (first resp2)
                     (cons (first lg)
                           (rest resp2))))))]
    [(eqv? a (first lg))
       (cons #t (rest lg))]
    [else
       (let [(resp2 (r-uma-aux a (rest lg)))]
               (cons (first resp2)
                     (cons (first lg)
                           (rest resp2))))]
    ))
             
    

