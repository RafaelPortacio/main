#lang racket
(require "funcoes-do-livro.rkt")


(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (is-in? elt lst) ;recebe um elemento e uma lista e responde (com boolean) se o elemento está na lista
  (pair? (filter (λ (x) (equal? x elt)) lst)))

(define (any-of-in? lst1 lst2) ;recebe duas listas e responde (com boolean) se elas tem algum elemento em comum
  (pair? (filter (λ (x) (is-in? x lst1)) lst2)))

(define (aux-t-sub lsub) ;recebe uma lista de subconjuntos da subsets (lsub) e retorna a lista com a remoção dos subconjuntos repetidos
  
  (filtermod (λ (x) (not (any-of-in? (permutations (car x)) (cdr x)))) lsub)) ;FILTER MODIFICADO, definição presente no arquivo funcoes-do-livro.rkt

(define (true-subsets l) ;subsets aprimorado
  (aux-t-sub (subsets l)))

(true-subsets (list 1 2 1 3))