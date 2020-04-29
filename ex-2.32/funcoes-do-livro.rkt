#lang racket

;;ctrl C ctrl V de funções do livro que foram úteis pro programa (leve alteraçâo no filter)
(define (filtermod predicate sequence)
  (cond ((null? sequence) null)
        ((predicate sequence) ;diferente do filter tradicional, o predicado recebe toda a sequence
         (cons (car sequence)
               (filtermod predicate (cdr sequence))))
        (else (filtermod predicate (cdr sequence)))))


(define (accumulate op initial sequence) 
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list null)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(provide filtermod permutations)