#lang plai-typed

(define (zero f x) x)

(define (one f x) (f x))

(define (two f x) (f (f x)))


;(zero rest 1)

(one rest(list 1 2 3 4))

(two rest(list 1 2 3 4))



(define (add1 n) (lambda (g x) (g (n g x))))

((add1 two) rest (list 1 2 3 4))

(define (add g f) (lambda (g f) (n g(n g f))))

((add one two) one 2)

