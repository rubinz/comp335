#lang plai-typed
(define (InsertSort l)
    (cond
      [(empty? l) empty]
      [else (cons (+ (first l) 5)
                  (InsertSort (rest l)))]))
  
  





(reverse (list 1 2 3))
