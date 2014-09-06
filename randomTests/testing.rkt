#lang plai-typed

(define c (list 1 2 3 4 5))

(define (plus­5­each x)

  (cond 
    
;this is a comment
    
    
    #| do this 
    for long lines|# 
    
    [(empty? x) empty]

    [else (cons (+ (first x) 5) 

       (plus­5­each (rest x)))]))
  
