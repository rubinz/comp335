#lang plai-typed

;(define x (list 1 2 3));
;(define num 1);


(define (helperfunction [x : (listof number)] [num : number]) : (listof number)
  (cond
    [(empty? x ) (list num)]
    [(<= num (first x)) (cons num x)]
    [else (cons (first x) (helperfunction (rest x) num) )])
    
)

(define (helperfunction2 [sorted1 : (listof number)] [unsorted1 : (listof number)]) : (listof number)
  (cond
    [(empty? unsorted1) sorted1]
    [else (helperfunction2 (helperfunction sorted1 (first unsorted1)) (rest unsorted1))]

)
)



(define (insertion-sort [x : (listof number)]) : (listof number) 
  (cond
    [(empty? x) empty]
    [else
          (helperfunction2 (list (first x)) (rest x))]
    
)

)


(test (insertion-sort (list 1 2 3)) (list 1 2 3))
(test (insertion-sort (list 3 2 1)) (list 1 2 3))
(test (insertion-sort (list 1 3 1)) (list 1 1 3))
(test (insertion-sort (list 1)) (list 1))
(test (insertion-sort empty) empty)