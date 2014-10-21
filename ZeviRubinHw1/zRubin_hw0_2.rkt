#lang plai-typed

(define-type Tree 
  [node (n : number ) (left : Tree) (right : Tree)]
  [leaf]
  )


(define (combine [l1 : (listof number)] [l2 : (listof number)]) : (listof number)
  (cond
    [(empty? l1) l2]
    [else (combine (reverse (rest (reverse l1))) (cons (first (reverse l1)) l2))]

  ))
                   

(define (tree-to-list [t : Tree]) : (listof number) 
  (cond
    [(node? t) (combine (combine (tree-to-list (node-left t)) (list (node-n t))) (tree-to-list (node-right t)))]
    [(leaf? t) empty]

)
)

;test case

(tree-to-list(node 5 (node 4 (node 3 (leaf) (leaf)) (leaf)) (node 7 (leaf) (node 9 (node 8 (leaf) (leaf)) (leaf)))))

