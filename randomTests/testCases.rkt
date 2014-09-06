#lang plai-typed


(test (test (insertion-sort (list 1 2 3)) (list 1 2 3)))
(test (insertion-sort (list 3 2 1)) (list 1 2 3))
(test (insertion-sort (list 1 3 1)) (list 1 1 3))
(test (insertion-sort (list 1)) (list 1))
(test (insertion-sort empty) empty)
      