#lang plai-typed

(define-type Candy
  [gumball (color : symbol)]
  [chocolate (kg : number)]
  [lollipop (color : symbol) (cm : number)]
  )

(define 5-kg-choc (chocolate 5))

