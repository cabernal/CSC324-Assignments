#lang racket

(provide interpret₀)

#| Interpret Scheme expression 's₀' built from the following subset:

    «numeric-literal»
    (if «condition-expr» «consequent-expr» «alternative-expr»)
    (+ «expr» ...)
    (zero? «expr»)

 For extensibility, the optionally provided 'intepret' is used to interpret 's₀'
  if it's not one of the above forms, and also for the sub-expressions in 's₀'.
|#


(define (interpret₀ s₀ [interpret interpret₀])
  (match s₀
    [`(if ,cond ,then-expr ,else-expr)
     (if (interpret₀ cond) (interpret₀ then-expr) (interpret₀ else-expr))]
    [`(+ . ,expr) (apply + (map interpret₀ expr))]
    [`(zero? ,expr) (zero? (interpret₀ expr))]
    [default 
      (if (number? default) default 
          (interpret default))]))

(interpret₀ '(if (zero? (+ 1 2)) (+ 3 4) (+ 5 6)))
(interpret₀ '(+ 1 1 1 1 1 1 (if (zero? 1) 0 1)))

