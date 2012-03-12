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
(define (interpret₀ s₀ [interpret interpret₀]) s₀)
