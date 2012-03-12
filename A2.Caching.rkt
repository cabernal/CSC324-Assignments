#lang racket

(provide caching define-cached
         fibonacci)

#| Return a version of unary function 'uf' that caches its return values
    so that repeated calls with the same argument use the cached value.

 Let f′ be the returned function, and assume 'uf' has no side-effects.
 1. Given the same argument, f′ returns the same value as 'uf' would.
 2. The first time f′ is called with any particular argument, it records
     the result in a hash table. Later, if called with that same argument
     it returns the recorded result instead of calling 'uf' again.

 The notion of equality used to compare arguments is 'equal?'.
|#
(define (caching uf) uf)


#| [Remove this discussion before submitting].
 A few test cases. Decide carefully what the specification above says about these.
|#
(define (f n) (displayln 'hi) 'bye)
(define caching-f (caching f))
(map caching-f '(1 2 1))
((caching f) 2)
#|
 Convince yourself by a tracing diagram that the following won't make 'fibonacci₁'
  a fast version 'fibonacci₀'.
|#
(define (fibonacci₀ n)
    (if (< n 2)
        1
        (+ (fibonacci₀ (- n 1)) (fibonacci₀ (- n 2)))))
(define fibonacci₁ (caching fibonacci₀))
#|
 For the insight needed to implement 'define-cached': define 'fibonacci₀' without
  the shorthand create-and-name syntax, and insert a *single* call to 'caching'
  in the definition to make a fast 'fibonacci₀'. Now make 'define-cached', for
  any unary function defined in shorthand form.
|#


(define-syntax-rule (define-cached «» ...) (define «» ...))

(define-cached (fibonacci n)
  (if (< n 2)
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
