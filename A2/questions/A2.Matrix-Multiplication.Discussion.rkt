#lang racket
#| Sean Uppal [a MAT223/224 Lecturer] asked whether there are languages
    in which he can capture and run the algorithms presented in lecture,
    during the lecture.
|#
(require (only-in "A2.Matrix-Multiplication.rkt"
                  Σ 
                  ℳ ₍
                  rows columns
                  ℝ· ·))

#| Let's take a look at matrix multiplication on Wikipedia:
|#
#;http://en.wikipedia.org/wiki/Matrix_multiplication#Technical_details
#|
 There are two new common concepts and associated notations:
   1. Sum an expression depending on a variable name ranging over the
       set of integers from a start expression to an end expression.
   2. Define a matrix by an element expression depending on two variables
       each ranging a set of natural numbers from 1 up to an end expression.
 
 As asked and briefly looked at in lecture, infix notation is straightforward
  if fully-parenthesized. One would give Sean some of that, but it's not the
  core signficance of syntactic forms so we'll omit that wrinkle while learning
  and practicing in this assignment.
|#

#| Summation Notation.

 Implement 'Σ' to make the following kind of expression work.

|#
#;#;68351585149469122636640694597412187693952969395515555130761821634974212652769227314494234976845824
(Σ c = 223 to 324 (expt 2 c))

#| Matrix Definition [A Matrix "Comprehension" Notation].
 
 Matrices are represented as a vector of row vectors.

 Implement 'ℳ', '₍', 'rows' and 'columns' to make the following work.
|#
(define A (ℳ 3 × 4 ₍ i j ₎ (+ i j)))
#;'#(#(2 3 4 5)
     #(3 4 5 6)
     #(4 5 6 7))
(define B (ℳ 4 × 3 ₍ a b ₎ (- a b)))
#;'#(#(0 -1 -2)
     #(1  0 -1)
     #(2  1  0)
     #(3  2  1))
#;(rows A) #; 3
#;(columns A) #; 4
#;(₍ B 2 3 ₎) #; -1
#;(₍ B 4 1 ₎) #; 3

#| Multiplication.

 Implement mixed multiplication of any number of scalars [real numbers] and matrices.

 For zero arity multiplication return the Real number 1.
 
 Implement 'ℝ·' to do [only] regular scalar multiplication, to visually distinguish it
  from the overloaded version when pedagogically worthwhile.

 For binary matrix multiplication try to match the Wikipedia formula presentation
  as closely as reasonable. Since Real number multiplication is being bootstrapped
  use 'ℝ·' as appropriate.
|#
#;(· A B)
#;'#(#(26 12 -2)
     #(32 14 -4)
     #(38 16 -6))
#;(· 3 2 4) #; 24
#;(· 324 A)
#;'#(#(648  972  1296 1620)
     #(972  1296 1620 1944)
     #(1296 1620 1944 2268))