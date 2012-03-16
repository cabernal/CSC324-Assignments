#lang racket

#| g0bernal g1amogse |#

(provide Σ
         ℳ ₍
         rows columns
         ℝ· ·)


(define-syntax-rule (block «expr» ...) (let () «expr» ...))

(define-syntax Σ
  (syntax-rules (= to)
    [(Σ «variable» = «lower» to «higher» «body-expr»)
     (block 
      ;Make sure higher bound «higher» is indeed greater than the lower bound «lower».
      (when (> «higher» «lower»)
        (define sum 0)
        ;Initialize the given variable «variable» to be the lower bound.
        (define «variable» «lower»)
        ;Define a loop where the result of calling the expression «body-expr»
        ;is added to the sum.
        (define (loop «variable») (set! sum (+ sum «body-expr»))
          (unless (equal? «variable» «higher»)
            (loop (+ «variable» 1))))
        ;Call the loop
        (loop «variable»)
        ;Return the sum
        sum))]))

(define-syntax ℳ
  (syntax-rules (× ₍ ₎)
    [(ℳ «rows» × «cols» ₍ «row-name» «col-name» ₎ «expr»)
     ;We want to match the pattern exactly, so × ₍ ₎ are provided in syntax-rules
     (block 
      (define «row-name» 1)
      (define «col-name» 1)
      ;Make a vector with the same number of slots as «rows»
      (define m (make-vector «rows»))
      ;Define two loops, one that calls the expression «expr» on each entry,
      ;and one that calls the above loop for each row of the matrix.
      (define (row-loop) (unless (> «row-name» «rows»)
                           (define n (make-vector «cols»))
                           (set! «col-name» 1)
                           (col-loop n)
                           (vector-set! m (- «row-name» 1) n)
                           (set! «row-name» (+ «row-name» 1))
                           (row-loop)))
      (define (col-loop var) (unless (> «col-name» «cols»)
                               (vector-copy! var (- «col-name» 1) (vector «expr»))
                               (set! «col-name» (+ «col-name» 1))
                               (col-loop var)))
      (row-loop)
      m)]))

(define (rows «matrix»)
  ;Return length of matrix (which is a vector).
  ;If empty, vector-length will automatically return 0, so no checks are needed.
  (vector-length «matrix»))

(define (columns «matrix»)
  (if (equal? (vector-length «matrix») 0)
      ;If given empty matrix, return 0 columns.
      0
      (vector-length (vector-ref «matrix» 0))))

(define-syntax ₍
  (syntax-rules (₎)
    [(₍ «matrix» «row» «col» ₎)
     ;We want to match this pattern exactly (with the closing lower bracket)
     ;So define-syntax used, to provide identifiers
     (block
      (when (and (<= «row» (rows «matrix»)) (<= «col» (columns «matrix»)))
        ;As long as the row and column desired is within the scope of the matrix, 
        ;Return the reference from the desired entry.
        (vector-ref (vector-ref «matrix» (sub1 «row»)) (sub1 «col»))))]))

(define (ℝ· . x)
  ;Simply apply multiplication on x.
  (apply * x))

(define (· . x)
  (cond [(equal? (length x) 0) 1
                               ;Return 1 if no arugments given
                               ]
        [(equal? (length x) 1) (first x)
                               ;Just return the given argument.
                               ]
        [(equal? (length x) 2)
         (cond [(and (number? (first x)) (vector? (second x)))
                (ℳ 3 × 4 ₍ i j ₎ (* (first x) (₍ (second x) i j ₎)))
                ;Multiply each entry of the matrix by the scalar. 
                ]
               [(and (number? (second x)) (vector? (first x)))
                (ℳ 3 × 4 ₍ i j ₎ (* (second x) (₍ (first x) i j ₎)))
                ;Same as above.
                ] 
               [(and (number? (first x)) (number? (second x)))
                (ℝ· (first x) (second x))
                ;Call the scalar multiplication function on the numbers.
                ]
               [else
                (ℳ (rows (first x)) × (columns (second x)) ₍ i j ₎ (Σ c = 1 to (columns (first x)) (* (₍ (first x) i c ₎) (₍ (second x) c j ₎))))
                ;Create a new matrix, with the number of rows of the matrix on left, and number of cols of matrix on right
                ;Each entry will contain the sum of the product of the (i,c)th entry of right matrix,
                ;and the (c, j)'th entry of the left matrix.
                ])]
        [else (· (first x) (apply · (rest x)))
              ;If given more than 3 args,
              ;Recursively call · on the first argument with · applied to the rest of the arguments
              ]))
