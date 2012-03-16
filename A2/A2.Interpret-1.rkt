#lang racket
#| g0bernal g1amogse |#
(provide interpret
         Tree Environment Closure)

#| partners: g0bernal , g1amogse |#

#| The Semantic/Memory/Runtime/Evaluation Model is a Tree.

 For interpretation only the parent relation is needed, i.e.
  the up arrows in the drawings. It's used by a running/called
  closure, to look up [literally] from its local/parameter
  environment any identifiers not in that environment.

 The lookup process goes through [skipping] each ancestor closure
  and checking for the identifier in the environment the closure
  was made in, i.e. the environment immediately above the closure.
  
 [Can you match this description with how you [should] trace?
  Answer that for the entire specification and your implementation!]
|#

#| Nodes in the Tree.
 
 Built only from function creation and calling [no specialized local
  environment operations] the levels alternate between environments
  and closures. Starting with a combined global and main top-level
  environment as the root: even numbered levels contain environments,
  odd numbered contain closures.
|#

#| Tree Implementation.

 As usual [CSC148/150] with pointer tree representations, a node vs
  a tree is simply a distinction of usage/context.

 A general [Sub-]Tree datatype contains a parent pointer, immutable.

 To traverse the tree for display in various forms, a children pointer
  is convenient. The arity isn't fixed, the children are represented
  by a list of sub-Trees, and new children can be added dynamically,
  so the children field is mutable [vs. the field's value ---another
  possible design choice].
|#
(struct Tree (parent
              [children #:mutable])
  #:transparent)

#| Environments.

 A subtype of 'Tree'.
   Represented by a struct type inheriting from 'Tree'.
   The 'Environment' constructor takes the arguments for a 'Tree',
    followed by the argument for its 'bindings' field.
   The 'Tree' fields are accessed and mutated by the 'Tree' operations.

 Created when a closure is called [implictly for the top-level environment].
   Parent: the closure it's a child of, or false for the top level.
     Represented by a Closure or #f.
     The parent adds it to its children.
   Children: A list of closures created during execution of the parent's body
              for this call to the parent.

 Bindings: mapping of parameter and body sequence defined ids to values.
   Represented by a hash-table of symbol→value.
|#
(struct Environment Tree
  (bindings) ;hash table contaning keys as identifiers
  #:transparent)

#| Closures.

 Created for lambdas in the body of another closure, during a call to that closure,
  as a child of the local parameter/define environment of that call.
   Parent: environment of the call in which it was created.
     Represented by an Environment.
     The parent adds it to its children.
   Children: A list of environments for each call to the closure.

 Parameters: sequence of identifiers.
   Represented by a list of symbols.
 Body: sequence of body expressions.
   Represented by a list of s-expressions.

 Unique Id: for display [vs pointers from variables to closures], a unique index.
   Represented by a number.
|#
(struct Closure Tree
  (parameters body uid)
  #:transparent)


#| The Language of Expressions to Interpret. |#
#;(λ («parameter-id» ...) «body-expr» ...)
#;(«closure-expr» «argument-expr» ...)
#;(set! «id» «expr»)
#;«identifier»
;
; Additionally, a «body-expr» may be a 'define', slightly restricted:
;   The «id» is not a «parameter-id» for the body's λ, nor the «id» of
;   a 'set!' in a preceding «body-expr».
; When a closure is called, its body's 'define'd identifiers are added to
;  the enviroment with an undefined value, before the body is executed.
#;(define «id» «expr»)
;
#| Strong hints for simplest/shortest implementation: make a 'match' with
    a pattern for each of those. Each clause is on average a half dozen lines,
    feel free to make helper methods for the most atomic steps. You can also
    leaf through the tracer [which does something slightly different and
    more complicated, but contains most of this interpreter] as long as you
    don't try to adapt it without thinking or are distracted by the extra
    difficulty of its job: trying to copy paste and modify will get out of
    control. Using this as an opportunity to learn and formalize and record
    manual tracing of code will work very well and be rewarding in many ways.
|#

#| Value for 'define'd identifiers whose define has not yet executed. |#
(define undefined (letrec ([u u]) u))

#| Function that when called repeatedly returns 0, 1, 2, ... . |#
(define (uid!)
  (define c -1) 
  (λ () (set! c (+ 1 c)) c))



#| Global count for id generation|#
(define new-id (uid!))

#| Put all define ids in given environment.|#
#;(define (allocate-defines expr env)
    
    )

#| Return the value of source code s-expression 'expr' from the "Language
    of Expressions to Interpret", where all free/open identifiers [except
    for 'set!', 'λ', 'define', '+', 'zero?' and 'if' of course] are bound
    in Environment 'env'. If the result is a Closure, just return that
    [as opposed to a Racket <procedure>].
|#

(define (interpret expr env)  
  (match expr
    ; Assume at least one parameter given.
    [`(λ ,«parameter-id» . ,«body-expr»)
     (Closure env '() «parameter-id» «body-expr» (new-id))]
    
    [`(set! ,«id» ,«expr»)
     (define eval-expr (interpret «expr» env))
     (if (hash-has-key? (Environment-bindings env) «id» «expr»)
         (hash-set! (Environment-bindings env) «id» «expr»)
         (if (Tree-parent env)
             (interpret `(set! ,«id» ,«expr») (Tree-parent (Tree-parent env)))
             (error "reference to an identifier before its definition")))]
    
    [`(define ,«id» ,«expr»)
     

     (hash-set! (Environment-bindings env) «id» (interpret «expr» env))]
    
    [`(,«closure-expr» . ,«argument-expr»)
     

     
     ;Get closure and attach it to current environment.
     (define called-closure (interpret «closure-expr» env))
     
     (set-Tree-children! env (append (Tree-children env) (list called-closure)))
     
     ; Create environment based on closure's and argument expr
     ;    * Parent is curently called closure.
     ;    * No children, yet.
     ;    * Hash table to represent bindings.
     
     (define parameters (Closure-parameters called-closure))
     (define local-env (Environment called-closure '() (make-hash)))
     (define lst-vars (filter-map (λ (one-expr)
                                    (match one-expr
                                      [`(define ,var . ,rest) var]
                                      [_ #f])) (Closure-body called-closure)))
     
     (map (λ(param) 
            (hash-set! 
             (Environment-bindings local-env) param undefined))  lst-vars)

     (map (λ (param arg) 
            (hash-set! 
             (Environment-bindings local-env) param (interpret arg env))) parameters «argument-expr»)
     
     (set-Tree-children! called-closure (append (Tree-children called-closure) (list local-env)))
          
     (last (map (λ(x) (interpret x local-env)) (Closure-body called-closure)))]
    
    [`,«identifier» 
     ;look for «identifier»  in ancestor environments (even environemnts)
     (if (number? «identifier» ) «identifier» 
         (if (hash-has-key? (Environment-bindings env) «identifier»)
             (hash-ref (Environment-bindings env) «identifier» )
             (when (Tree-parent env)
               (interpret «identifier» (Tree-parent (Tree-parent env))))))]))

;Working test
#;(define env (Environment #f '() (make-hash)))
#;(define expr '((λ() (define f (λ (x) (λ () x))) (define g (f 324)) (g))))
#;(interpret expr env)





