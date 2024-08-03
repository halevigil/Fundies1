;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |test 2 2018|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Expr is one of:
; - String (representing a variable)
(define-struct lam [var body])
; - (make-lam String Expr)
(define-struct app [fun arg])
; - (make-app Expr Expr)
; interpretation: An Expr represents an expression, and
; is either a String (which represents a variable),
; or a (make-lam v b) which represents a lambda
; expression where v is a variable bound by the
; lambda and b is the body of the lambda,
; or a (make-app f a) representing an application
; of an expression f to an argument a.

;free-bound: Expr -> Expr
;replaces all free variables with "free" and all bound variables with "bound",
;except those in the var position of a lambda
(define (free-bound exp)
  (local [;bound: Expr -> Expr
          ;replaces all variables with "bound" except those in the var position of a lam
          (define (bound exp)
            (cond
              [(string? exp) "bound"]
              [(lam? exp) (make-lam (lam-var exp) (bound (lam-body exp)))]
              [(app? exp) (make-app
                           (bound (app-fun exp))
                           (bound (app-arg exp)))]))]
    (cond
      [(string? exp) "free"]
      [(lam? exp) (make-lam (lam-var exp) (free-bound (lam-body exp)))]
      [(app? exp) (make-app
                   (bound (app-fun exp))
                   (free-bound (app-arg exp)))])))

(check-expect (free-bound "x") "free")
(check-expect (free-bound (make-lam "y" "z"))
              (make-lam "y" "free"))
(check-expect
 (free-bound (make-lam "y"
                       (make-app (make-lam "z" "y")
                                 "z")))
 (make-lam "y"
           (make-app (make-lam "z" "bound")
                     "free")))


#;(define fib
    (make-rr (list (make-base 0 1)
                   (make-base 1 1))
             (make-recursive (λ (n) (list (sub1 n) (- n 2)))
                             (λ (n subresults)
                               (apply + subresults)))))

#;(define fact
    (make-rr (list (make-base 0 1))
             (make-recursive (λ (n) (list (sub1 n)))
                             (λ (n subresults)
                               (* n (first subresults))))))

#;(define numbers
    (make-rr (list (make-base 0 "0"))
             (make-recursive (λ (n) (list (sub1 n)))
                             (λ (n subresults)
                               (string-append (first subresults) (number->string n))))))

;Exercise 2
#|(define (eval-input rr i)
  (local [;in-inputs? [List-of [Base X]] Nat -> Boolean
          ;is the given number one of the base cases for the recurrence relation?
          (define (in-inputs? bases input)
            (ormap (λ (b) (= input (base-input b))) bases))
          ;get-output: [List-of [Base X]] Nat -> X
          ;gets the output for the base case with input _input_
          ;ASSUMES: one of the base cases has input _input_
          (define (get-output bases input)
            (base-output (first (filter (= input (base-input b))))))
          (define base-cases (rr-bases rr))]
    (if (in-inputs? base-cases i)
        (get-output base-cases i)
        (eval-recursive-case rr)))
  (define (eval-recursive-case recur)
    (local [(define 
              (define recur-calls
                (map (λ (x) (eval-input x)))
                ((recursive-gen-args recur) i)))])))
((recursive-comb recur) i base-outputs)

(define (recurrence->func rr)
  (λ (i)
    (local [
            
            ]
      (if (in-inputs? base-cases i)
          (get-output base-cases i)
          (local [(define outputs (map (λ (x) (get-output x)) (rr-gen-args)))])))))|#




;;;;;;;;;;;;;;;;
(define-struct node (size value left right))
; A [FullTree X] is one of:
; - ’leaf
; - (make-node Nat X [FullTree X] [FullTree X])
; interpretation: A [FullTree X] is either an empty tree ('leaf)
; or a node (make-node sz val l r) where sz represents the
; number of nodes (including this node itself and the nodes
; in the subtrees) that are in the tree, val is some data,
; l is the left subtree, and r is the right subtree.
; A full tree is guaranteed to be perfectly balanced, which
; means the height of its two subtrees are equal and both
; of its subtrees are perfectly balanced. In other words,
; non-empty full trees can only contain 1, 3, 7, 15, 31, etc.
; values.

(define LEAF 'leaf)
(define ft1 (make-node 1 'good LEAF LEAF))
(define ft2 (make-node 3 'hello
                       ft1
                       (make-node 1 'bye LEAF LEAF)))
(define ft3 (make-node 3 'so
                       (make-node 1 'it LEAF LEAF)
                       (make-node 1 'is LEAF LEAF)))
(define ft4 (make-node 7 'foo ft2 ft3))


;tree-combine: Nat [FullTree X] [FullTree X] -> [FullTree X]
;generates a new full tree with _root_ stored in the root branch
;and _left_ and _right being the left and right subtrees
(define (tree-combine root left right)
  (cond
    [(and (symbol? left) (symbol? right)) (make-node 1 root left right)]
    [(and (node? left) (symbol? right)) (make-node (+ (node-size left) 1) root left right)]
    [(and (symbol? left) (node? right)) (make-node (+ (node-size right) 1) root left right)]
    [(and (node? left) (node? right)) (make-node (+ (node-size left) (node-size right) 1)
                                                 root
                                                 left
                                                 right)]))
(check-expect (tree-combine 'good LEAF LEAF) ft1)
(check-expect (tree-combine 'foo ft2 ft3) ft4)

(define (tree-ref tree ref)
  (cond
    [(zero? ref) (node-value tree)]
    [(< ref (/ (node-size tree) 2)) (tree-ref (node-left tree) (sub1 ref))]
    [(> ref (/ (node-size tree) 2)) (tree-ref (node-right tree)
                                              (- ref (/ (add1 (node-size tree)) 2)))]))
(check-expect (tree-ref ft1 0) 'good)
(check-expect (tree-ref ft2 0) 'hello)
(check-expect (tree-ref ft2 1) 'good)
(check-expect (tree-ref ft2 2) 'bye)
(check-expect (build-list 7 (λ (i) (tree-ref ft4 i)))
              '(foo hello good bye so it is))

;quicklist-ref: [QuickList X] Natural -> X
;produces the n-th element of the quicklist
(define (quicklist-ref quicklist ref)
  (if
    (< ref (node-size (first quicklist)))
      (tree-ref (first quicklist) ref)
      (quicklist-ref (rest quicklist) (- ref (node-size (first quicklist))))))
(check-expect
 (build-list 4
             (λ (i) (quicklist-ref (list ft1 ft3) i)))
 '(good so it is))

(define (quick-cons e quicklist)
  (local [(define (node-size=? n1 n2)
            (= (node-size n1) (node-size n2)))]
    (if
     (node-size=? (first quicklist) (list-ref quicklist 1))
     (cons (tree-combine e (first quicklist) (list-ref quicklist 1))
           (rest (rest quicklist)))
     (cons (make-node 1 e LEAF LEAF) quicklist))))
(check-expect (quick-cons 'foo (list ft2 ft3)) (list ft4))
   



