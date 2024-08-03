;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of: 
; – (make-no-parent)
; – (make-child FT FT String N String)


(define (count-persons ft)
  (cond
    [(no-parent? ft) 0]
    [(child? ft) (+ 1
                    (count-persons (child-father ft))
                    (count-persons (child-mother ft)))]))
(check-expect (count-persons
               (make-child
                (make-child
                 (make-no-parent)
                 (make-no-parent)
                 "Bob" 1970 "Green")
                (make-no-parent)
                "Jessica" 1998 "Brown")) 2)


(define-struct avg [sum n])
;A Avg is a (make-avg Number Number)
;a (make-avg sum n) represents n numbers that sum to _sum_


;average-age: FamilyTree Number -> Number
;computes the average age of all child structures in the tree
(define (average-age ft yr)
  (local [;sum-age: FamilyTree Number -> Number
          ;computes the sum of the ages of all child structures in the tree
          (define (sum-age ft yr)
            (cond
              [(no-parent? ft) 0]
              [(child? ft) (+ (-  yr (child-date ft))
                              (sum-age (child-father ft) yr)
                              (sum-age (child-mother ft) yr))]))]
    (/ (sum-age ft yr) (count-persons ft))))
(check-expect (average-age
               (make-child
                (make-child
                 (make-no-parent)
                 (make-no-parent)
                 "Bob" 1970 "Green")
                (make-no-parent)
                "Jessica" 1998 "Brown") 2010) 26)

(define NP (make-no-parent))

; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field
(define (blue-eyed-child? ft)
  (cond
    [(no-parent? ft) #f]
    [(child? ft)
     (or (string=? (child-eyes ft) "blue")
         (blue-eyed-child? (child-father ft))
         (blue-eyed-child? (child-mother ft)))]))
(check-expect (blue-eyed-child? (make-child (make-child NP NP "Carl" 1926 "green")
                                            (make-child NP NP "Bettina" 1926 "green")
                                            "Adam"
                                            1950
                                            "hazel")) #f)
(check-expect (blue-eyed-child? (make-child (make-child NP NP "Carl" 1926 "green")
                                            (make-child NP NP "Bettina" 1926 "green")
                                            "Adam"
                                            1950
                                            "blue")) #t)
(check-expect (blue-eyed-child? (make-child (make-child NP NP "Carl" 1926 "green")
                                            (make-child NP NP "Bettina" 1926 "blue")
                                            "Adam"
                                            1950
                                            "green")) #t)
; FT -> Boolean
; does an-ftree contain an ancestor child
; structure with "blue" in the eyes field
(define (blue-eyed-ancestor? ft)
  (local [(define dad (child-father ft))
          (define mom (child-mother ft))]
    (cond
      [(no-parent? ft) #f]
      [(child? ft)
       (or (blue-eyed-child?
            (child-father ft))
           (blue-eyed-child?
            (child-mother ft)))])))
(check-expect (blue-eyed-ancestor? (make-child (make-child NP NP "Carl" 1926 "green")
                                               (make-child NP NP "Bettina" 1926 "green")
                                               "Adam"
                                               1950
                                               "hazel")) #f)
(check-expect (blue-eyed-ancestor? (make-child (make-child NP NP "Carl" 1926 "green")
                                               (make-child NP NP "Bettina" 1926 "green")
                                               "Adam"
                                               1950
                                               "blue")) #f)
(check-expect (blue-eyed-ancestor? (make-child (make-child NP NP "Carl" 1926 "green")
                                               (make-child NP NP "Bettina" 1926 "blue")
                                               "Adam"
                                               1950
                                               "green")) #t)

;eye-colors: FamilyTree -> [List-of String]
;produces a list of all eye colors in the tree
(define (eye-colors ft)
  (cond
    [(no-parent? ft) '()]
    [(child? ft) (append
                  (list (child-eyes ft))
                  (eye-colors (child-mother ft))
                  (eye-colors (child-father ft)))]))
(check-expect (eye-colors
               (make-child (make-child NP NP "Carl" 1926 "green")
                           (make-child NP NP "Bettina" 1926 "blue")
                           "Adam"
                           1950
                           "red")) (list "red" "blue" "green"))


;foldr:
;(f (f (f x1) x2) x3)

(define (folder func base l)
  (cond
    [(empty? l) base]
    [(cons? l) (func (first l) (folder func base (rest l)))]))

;(f (f (f x3) x2) x1)

(define (foldel func base l)
  (cond
    [(empty? l) base]
    [(cons? l)
     (foldel func (func (first l) base) (rest l))]))
(check-expect
 (foldel (λ (x1 x2) (- x2 x1)) 0 '())
 0)
(check-expect
 (foldel (λ (x1 x2) (- x2 x1)) 0 (list 1))
 -1)

(check-expect
 (foldel (λ (x1 x2) (- x2 x1)) 0 (list 1 2 3))
 -6)


	
; An S-expr is one of: 
; – Number
; – String
; – Symbol
; – [List-of S-expr]

(define (count sexp s)
  (local [(define (count/sl sl s)
            (cond
              [(empty? sl) 0]
              [(cons? sl) (+ (count (first sl) s)
                             (count/sl (rest sl) s))]))]
    (cond
      [(number? sexp) 0]
      [(string? sexp) 0]
      [(symbol? sexp)
       (if (symbol=? sexp s) 1 0)]
      [(list? sexp) (count/sl sexp s)])))

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

; An S-expr is one of: 
; – Number
; – String
; – Symbol
; – [List-of S-expr]

(define (substitute-all sexpr old-s new-s)
  (cond
    [(number? sexpr) sexpr]
    [(string? sexpr) sexpr]
    [(symbol? sexpr)(substitute/symbol sexpr old-s new-s)]
    [(list? sexpr) (map
                    (λ (sexpr0) (substitute-all sexpr0 old-s new-s))
                    sexpr)]))
(check-expect (substitute-all '(1 g (a 3) a) 'a 'b)
              '(1 g (b 3) b))


(define (substitute/symbol s old-s new-s)
  (if (symbol=? s old-s) new-s s))
(check-expect (substitute/symbol 'a 'a 'b) 'b)
(check-expect (substitute/symbol 's 'a 'b) 's)

(define (contains-symbol? sexpr s)
  (cond
    [(number? sexpr) #f]
    [(string? sexpr) #f]
    [(symbol? sexpr) (symbol=? sexpr s)]
    [(list? sexpr) (ormap
                    (λ(sexp) (contains-symbol? sexp s))
                    sexpr)]))
(check-expect (contains-symbol? '(0 4 (3 a t)) 'a) #t)
(check-expect (contains-symbol? '(0 4 (3 b t)) 'a) #f)

(define (substitute-first sexpr old-s new-s)
  (cond
    [(number? sexpr) sexpr]
    [(string? sexpr) sexpr]
    [(symbol? sexpr) (substitute/symbol sexpr old-s new-s)]
    [(list? sexpr) (substitute-first/losexpr sexpr old-s new-s)]))

(check-expect (substitute-first '(1 2 (4 3)) 'a 'b) '(1 2 (4 3)))
(check-expect (substitute-first '(1 a (a 3)) 'a 'b) '(1 b (a 3)))
(check-expect (substitute-first '(1 (a 3) (4 a)) 'a 'b) '(1 (b 3) (4 a)))

(define (substitute-first/losexpr los old-s new-s)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (contains-symbol? (first los) old-s)
         (cons (substitute-first (first los) old-s new-s) (rest los))
         (cons (first los) (substitute-first (rest los) old-s new-s)))]))


(check-expect (substitute-first/losexpr '(1 2 (4 3)) 'a 'b) '(1 2 (4 3)))
(check-expect (substitute-first/losexpr '(1 a (a 3))  'a 'b) '(1 b (a 3)))
(check-expect (substitute-first/losexpr '(1 (a 3) (4 a)) 'a 'b) '(1 (b 3) (4 a)))


;HW8 #6:
; A SExpr is one of:
; - Symbol
; - [List-of SExpr]

(define s1 'bruh)
(define s2 (list 'zeroth 'first 'second 'third 'fourth 'fifth))
(define s3 (list (list 'yo 'no 'way)
                 (list 'never 'gonna 'give 'you 'up) 'never 'gonna 'let 'you 'down))

(define (find-path sexpr s)
  (if (not (contains-symbol? sexpr s)) #f
      (cond
        [(symbol? sexpr) '()]
        [(list? sexpr) (find-path/los sexpr s)])))
(check-expect (find-path s1 'hi) #f)
(check-expect (find-path s1 'bruh) '())
(check-expect (find-path s2 'third) (list 3))
(check-expect (find-path s3 'gonna) (list 1 1))
(check-expect (find-path s3 'notinexp) #f)



;find-path/los.acc: [List-of Sexpr] Symbol -> Natural
;returns the index of the first Sexpr containing the given symbol
;ASSUMES: the given symbol is contained within one of the Sexprs
(define (find-path/los los s)
  (local [;find-path/los.acc: [List-of Sexpr] Symbol Natural -> Natural
          ;returns the index of the Sexpr containing the given symbol (given the current index)
          ;ASSUMES: the given symbol is contained within one of the Sexprs
          (define (find-path/los.acc l s i)
            (if (contains-symbol? (first l) s)
                (cons i (find-path (first l) s))
                (find-path/los.acc (rest l) s (add1 i))))]
    (find-path/los.acc los s 0)))



;;;;BST
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)
(define (search-bt bt n)          
  (cond
    [(no-info? bt) #f]
    [(node? bt) (cond
                  [(= (node-ssn bt) n) (node-name bt)]
                  [(symbol? (search-bt (node-left bt) n))
                   (search-bt (node-left bt) n)]
                  [(symbol? (search-bt (node-right bt) n))
                   (search-bt (node-right bt) n)]
                  [else #f])]))

(define bst0
  (make-node
   15
   'd
   (make-node
    12 'i NONE NONE)
   (make-node
    16
    'd
    NONE
    (make-node
     87 'h NONE NONE)
    )))

(check-expect (search-bt bst0 87) 'h)
(check-expect (search-bt bst0 12) 'i)
(check-expect (search-bt bst0 15) 'd)
(check-expect (search-bt bst0 18) #f)


(define (search-bst bst n)          
  (cond
    [(no-info? bst) #f]
    [(node? bst) (cond
                   [(= (node-ssn bst) n) (node-name bst)]
                   [(> n (node-ssn bst)) (search-bst (node-right bst) n)]
                   [(< n (node-ssn bst)) (search-bst (node-left bst) n)])]))

(check-expect (search-bst bst0 87) 'h)
(check-expect (search-bst bst0 12) 'i)
(check-expect (search-bst bst0 15) 'd)
(check-expect (search-bst bst0 18) #f)

;insert-node: BST Number Symbol -> BST
;inserts the given number and symbol as a new node in its proper place
;in the given bst
(define (insert-node bst n s)
  (cond
    [(no-info? bst) (make-node n s NONE NONE)]
    [(node? bst)(if (<= n (node-ssn bst))
                    (make-node (node-ssn bst) (node-name bst)
                               (insert-node (node-left bst) n s)
                               (node-right bst))
                    (make-node (node-ssn bst) (node-name bst)
                               (node-left bst)
                               (insert-node (node-right bst) n s)))]))
#;(define bst0
    (make-node
     15
     'd
     (make-node
      12 'i NONE NONE)
     (make-node
      16
      'd
      NONE
      (make-node
       87 'h NONE NONE)
      )))
(check-expect (insert-node bst0 90 'new)
              (make-node
               15
               'd
               (make-node
                12 'i NONE NONE)
               (make-node
                16
                'd
                NONE
                (make-node
                 87 'h NONE (make-node 90 'new NONE NONE)
                 ))))
;create-bst: [List-of (list Number Symbol)] -> BST
(define (create-bst l)
  (foldr (λ (numsym prev) (insert-node prev
                                       (first numsym)
                                       (first (rest numsym)))) NONE l))

#;(create-bst '((99 o)
  (77 l)
  (24 i)
  (10 h)
  (95 g)
  (15 d)
  (89 c)
  (29 b)
  (63 a)))


      



  
  
  





      
