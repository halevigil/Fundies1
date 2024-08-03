;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/string)
;;;From Homework 10;;;
;Exercise 4
; A [Set-of X] is a [List-of X] with no repeated values
; it represents a set
(define s1 '())
(define s2 (list 1 4))

; A Graph is a (make-graph [Set-of Symbol] [Symbol -> [Set-of Symbol]])
(define-struct graph [nodes neighbors])
; and represents the nodes and edges in a graph.
(define gr1 (make-graph (list 'a 'b 'c 'd)
                        (λ (n)
                          (cond
                            [(symbol=? n 'a) (list 'b 'd)]
                            [(symbol=? n 'b) (list 'c 'd)]
                            [(symbol=? n 'c) (list 'b)]
                            [(symbol=? n 'd) '()]))))
(define gr2 (make-graph (list 'c 'a 'b 'd)
                        (λ (n)
                          (cond
                            [(symbol=? n 'a) (list 'd 'b)]
                            [(symbol=? n 'b) (list 'c 'd)]
                            [(symbol=? n 'c) (list 'b)]
                            [(symbol=? n 'd) '()]))))
(define gr3 (make-graph (list 'c 'a 'd)
                        (λ (n)
                          (cond
                            [(symbol=? n 'a) (list 'd 'b)]
                            [(symbol=? n 'c) (list 'b)]
                            [(symbol=? n 'd) '()]))))
(define gr4 (make-graph (list 'c 'a 'b 'd)
                        (λ (n)
                          (cond
                            [(symbol=? n 'a) (list 'b)]
                            [(symbol=? n 'b) (list 'c 'd)]
                            [(symbol=? n 'c) (list 'b)]
                            [(symbol=? n 'd) '()]))))
(define gr5 (make-graph (list 'c 'a 'b 'd)
                        (λ (n)
                          (cond
                            [(symbol=? n 'a) '()]
                            [(symbol=? n 'b) '()]
                            [(symbol=? n 'c) '()]
                            [(symbol=? n 'd) '()]))))


;e-in-set?: {X} [Set-of X] X  [X X -> Boolean] -> Boolean
;is the given element in the given set?
(define (e-in-set? s set equiv?)
  (ormap (λ (x) (equiv? s x)) set))
(check-expect (e-in-set? 'a (list 'c 'a 'b) symbol=?) #t)
(check-expect (e-in-set? 'a (list 'c 'd 'b) symbol=?) #f)

;set=?: {X} : [Set-of X] [Set-of X] [X X -> Boolean] -> Boolean
;are the two given sets equal?
(define (set=? s1 s2 equiv?)
  (and (= (length s1) (length s2))
       (andmap (λ (e) (e-in-set? e s2 equiv?)) s1)))
(check-expect (set=? (list 0 1) (list 0 1 2) =) #f)
(check-expect (set=? (list 0 4 1) (list 0 1 2) =) #f)
(check-expect (set=? (list 0 4 1) (list 0 1 4) =) #t)

;neighbor-of?: Graph Symbol Symbol -> Boolean
;is the second node a neighbor of the first?
(define (neighbor-of? graph node1 node2)
  (local [(define neighbors-fn (graph-neighbors graph))]
    (e-in-set? node2 (neighbors-fn node1) symbol=?)))
(check-expect (neighbor-of? gr1 'a 'b) #t)
(check-expect (neighbor-of? gr1 'c 'a) #f)
;Exercise 5
;union: {X} [Set-of X] [Set-of X] [X X -> Boolean] -> [Set-of X]
;returns the union of the two sets
(define (union s1 s2 equiv?)
  (append (filter (λ (e) (not (e-in-set? e s1 equiv?))) s2) s1))
(check-expect (set=? (union (list 'a 'b) (list 'a 'c) symbol=?) (list 'c 'a 'b) symbol=?) #t)
(check-expect (set=? (union (list 0 1) (list 0 1) =) (list 0 1) =) #t)

;both-neighbors: Graph Symbol Symbol -> [Set-of Symbol]
;returns the set containing all neighbors of both functions

(define (both-neighbors graph node1 node2)
  (local [(define neighbors-fn (graph-neighbors graph))]
    (union (neighbors-fn node1) (neighbors-fn node2) symbol=?)))

(check-expect (set=? (both-neighbors gr1 'a 'b) (list 'b 'c 'd) symbol=?) #t)
(check-expect (set=? (both-neighbors gr1 'a 'c) (list 'b 'd) symbol=?) #t)

;; Exercise 6
;graph=?: Graph Graph -> Boolean
;are the two graphs equal?
(define (graph=? g1 g2)
  (local [(define nodes1 (graph-nodes g1))
          (define nodes2 (graph-nodes g2))
          (define neighbors1 (graph-neighbors g1))
          (define neighbors2 (graph-neighbors g2))]
    (and (set=? nodes1 nodes2 symbol=?)
         (andmap (λ (e) (set=? (neighbors1 e) (neighbors2 e) symbol=?)) nodes1))))

(check-expect (graph=? gr1 gr2) #t)
(check-expect (graph=? gr2 gr3) #f)
(check-expect (graph=? gr2 gr4) #f)

;; Exercise 7

; make-graph=? : Graph -> [Graph -> Boolean]
; Takes a graph and produces a function that checks if
; its argument is graph=? to the original graph
(define make-graph=? (λ (g1) (λ (g2) (graph=? g1 g2))))

; f : Graph ... -> Graph
; Do something to g
#;(define (f g ...) ...)
 
#;(check-satisfied (f some-input-graph ...)
                   (make-graph=?
                    some-expected-graph))

;; collapse : Symbol Symbol Symbol Graph -> Graph
;; collapses _n1_ _n2_ into one new node, which is named by _n3_.
;; All nodes in the graph that were pointing to either _n1_ and _n2_ should now point to _n3_,
;; and _n3_ should point to all nodes either _n1_ and _n2_ were pointing to


(define (collapse n1 n2 n3 gr)
  (local [(define nodes (graph-nodes gr))
          (define neigh (graph-neighbors gr))]
    (make-graph (replace n1 n2 n3 nodes)
                (λ (n)
                  (if (symbol=? n n3)
                      (replace n1 n2 n3 (both-neighbors gr n1 n2))
                      (replace n1 n2 n3 (neigh n)))))))

(check-satisfied (collapse 'a 'b 'f gr1)
                 (make-graph=?
                  (make-graph (list 'f 'c 'd)
                              (λ (n)
                                (cond
                                  [(symbol=? n 'f) (list 'f 'c 'd)]
                                  [(symbol=? n 'c) (list 'f)]
                                  [(symbol=? n 'd) '()])))))

(check-satisfied (collapse 'd 'a 'z gr2)
                 (make-graph=?
                  (make-graph (list 'z 'b 'c)
                              (λ (n)
                                (cond
                                  [(symbol=? n 'z) (list 'z 'b)]
                                  [(symbol=? n 'b) (list 'c 'z)]
                                  [(symbol=? n 'c) (list 'b)])))))


;; replace : Symbol Symbol Symbol [Set-of Symbol] -> [Set-of Symbol]
;; replaces the instance of _s1_ and/or _s2_ with _s3_ in a given set of symbols
;; Assume _s3_ is not in the set

(define (replace s1 s2 s3 ss)
  (foldr (λ (n ans-so-far)
           (cond
             [(and (or (symbol=? n s1) (symbol=? n s2)) (not (e-in-set? s3 ans-so-far symbol=?)))
              (cons s3 ans-so-far)]
             [(and (or (symbol=? n s1) (symbol=? n s2)) (e-in-set? s3 ans-so-far symbol=?))
              ans-so-far]
             [else (cons n ans-so-far)])) '() ss))

(check-expect (replace 'a 'b 'f (list 'a 'b 'c 'd)) (list 'f 'c 'd))
(check-expect (replace 'a 'b 'f (list 'a 'c 'd 'q)) (list 'f 'c 'd 'q))
(check-expect (replace 'a 'b 'f (list 'w 'e 'r 'h)) (list 'w 'e 'r 'h))
(check-expect (replace 'a 'b 'f (list 'c 'a 'd 'q)) (list 'c 'f 'd 'q))
;;;Homework 10 end;;;

;Exercise 1
;reverse-edges: Graph -> Graph
;reverses the edges in a graph (ie, if and only if 'a pointed to 'b in the original graph,
;'b will point to a in the new graph)
(define (reverse-edges g)
  (local [(define neighbors (graph-neighbors g))
          (define nodes (graph-nodes g))]
    (make-graph
     nodes
     (λ (n1)
       (filter (λ (n2) (neighbor-of? g n2 n1)) nodes)))))


(check-satisfied (reverse-edges gr5) (make-graph=? gr5))
(check-satisfied (reverse-edges gr1)
                 (make-graph=?
                  (make-graph (list 'a 'b 'c 'd)
                              (λ (n)
                                (cond
                                  [(symbol=? n 'a) '()]
                                  [(symbol=? n 'b) (list 'a 'c)]
                                  [(symbol=? n 'c) (list 'b)]
                                  [(symbol=? n 'd) (list 'a 'b)])))))

;Exercise 2
;map-from-to/symbol: Symbol [List-of Symbol] [List-of Symbol] -> Symbol
;replaces the given symbol, which should be in _map-from_, with it's corresponding
;symbol in _map-to_
(define (map-from-to/symbol e map-from map-to)
  (if (symbol=? e (first map-from)) (first map-to)
      (map-from-to/symbol e (rest map-from) (rest map-to))))
(check-expect (map-from-to/symbol 'a '(a b c d) '(e f g h)) 'e)
(check-expect (map-from-to/symbol 'c '(a b c d) '(e f g h)) 'g)
          
;map-from-to/los: [List-of Symbol] [List-of Symbol] [List-of Symbol] -> [List-of Symbols]
;replaces the given symbols in _los_, which should all be in _map-from_, with their corresponding
;symbols in _map-to_
(define (map-from-to/los los map-from map-to)
  (local [(define mapping-fn (λ (e) (map-from-to/symbol e map-from map-to)))]
    (map mapping-fn los)))

(check-expect (map-from-to/los '() '(a b c d) '(e f g h)) '())
(check-expect (map-from-to/los '(b a) '(a b c d) '(e f g h)) '(f e))
  


;rename: Graph [List-of Symbol] -> Graph
;renames the nodes in the graph to the names in the list of symbols
(define (rename gr lon)
  (local [(define nodes (graph-nodes gr))
          (define neigh (graph-neighbors gr))
          (define mapping-fn/symbol.reverse
            (λ (node-to-replace) (map-from-to/symbol node-to-replace lon nodes)))
          (define mapping-fn/los
            (λ (nodes-to-replace) (map-from-to/los nodes-to-replace nodes lon)))]
    (make-graph (mapping-fn/los nodes)
                (λ (n)
                  (mapping-fn/los (neigh (mapping-fn/symbol.reverse n)))))))

(check-satisfied (rename gr5 '(e f g h))
                 (make-graph=?
                  (make-graph '(e f g h)
                              (λ (n)
                                (cond
                                  [(symbol=? n 'e) '()]
                                  [(symbol=? n 'f) '()]
                                  [(symbol=? n 'g) '()]
                                  [(symbol=? n 'h) '()])))))
(check-satisfied (rename gr1 '(e f g h))
                 (make-graph=?
                  (make-graph '(e f g h)
                              (λ (n)
                                (cond
                                  [(symbol=? n 'e) '(f h)]
                                  [(symbol=? n 'f) '(g h)]
                                  [(symbol=? n 'g) '(f)]
                                  [(symbol=? n 'h) '()])))))

;Exercise 3

; node-name->numbers : Symbol -> (list Nat Nat)
; Convert a symbol of the form 'n1->n2 to (list n1 n2)
(define (node-name->numbers s)
  (map string->number (string-split (symbol->string s) "->")))
(check-expect (node-name->numbers '0->3) '(0 3))

; numbers->node-name : Nat Nat -> Symbol
; Convert a symbol of the form (list n1 n2) to 'n1->n2 
(define (numbers->node-name n1 n2)
  (string->symbol (string-append 
                   (number->string n1)
                   "->"
                   (number->string n2))))
(check-expect (numbers->node-name 0 3) '0->3)

;valid-node-con? Symbol Graph -> Boolean
;Is the given node connection in the graph?
(define (valid-node-con? node-con g)
  (local [(define nodes (graph-nodes g))]
    (neighbor-of? g (list-ref nodes (first (node-name->numbers node-con)))
                  (list-ref nodes (first (rest (node-name->numbers node-con)))))))
(check-expect (valid-node-con? '0->1 (make-graph '(a b) (λ (s)
                                                          (cond
                                                            [(symbol=? s 'a) (list 'b)]
                                                            [(symbol=? s 'b) '()])))) #t)
(check-expect (valid-node-con? '0->1 (make-graph '(a b) (λ (s)
                                                          (cond
                                                            [(symbol=? s 'a) '()]
                                                            [(symbol=? s 'b) '(a)])))) #f)

;; get-swapped-nodes : Graph -> [List-of Symbol]
;; produces the list of nodes in the swapped graph
(define (get-swapped-nodes graph)
  (local [(define nodes (graph-nodes graph))
          (define neighbors (graph-neighbors graph))
          ;flatten: {X} [List-of [List-of X]] -> [List-of X]
          ;flattens the given list; ie, the elements of all inner lists become the elements
          ;of one list
          (define (flatten l)
            (foldr append '() l))
          (define possible-connections
            (flatten (build-list
                      (length nodes)
                      (λ (s1)
                        (build-list
                         (length nodes)
                         (λ (s2)
                           (numbers->node-name s1 s2)))))))] 
    (filter (λ (nc) (valid-node-con? nc graph)) possible-connections)))

(check-expect (get-swapped-nodes (make-graph '(a b c)
                                             (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                          [(symbol=? n 'b) '(b)]
                                                          [(symbol=? n 'c) '(a)]))))
              (list '0->1 '0->2 '1->1 '2->0))
(check-expect (get-swapped-nodes (make-graph '(a b c)
                                             (λ (n) (cond [(symbol=? n 'a) '()]
                                                          [(symbol=? n 'b) '()]
                                                          [(symbol=? n 'c) '()]))))
              '())

;; get-swapped-neighbors : [List-of Symbol] -> [Symbol -> [List-of Symbol]]
;; produces the neighbors (function) of the swapped graph given the swapped nodes
(define (get-swapped-neighbors los)
  (λ (n1)
    (filter (λ (n2) (= (first (rest (node-name->numbers n1)))
                       (first (node-name->numbers n2)))) los)))
(check-expect ((get-swapped-neighbors '(0->2 2->1 2->3 1->0)) '1->2)
              (list '2->1 '2->3))
(check-expect ((get-swapped-neighbors '(0->2 2->1 2->3 1->0)) '2->3)
              '())
(check-expect ((get-swapped-neighbors '(0->2 2->1 2->3 1->0)) '2->1)
              '(1->0))


;; swap : Graph -> Graph
;; swaps the nodes of the graph with its edges
(define (swap gr)
  (local [(define swapped-nodes (get-swapped-nodes gr))]
    (make-graph swapped-nodes
                (get-swapped-neighbors swapped-nodes))))

(check-satisfied (swap (make-graph '(a b c)
                                   (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                [(symbol=? n 'b) '(b)]
                                                [(symbol=? n 'c) '(a)]))))
                 (make-graph=?
                  (make-graph '(0->1 0->2 1->1 2->0)
                              (λ (n) (cond [(symbol=? n '0->1) '(1->1)]
                                           [(symbol=? n '0->2) '(2->0)]
                                           [(symbol=? n '1->1) '(1->1)]
                                           [(symbol=? n '2->0) '(0->1 0->2)])))))

(check-satisfied (swap gr1)
                 (make-graph=?
                  (make-graph '(0->1 0->3 1->2 1->3 2->1)
                              (λ (n)
                                (cond
                                  [(symbol=? n '0->1) '(1->2 1->3)]
                                  [(symbol=? n '0->3) '()]
                                  [(symbol=? n '1->2) '(2->1)]
                                  [(symbol=? n '1->3) '()]
                                  [(symbol=? n '2->1) '(1->2 1->3)])))))

;Exercise 4
;; close? : Graph Symbol Symbol Natural -> Boolean
;; Is _n2_ within _nat_ steps from _n1_?
(define (close? gr n1 n2 nat)
  (local [;; close?.acc: Graph Symbol Symbol [List-of Symbol] -> [List-of [List-of Symbol]]
          ;; is there path in the graph g, from _from_ to _to_, of length fewer
          ;; than _max-length_?
          ;; ACCUMULATOR: visited contains the number of nodes we've already seen
          ;; TERMINATES: each call visits one symbol,
          ;; so visited will eventually grow larger than max-length
          (define (close?.acc g from to max-length visited)
            (local [(define neighbors (graph-neighbors g))]
              (cond
                [(> visited nat) #f]
                [(symbol=? from to) #t]
                [else (ormap (λ (c) (close?.acc g c to max-length (add1 visited)))
                             (neighbors from))])))]
    (close?.acc gr n1 n2 nat 0)))
(check-expect (close? gr1 'a 'c 2) #t)
(check-expect (close? gr1 'a 'c 1) #f)
(check-expect (close? gr1 'd 'c 50) #f)

;Exercise 5
;; find-all-paths : Graph Symbol Symbol -> [List-of [List-of Symbol]]
;; returns the list of all possible paths from _n1_ to _n2_
(define (find-all-paths gr n1 n2)
  (local [;find-all-paths.acc: Graph Symbol Symbol [List-of Symbol] -> [List-of [List-of Symbol]]
          ;returns all paths from _from_ to _to_ not passing through _visited_
          ;ACCUMULATOR: visited contains a list of the symbols already visited
          ;TERMINATES: each call visits one symbol, so visited will eventually contain all nodes
          (define (find-all-paths.acc g from to visited)
            (local [(define neighbors (graph-neighbors g))]
              (cond
                [(symbol=? from to) (list (reverse (cons to visited)))]
                [(e-in-set? from visited symbol=?) '()]
                [else (foldr append '()
                             (map (λ (n)
                                    (find-all-paths.acc g n to (cons from visited)))
                                  (neighbors from)))])))]
    (find-all-paths.acc gr n1 n2 '())))
(define G1
  (make-graph '(A B C D E F G)
              (λ (n)
                (cond [(symbol=? n 'A) '(B E)]
                      [(symbol=? n 'B) '(E F)]
                      [(symbol=? n 'C) '(D)]
                      [(symbol=? n 'D) '()]
                      [(symbol=? n 'E) '(C F A)]
                      [(symbol=? n 'F) '(D G)]
                      [(symbol=? n 'G) '()]))))
(check-expect (find-all-paths G1 'C 'C) '((C))) ; src = dest
(check-expect (find-all-paths G1 'C 'G) '()) ; no paths from 'C to 'G
(check-expect (find-all-paths G1 'A 'B) '((A B)))
(check-expect (find-all-paths G1 'E 'G) '((E F G)  (E A B F G)))
(check-expect (find-all-paths G1 'B 'G) '((B E F G)  (B F G)))
(check-expect (find-all-paths G1 'A 'G) '((A B E F G) (A B F G) (A E F G)))

;Exercise 6
;connected?: Graph -> Boolean
;can every node in a graph reach every other node in a graph?
(define (connected? g)
  (local [(define nodes (graph-nodes g))]
    (andmap (λ (n1)
              (andmap (λ (n2) (cons? (find-all-paths g n1 n2))) nodes)) nodes)))

(check-expect (connected? (make-graph '(a b c)
                                      (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                   [(symbol=? n 'b) '(b)]
                                                   [(symbol=? n 'c) '(a)])))) #f)
(check-expect (connected? (make-graph '(a b c)
                                      (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                   [(symbol=? n 'b) '(c)]
                                                   [(symbol=? n 'c) '(a)])))) #t)
;Exercise 7
;undirect? Graph -> Boolean
;Does each edge in the graph have a matching edge going the opposite direction?
(define (undirect? g)
  (graph=? (reverse-edges g) g))
(check-expect (undirect? (make-graph '(a b c d)
                                     (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                  [(symbol=? n 'b) '(a b)]
                                                  [(symbol=? n 'c) '(a)]
                                                  [(symbol=? n 'd) '()])))) #t)
(check-expect (undirect? (make-graph '(a b c)
                                     (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                  [(symbol=? n 'b) '(b)]
                                                  [(symbol=? n 'c) '(a)])))) #f)

;Exercise 8

;permutations: [List-of X] -> [List-of [List-of X]]
;returns a list of all possible permutations of the given list
(define (permutations l)
  (local [;add-to-end: [List-of X] X -> [List-of X]
          ;appends the given element to the end of the given list
          (define (add-to-end l e)
            (append l (list  e)))
          ;cons-to-all: X [List-of [List-of X]] -> [List-of [List-of X]]
          ;appends the given element to the front of each of the given inner lists
          (define (cons-to-all e ls)
            (map
             (λ (x) (cons e x)) ls))
          ;permutations.acc: [List-of X] [List-of X] -> [List-of [List-of X]]
          ;returns all permutations of the elements of prev-l and l combined
          ;where the first element in the permutation belongs to l
          ;ACCUMULATOR: prev-l keeps track of all the previous elements in the list 
          ;TERMINATES:
          ;each call, either (number of elements in prev-l + number of elements in l) reduces by one
          ;or one element is transferred from l to prev-l. l eventually becomes empty
          (define (permutations.acc prev-l l)
            (cond
              [(and (empty? prev-l) (empty? l)) (list '())]
              [(and (cons? prev-l) (empty? l)) '()]
              [(cons? l)
               (append
                (cons-to-all (first l) 
                             (permutations.acc '() (append prev-l (rest l))))
                (permutations.acc (add-to-end prev-l (first l)) (rest l)))]))]
    (permutations.acc '() l)))
(check-expect (permutations '()) (list '()))
(check-expect (permutations (list 'a)) (list (list 'a)))
(check-expect (permutations (list 1 2 3)) (list (list 1 2 3) (list 1 3 2)
                                                (list 2 1 3) (list 2 3 1)
                                                (list 3 1 2) (list 3 2 1)))
;graph-shape=? Graph Graph -> Boolean
;are the shapes of the two graphs equal?
(define (graph-shape=? g1 g2)
  (local [(define nodes1 (graph-nodes g1))
          (define nodes2 (graph-nodes g2))]
    (and
     (= (length nodes1) (length nodes2))
     (ormap
      (λ (p) (graph=? g1 (rename g2 p))) (permutations nodes1)))))
(check-expect (graph-shape=? (make-graph '(a b c)
                                         (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                      [(symbol=? n 'b) '(b)]
                                                      [(symbol=? n 'c) '(a)])))
                             (make-graph '(e b c)
                                         (λ (n) (cond [(symbol=? n 'e) '(b c)]
                                                      [(symbol=? n 'b) '(b)]
                                                      [(symbol=? n 'c) '(e)])))) #t)
(check-expect (graph-shape=? (make-graph '(a b c)
                                         (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                      [(symbol=? n 'b) '(b)]
                                                      [(symbol=? n 'c) '(a)])))
                             (make-graph '(b a c)
                                         (λ (n) (cond [(symbol=? n 'a) '(c b)]
                                                      [(symbol=? n 'b) '(b)]
                                                      [(symbol=? n 'c) '(a)])))) #t)
(check-expect (graph-shape=? (make-graph '(a b c)
                                         (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                      [(symbol=? n 'b) '(b)]
                                                      [(symbol=? n 'c) '(a)])))
                             (make-graph '(e f g)
                                         (λ (n) (cond [(symbol=? n 'e) '(f)]
                                                      [(symbol=? n 'f) '(g e)]
                                                      [(symbol=? n 'g) '(g)])))) #t)
(check-expect (graph-shape=? (make-graph '(a b c)
                                         (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                      [(symbol=? n 'b) '(b)]
                                                      [(symbol=? n 'c) '(a)])))
                             (make-graph '(a b c d)
                                         (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                      [(symbol=? n 'b) '(b)]
                                                      [(symbol=? n 'c) '(a)]
                                                      [(symbol=? n 'd) '()])))) #f)
(check-expect (graph-shape=? (make-graph '(a b c)
                                         (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                      [(symbol=? n 'b) '(b)]
                                                      [(symbol=? n 'c) '(a)])))
                             (make-graph '(a b c)
                                         (λ (n) (cond [(symbol=? n 'a) '(b)]
                                                      [(symbol=? n 'b) '(b a)]
                                                      [(symbol=? n 'c) '(c)])))) #f)
(check-expect (graph-shape=? (make-graph '(a b c)
                                         (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                                      [(symbol=? n 'b) '(b)]
                                                      [(symbol=? n 'c) '(a)])))
                             (make-graph '(e f g)
                                         (λ (n) (cond [(symbol=? n 'e) '(f e)]
                                                      [(symbol=? n 'f) '(g e)]
                                                      [(symbol=? n 'g) '(g)])))) #f)
