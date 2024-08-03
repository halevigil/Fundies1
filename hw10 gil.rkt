;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw10 gil|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


;e-in-set?: [Set-of X] X -> Boolean
;is the given element in the given set?
(define (e-in-set? s los equiv?)
  (ormap (λ (x) (equiv? s x)) los))

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

;set=?: [Set-of X] [Set-of X] [X X -> Boolean] -> Boolean
;are the two given sets equal?
(define (set=? s1 s2 equiv?)
  (and (= (length s1) (length s2))
       (andmap (λ (e) (e-in-set? e s2 equiv?)) s1)))
(check-expect (set=? (list 0 1) (list 0 1 2) =) #f)
(check-expect (set=? (list 0 4 1) (list 0 1 2) =) #f)
(check-expect (set=? (list 0 4 1) (list 0 1 4) =) #t)

;both-neighbors: Graph Symbol Symbol -> [Set-of Symbol]
;returns the set containing all neighbors of both functions
(define (both-neighbors graph node1 node2)
  (local [(define neighbors-fn (graph-neighbors graph))]
    (union (neighbors-fn node1) (neighbors-fn node2) symbol=?)))
(check-expect (set=? (both-neighbors gr1 'a 'b) (list 'b 'c 'd) symbol=?) #t)
(check-expect (set=? (both-neighbors gr1 'a 'c) (list 'b 'd) symbol=?) #t)

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



