;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 1
(define-struct node (key info left right))
;A [BST X Y] is one of:
;-'leaf
;-(make-node X Y [BST X Y] [BST X Y])
;where a (make-node k v l r) represents a bst node
;with key k, value v, left branch l and right branch r 
(define bst0 'leaf)
(define bst1 (make-node 2 "hi" 'leaf 'leaf))
(define bst2 (make-node "one" 0
                        (make-node "hi" 3 'leaf 'leaf)
                        (make-node "yo" 4 'leaf
                                   (make-node "brr" 9 'leaf 'leaf))))



;A [Comparison X] is a [X X -> Real],
;where a negative output indicates the first parameter comes
;before the second, a positive output indicates the first parameter
;comes after the second, and 0 indicates they are the same.
;Examples: -, string-order-comp,string-length-comp
(define string-order-comp
  (λ (s1 s2)
    (cond
      [(string>? s1 s2) 1]
      [(string=? s1 s2) 0]
      [(string<? s1 s2) -1])))
(define string-length-comp
  (λ (s1 s2)
    (- (string-length s1) (string-length s2))))
;;;;do I have to check expect these example functions???


(define-struct treemap (comparison bst))
;A [TreeMap X Y] is a (make-treemap [Comparison X] [BST X Y])
;Interp: a (make-treemap c b) represents a tree b where any two keys within it
;can be compared using the function c to determine which one should come first
(define tm0 (make-treemap - 'leaf))
(define tm1 (make-treemap - bst1))
(define tm2 (make-treemap string-order-comp bst2))
(define tm3 (make-treemap string-length-comp bst2))





;Exercise 8
; A Bool is a [X X -> X]
(define BOOLT (λ (x1 x2) x1))
(define BOOLF (λ (x1 x2) x2))

;bool->boolean: Bool->Boolean
;returns the boolean value that corresponds to the given BOOL
(define (bool->boolean b)
  (b #t #f))
(check-expect (bool->boolean BOOLT) #t)
(check-expect (bool->boolean BOOLF) #f)

;and/bool: Bool Bool -> Bool
;outputs BOOLT if both Bools are BOOLT, BOOLF otherwise
(define (and/bool b1 b2)
  (b1 b2 BOOLF))
    
(check-expect (bool->boolean (and/bool BOOLT BOOLT)) #t)
(check-expect (bool->boolean (and/bool BOOLT BOOLF)) #f)
(check-expect (bool->boolean (and/bool BOOLF BOOLT)) #f)
(check-expect (bool->boolean (and/bool BOOLF BOOLF)) #f)

;or/bool: Bool Bool -> Bool
;outputs BOOLT if at least one of the two Bools are BOOLT, BOOLF otherwise
(define (or/bool b1 b2)
  (b1 b1 b2))
(check-expect (bool->boolean (or/bool BOOLT BOOLT)) #t)
(check-expect (bool->boolean (or/bool BOOLT BOOLF)) #t)
(check-expect (bool->boolean (or/bool BOOLF BOOLT)) #t)
(check-expect (bool->boolean (or/bool BOOLF BOOLF)) #f)

;or/bool: Bool -> Bool
;outputs BOOLT if the input is BOOLF, BOOLF otherwise
(define (not/bool b)
  (b BOOLF BOOLT))
(check-expect (bool->boolean (not/bool BOOLT)) #f)
(check-expect (bool->boolean (not/bool BOOLF)) #t)


  

