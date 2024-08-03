;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |homework 5 gil|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An LoN (List of Numbers) is one of:
; - '()
; - (cons Number LoN)
#;(define (lon-temp l)
  (cond
    [(empty? l)...]
    [(cons? l) ...(first l) ...(lon-temp (rest l))..]))

;interleave: LoN LoN -> LoN
;takes two list of numbers and produces a list of their items,
;alternating from each list. If the lists have different lengths,
;it will finish with all the remaining items of the longer list.
(check-expect (interleave '() '()) '())
(check-expect (interleave (list 1 2 3) '()) (list 1 2 3))
(check-expect (interleave '() (list 1 2 3)) (list 1 2 3))
(check-expect (interleave (list 4) (list 1 2 3)) (list 4 1 2 3))
(check-expect (interleave (list 4 5 6) (list 1 2 3))
              (list 4 1 5 2 6 3))
(define (interleave l1 l2)
  (cond
   [(and (empty? l1) (empty? l2)) '()]
   [(and (empty? l1) (cons? l2)) l2]
   [(and (cons? l1) (empty? l2)) l1]
   [(and (cons? l1) (cons? l2))
    (cons (first l1)
                 (cons (first l2)
                       (interleave (rest l1) (rest l2))))]))






(define-struct options (first second))
(define-struct adjacent (first second))
(define (generate-all-sublists l)
  (cond
   [(empty? (rest l)) (append
               (first l)
               '())]
   [(cons? (rest l)) (cons
               (append (first l) (generate-all-sublists (rest l)))
               (generate-all-sublists (rest l)))]))




; A LoLoN (List of Numbers) is one of:
; - '()
; - (cons LoN LoLoN)
#;(define (lolon-temp l)
  (cond
    [(empty? l)...]
    [(cons? l) ...(lon-temp (first l)) ...(lolon-temp (rest l))...]))

; A Sublists is a non-empty LoLoN where no two LoN
; elements are equivalent. It represents all possible
; sublists of a given list
#;(define (sublists-temp l)
  (cond
    [(empty? (rest l)) ...]
    [(cons? (rest l)) ...(lon-temp (first l)) ...(sublists-temp (rest l))...]))
(define sublists0 (list '()))
(define sublists1 (list (list 0) '() ))
(define sublists2 (list (list 1 0) (list 1) (list 0) '()))
;(define sublists1 (list '() (list '() 0)))

; powerlist: LoN->Sublists
; returns a list of all possible sublists
; of a list of numbers
(check-expect (powerlist '()) sublists0)
(check-expect (powerlist (list 0)) sublists1)
(check-expect (powerlist (list 0 1)) sublists2)
(define (powerlist l)
  (generate-other-sublists (list '()) l))


; generate-other-sublists: Sublists LoN -> Sublists
; given the previously generated Sublists and the rest
; of the original list that has not yet been used to
; generate sublists, returns all sublists for the original
; list
(check-expect (generate-other-sublists (list '()) (list 0 1)) sublists2)
(check-expect (generate-other-sublists sublists1 (list 1)) sublists2)
(check-expect (generate-other-sublists sublists2 '()) sublists2)
(define (generate-other-sublists previous-sublists unused-list)
  (cond
   [(empty? unused-list) previous-sublists]
   [(cons? unused-list)
    (generate-other-sublists
     (double-sublists previous-sublists (first unused-list))
     (rest unused-list))]))


;double-sublists: Sublists Number -> Sublists
;For a given Sublists and an element
;returns a new Sublists. 
;The returned Sublists contains each previously
;created sublist as well as each previously
;created sublist with the given element appended to
;the front.
(check-expect (double-sublists sublists0 0) sublists1)
(check-expect (double-sublists sublists1 1) sublists2)
(define (double-sublists previous-sublists e)
  (append (sublists+element previous-sublists e) previous-sublists))




; sublists+element: Sublists Number -> Sublists
; Interp: given a Sublists and a
; number, returns a Sublists with the 
; number appended to the front of each sublist
(check-expect (sublists+element (list '()) 2) (list (list 2)))
(check-expect (sublists+element (list (list 1 3) (list 3)) 2)
              (list (list 2 1 3) (list 2 3)))
(define (sublists+element s e)
  (cond
    [(empty? (rest s)) (cons (cons e (first s))
                             '())]
    [(cons? (rest s)) (cons (cons e (first s))
                     (sublists+element (rest s) e))]))

#|(define (add-to-end l e)
  (cond
    [(empty? l) (cons e '())]
    [(cons? l) (cons (first l) (add-to-end (rest l)))]))|#