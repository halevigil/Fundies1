;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rotation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct pair (first second))
;A [Pair-of X Y] is a (make-pair X Y)
; it represents two elements

;add-elements-to-corresponding-lists: [List-of X] [Pair-of [List-of [List-of X]] -> [List-of [List-of X]]]
;appends each element in li1 to its corresponding element of li2
(define (add-elements-to-corresponding-lists li1 li2)
  (cond
    [(empty? li1) li2]
    [(cons? li1) (cons (cons (first li1) (first li2))
                       (add-elements-to-corresponding-lists (rest li1) (rest li2)))]))
(check-expect (add-elements-to-corresponding-lists '() (list (list 4 5) (list 6 7)))
              (list (list 4 5) (list 6 7)))
(check-expect (add-elements-to-corresponding-lists (list 1 2) (list (list 4 5) (list 6 7)))
              (list (list 1 4 5) (list 2 6 7)))


(define (add-empties prev-list n)
  (cond
    [(= n 0) prev-list]
    [else (add-empties (cons '() prev-list) (sub1 n))]))
(define init (add-empties '() 4))

;rotate-cw: [Lo [Lo N]] -> Board
;rotates the given board clockwise 90*
(define (rotate-cw l)
  (local [;Add-empties: [List-of X] Natural -> [List-of X]
          ;prepends n empty lists to a list
          (define (add-empties prev-list n)
            (cond
              [(= n 0) prev-list]
              [else (add-empties (cons '() prev-list) (sub1 n))]))
          (define init (add-empties '() (length (first l))))]
    (foldl add-elements-to-corresponding-lists init l)))

(define (rotate-180 l)
  (rotate-cw (rotate-cw l)))
(define 

(rotate-cw (list (list 1 2 3) (list 4 5 6)))

