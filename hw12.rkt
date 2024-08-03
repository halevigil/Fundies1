;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;e-in-list?: {X} X [List-of X] [X X -> Boolean] -> Boolean
;is the given element in the given list?
(define (e-in-list? e l equiv?)
  (ormap (λ (x) (equiv? e x)) l))
(check-expect (e-in-list? 'a (list 'c 'a 'b) symbol=?) #t)
(check-expect (e-in-list? 1 (list 0 2 4) =) #f)


;es-in-list?: {X} [List-of X] [List-of X] [X X -> Boolean] -> Boolean
;are all the given elements _es_ in the given list _l_? Order doesn't matter.
(define (es-in-list? es l equiv?)
  (cond
    [(empty? es) #t]
    [(cons? es) (and (e-in-list? (first es) l equiv?)
                     (es-in-list? (rest es) (remove-first (first es) l equiv?) equiv?))]))
(check-expect (es-in-list? (list 'c 'a 'a) (list 'a 'c 'b 'a) symbol=?) #t)
(check-expect (es-in-list? (list 1 4) (list 0 2 4) =) #f)
(check-expect (es-in-list? (list 4 4) (list 0 2 4) =) #f)

;remove-first: X [List-of X] [X X -> Boolean] -> [List-of X]
;removes the first instance of the given element in the given list, if
;the element is in the list. Else returns the given list.
(define (remove-first e l equiv?)
  (cond
    [(empty? l) l]
    [(cons? l)
     (if (equiv? e (first l))
         (rest l)
         (cons (first l)
               (remove-first e (rest l) equiv?)))]))
(check-expect (remove-first 1 (list 2 1 1 3) =) (list 2 1 3))
(check-expect (remove-first 'a (list 'b 'c) symbol=?) (list 'b 'c))
     
;list-: [List-of X] [List-of X] [X X -> Boolean] -> [List-of X]
;removes the first instance of each element in _l2_ from _l1_
(define (list- l1 l2 equiv?)
  (foldr (λ (x prev) (remove-first x prev equiv?)) l1 l2))
(check-expect (list- (list 1 2 3) '() =) (list 1 2 3))
(check-expect (list- (list 1 2 3) (list 4 3 5 2 2 1) =) '())
(check-expect (list- (list 4 3 5 2 2 1) (list 1 2 3) =) (list 4 5 2))

;A Payment is one of:
;- 'creditcard
;- DollarBill
;it represents a payment made by a customer
(define p1 'creditcard)
(define p2 5)
(define p3 10)
(define p4 20)
#;(define (payment-temp p)
    (cond
      [(symbol? p) ...]
      [(number? p) ... (db-temp p)...]))

;A DollarBill is one of:
;- 5
;- 10
;- 20
;it represents a 5,10 or 20 dollar bill
(define d1 5)
(define d2 10)
(define d3 20)
#;(define (db-temp db)
    (cond
      [(= d1 5) ...]
      [(= d1 10) ...]
      [(= d1 20) ...]))
    

;change-after-sale: Payment [List-of DollarBill] -> [Maybe [List-of DollarBill]]
;returns the bills left after the sale of 5 dollars given bills _lod_ left in the register, or 
;returns #false if the given payments cannot make a sale of 5 dollars
(define (change-after-sale p lod)
  (local [;change-after-sale/bill: DollarBill [List-of DollarBill] -> [Maybe [List-of DollarBill]]
          ;returns the bills left after the sale of 5 dollars given bills _lod_ left in the
          ;register, or returns #false if the given payments cannot make a sale of 5 dollars
          (define (change-after-sale/bill d lod)
            (cond
              [(= d 5) lod]
              [(and (= d 10) (e-in-list? 5 lod =))
               (cons 10 (remove-first 5 lod =))]
              [(and (= d 20) (es-in-list? (list 10 5) lod =))
               (cons 20
                     (list- lod (list 10 5) =))]
              [(and (= d 20) (es-in-list? (list 5 5 5) lod =))
               (cons 20
                     (list- lod (list 5 5 5) =))]
              [else #f]))]
    (cond
      [(symbol? p) lod]
      [(number? p) (change-after-sale/bill p lod)])))
(check-expect (change-after-sale 'creditcard (list 5 5)) (list 5 5))
(check-expect (change-after-sale 5 '()) '())
(check-expect (change-after-sale 10 (list 5)) (list 10))
(check-expect (change-after-sale 10 (list 10 20)) #f)
(check-expect (change-after-sale 20 (list 10 5 20)) (list 20 20))
(check-expect (change-after-sale 20 (list 5 5 5)) (list 20))
(check-expect (change-after-sale 20 (list 10 20)) #f)
(check-expect (change-after-sale 20 (list 5 5 20)) #f)

;sellable-tickets: [List-of Payment] Natural -> Natural
;returns the number of tickets that can be sold to the given _lop_ payments
;starting from _n_ 5 dollar bills
(define (sellable-tickets lop n)
  (local [;sellable-tickets: [List-of Payment] Natural -> Natural
          ;returns the number of tickets that can be sold to the given _lop_ payments
          ;with _bills_ left in the register
          ;ACCUMULATOR: bills represents the number of bills left in the register
          (define (sellable-tickets.acc lop bills)
            (cond
              [(empty? lop) 0]
              [(and (cons? lop) (boolean? (change-after-sale (first lop) bills)))
               (sellable-tickets.acc (rest lop) bills)]
              [(and (cons? lop) (list? (change-after-sale (first lop) bills)))
               (add1 (sellable-tickets.acc (rest lop) (change-after-sale (first lop) bills)))]))]
    (sellable-tickets.acc lop (make-list n 5))))
(check-expect (sellable-tickets '() 2) 0)
(check-expect (sellable-tickets (list 10) 2) 1)
(check-expect (sellable-tickets (list 'creditcard 5 5) 0) 3)
(check-expect (sellable-tickets (list 10 10 10) 2) 2)
(check-expect (sellable-tickets (list 20 10) 3) 1)
(check-expect (sellable-tickets (list 10 20 10 10) 4) 4)
(check-expect (sellable-tickets (list 5 10 5) 0) 2)
(check-expect (sellable-tickets (list 'creditcard 20 10 5 20 10 5) 5) 6)

;Exercise 2

;{X} A [NEL-of X] is one of:
;(cons X '())
;(cons X [NEL-of X])
;it represents a non-empty list of a given datatype
(define nel0 (list 0))
(define nel1 (list 'a 'b 't))
#;(define (nel-temp nel)
    (cond
      [(empty? (rest nel)) ...(first nel)...]
      [(cons? (rest nel)) ...(first nel)...(nel-temp (rest nel))...]))

;nth-smallest: Natural [NEL-of Number] -> Number
;returns the nth-smallest element in a list of numbers
;ASSUMES: n is less than the length of the given list
(define (nth-smallest n nel)
  (local [;smaller-than-pivot: [NEL-of Number] [Number] -> [List-of Number]
          ;returns a list of all the numbers in the original list less than or equal to _pivot_
          (define (smaller-than-pivot nel pivot)
            (local [(define nel-sans-pivot (remove-first pivot nel =))]
              (filter (λ (x) (<= x pivot)) nel-sans-pivot)))
          ;bigger-than-pivot: [NEL-of Number] [Number] -> [List-of Number]
          ;returns a list of all the numbers in the original list greater than _pivot_
          (define (bigger-than-pivot nel pivot)
            (filter (λ (x) (> x pivot)) nel))]
    (cond
      [(empty? (rest nel)) (first nel)]
      [(cons? (rest nel)) 
       (local [(define pivot (get-pivot nel))
               (define smaller (smaller-than-pivot nel pivot))
               (define bigger (bigger-than-pivot nel pivot))]
         (nth-smallest/pivot-smaller-bigger n pivot smaller bigger))])))
(check-expect (nth-smallest 0 (list 2)) 2)
(check-expect (nth-smallest 1 (list 5 3 7 1)) 3)
(define index-list (build-list 33 (λ (x) x)))
(check-expect (andmap
               (λ (n) (= (nth-smallest n index-list) n)) index-list) #t)


;nth-smallest/pivot-smaller-bigger: Number Number [List-of Number] [List-of Number] -> Number
;gets the _n_th smallest value out of the two lists and pivot
;ASSUMES: all elements in smaller are smaller than pivot, all elements in bigger are larger
;than pivot, and n is smaller than the length of pivot, smaller and bigger combined
(define (nth-smallest/pivot-smaller-bigger n pivot smaller bigger)
  (local [(define n_smaller (length smaller))]
    (cond
      [(> n_smaller n) (nth-smallest n smaller)]
      [(< n_smaller n) (nth-smallest (- n (+ n_smaller 1)) bigger)]
      [(= n_smaller n) pivot])))
(check-expect (nth-smallest/pivot-smaller-bigger 3 5 (list 3 1) (list 8 9)) 8)
(check-expect (nth-smallest/pivot-smaller-bigger 1 5 '() (list 8 9)) 8)
(check-expect (nth-smallest/pivot-smaller-bigger 0 5 (list 3 1) (list 8 9)) 1)
(check-expect (nth-smallest/pivot-smaller-bigger 1 5 (list 3 1) '()) 3)
(check-expect (nth-smallest/pivot-smaller-bigger 2 5 (list 3 1) (list 8 9)) 5)
(check-expect (nth-smallest/pivot-smaller-bigger 0 5 '() '()) 5)


;get-pivot: [NEL-of Number] -> Number
;returns the pivot element for the nth-smallest algorithm
(define (get-pivot nel)
  (local [(define medians (map median (size-5-sublists nel)))
          (define length/2 (quotient (length medians) 2))]
    (nth-smallest length/2 medians)))
(check-expect (get-pivot (list 1)) 1)
(check-expect (get-pivot (list 1 2 3 4)) 3)
(check-expect (get-pivot (list 1 2 3 4 5)) 3)
(check-expect (get-pivot (build-list 10 (λ (n) n))) 7)
(check-expect (get-pivot (build-list 15 (λ (n) n))) 7)
(check-expect (get-pivot (build-list 16 (λ (n) n))) 12)





;size-5-sublists: [NEL-of X] -> [NEL-of [NEL-of X]]
;splits the given list into sublists of length 5 (with one possibly shorter list at the end)
(define (size-5-sublists l)
  (local [(define (first5 l)
            (list
             (first l)
             (list-ref l 1)
             (list-ref l 2)
             (list-ref l 3)
             (list-ref l 4)))
          (define (rest5 l)
            (rest (rest (rest (rest (rest l))))))]
    (if (<= (length l) 5)
        (list l)
        (cons (first5 l)
              (size-5-sublists (rest5 l))))))
(check-expect (size-5-sublists (list 1 2)) (list (list 1 2)))
(check-expect (size-5-sublists (list 1 2 3 4 5)) (list (list 1 2 3 4 5)))
(check-expect (size-5-sublists (list 1 2 3 4 5 6)) (list (list 1 2 3 4 5) (list 6)))



;median: [NEL-of Number] -> Number
;returns the median element in the given list.
;If the length of the list is even, returns the larger of the two middle values
(define (median l)
  (local [(define sorted-list (quicksort l <))]
    (list-ref sorted-list (quotient (length sorted-list) 2))))
(check-expect (median (list 0)) 0)
(check-expect (median (list 1 2 3)) 2)
(check-expect (median (list 1 2 3 4)) 3)


;Exercise 3

; A Circle is a [CircleMessage -> Any]
 
; A CircleMessage is one of:
; - 'center
; - 'radius
; - 'resize
; - 'equal
; and represents a message to circle, requesting either:
; its center (a Posn)
; its radius (a Number)
; how much to addtively change its radius by (a [Number -> Circle])
; whether or not it has the same size and position as another circle (a [Circle -> Boolean])

;posn=?: Posn Posn -> Boolean
;are the two given Posns equal?
(define (posn=? p1 p2)
  (and
   (= (posn-x p1) (posn-x p2))
   (= (posn-y p1) (posn-y p2))))
(check-expect (posn=? (make-posn 2.5 -3) (make-posn 2.5 -3)) #t)
(check-expect (posn=? (make-posn 2.5 -3) (make-posn 2 -3)) #f)
(check-expect (posn=? (make-posn 2.5 -3) (make-posn 2.5 -1)) #f)


;new-circle: Posn Number -> Circle
;produces a Circle with center _posn_ and radius _rad_
(define (new-circle c rad)
  (λ (msg)
    (cond
      [(symbol=? msg 'center) c]
      [(symbol=? msg 'radius) rad]
      [(symbol=? msg 'resize) (λ (d_rad) (new-circle c (+ rad d_rad)))]
      [(symbol=? msg 'equal) (λ (circ) (and
                                        (posn=? (circ 'center) c)
                                        (= (circ 'radius) rad)))])))
(define c0 (new-circle (make-posn 10 20) 4))
(define c1 (new-circle (make-posn 10 20) 9))
(check-expect (c0 'radius) 4)
(check-expect (c0 'center) (make-posn 10 20))
(check-expect (((c0 'resize) 10) 'radius) 14)
(check-expect ((c1 'equal) c0) #f)
(check-expect ((((c1 'resize) -5) 'equal) c0) #t)
    
  
  
  
  
