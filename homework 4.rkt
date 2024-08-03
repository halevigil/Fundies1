;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |homework 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 2
; A LoI is one of:
; '()
; (cons Integer LoI)
; it represents a list of integers
(define loi0 '())
(define loi1 (cons 1 loi0))
(define loi2 (cons 0 loi1))
(define loi3 (cons 1 loi2))
(define loi4 (cons 2 loi3))

#;(define (loi-temp loi)
    (cond
      [(empty? loi) ...]
      [(cons? loi) ...(first loi)...(loi-temp (rest loi))]))

; not-present?: LoI Integer -> Boolean
; returns false if the integer is present
; in the LoI, true otherwise
(check-expect (not-present? loi0 0) #t)
(check-expect (not-present? loi1 0) #t)
(check-expect (not-present? loi2 0) #f)
(check-expect (not-present? loi3 0) #f)

(define (not-present? loi i)
  (cond
    [(empty? loi) #t]
    [(cons? loi) (not (or
                       (equal? i (first loi))
                       (not (not-present? (rest loi) i))))]))

;Exercise 3
;all-unique? LoI -> Boolean
; returns true if every member of the LoI
; is unique in the LoI, false otherwise
(check-expect (all-unique? loi0) #t)
(check-expect (all-unique? loi1) #t)
(check-expect (all-unique? loi3) #f)
(define (all-unique? loi)
  (cond
    [(empty? loi) #t]
    [(cons? loi) (and
                  (not-present? (rest loi) (first loi))
                  (all-unique? (rest loi))
                  )]))


;Exercise 4
; has-even-count?: LoI Integer -> Boolean
; returns true if the integer occurs an even
; number of times in the LoI; false otherwise
(check-expect (has-even-count? loi0 1) #t)
(check-expect (has-even-count? loi1 1) #f)
(check-expect (has-even-count? loi3 1) #t)
(check-expect (has-even-count? loi4 1) #t)

(define (has-even-count? loi i)
  (cond
    [(empty? loi) #t]
    [(cons? loi)
     (if (equal? (first loi) i)
         (not (has-even-count? (rest loi) i))
         (has-even-count? (rest loi) i))]))

;Exercise 5
;To create all-even-count?, we would have to
;build a list of the unique integers in LoI
;and then run has-even-count? on the full list
;with each integer. We could not use the same
;approach from all-uniquer?
;We could not simply call has-even-count?
;recursively on subsets of the list because
;any non-empty list with integers of all even counts
;is guaranteed to have sub-lists without this
;property, thus all-even-count? would erroneously
;return false

;Exercise 6
; A Coordinate is an integer in range [0,99]
; it represents the x or y coordinates of an
; intersection
(define coord0 0)
(define coord5 5)
(define coord99 99)
#;(define (coord-temp c)
    ...c...)


; An Isec is a (make-posn Coordinate Coordinate)
; A #f represents an intersection that does not exist
; a (make-posn east south)
; the coordinates of an intersection, where the origin
; is the north-west
(define isec-0-0 (make-posn 0 0))
(define isec-5-10 (make-posn 5 10))
(define isec-99-99 (make-posn 99 99))
#;(define (maybeisec-temp mi)
    ...(posn-x mi)... (posn-y mi)...)


; A MaybeIsec is one of:
; #f
; Isec
; A #f represents an intersection that does not exist
; an isec is an isec
(define maybeisec-f #f)
(define maybeisec-0-0 (make-posn 0 0))
(define maybeisec-5-10 (make-posn 5 10))
(define maybeisec-99-99 (make-posn 99 99))
#;(define (maybeisec-temp mi)
    (cond
      [(boolean? mi) ...]
      [(posn? mi) (isec-temp mi)]))

(define-struct neighbors (n e s w))
; A Neighbors is a
;(make-neighbors MaybeIsec MaybeIsec MaybeIsec MaybeIsec)
; a (make-neighbors n e s w)
; represents the north, east, south and west neighbors
; of an intersection
(define neighbors0
  (make-neighbors #f (make-posn 1 0) (make-posn 0 1) #f))
(define neighbors1
  (make-neighbors (make-posn 99 98) #f #f (make-posn 98 99)))
(define neighbors2
  (make-neighbors (make-posn 5 4) (make-posn 6 5)
                  (make-posn 5 6) (make-posn 4 5)))
#;(define (neighbors-temp n)
    ...(maybeisec-temp(neighbors-n n))
    ...(maybeisec-temp(neighbors-e n))
    ...(maybeisec-temp(neighbors-s n))
    ...(maybeisec-temp(neighbors-w n))...)

;posn->misec: (make-posn Integer Integer)->MaybeIsec
;returs false if the given posn is out of bounds
;(x and y are not in range (0,99))
;returns the input otherwise
(check-expect (posn->misec (make-posn -1 5)) #f)
(check-expect (posn->misec (make-posn 5 -1)) #f)
(check-expect (posn->misec (make-posn 100 5)) #f)
(check-expect (posn->misec (make-posn 5 100)) #f)
(check-expect (posn->misec (make-posn 5 5)) (make-posn 5 5))
(define (posn->misec posn)
  (cond
    [(< (posn-x posn) 0) #f]
    [(< (posn-y posn) 0) #f]
    [(> (posn-x posn) 99) #f]
    [(> (posn-y posn) 99) #f]
    [else posn]))

;get-neighbors: Isec->Neighbors
;given one of these intersections outputs
;its four neighbors, directly to the east, west,
; north, and south.
(check-expect (get-neighbors isec-0-0)
              (make-neighbors #false (make-posn 1 0)
                              (make-posn 0 1) #false))
(check-expect (get-neighbors isec-5-10)
              (make-neighbors (make-posn 5 9)
                              (make-posn 6 10)
                              (make-posn 5 11)
                              (make-posn 4 10)))
(check-expect (get-neighbors isec-99-99)
              (make-neighbors (make-posn 99 98)
                              #false #false
                              (make-posn 98 99)))

(define (get-neighbors isec)
  (make-neighbors
   (posn->misec (make-posn (posn-x isec) (sub1 (posn-y isec))))
   (posn->misec (make-posn (add1 (posn-x isec)) (posn-y isec)))
   (posn->misec (make-posn (posn-x isec) (add1 (posn-y isec))))
   (posn->misec (make-posn (sub1 (posn-x isec)) (posn-y isec)))))




;Exercise 7
; A Cost is one of:
; - (make-unit Number)
; - (make-lb Number)
(define-struct unit [cost])
(define-struct lb [cost])
; and represents either the cost per unit or per lb of an item

(define cost-1 (make-unit 10))
(define cost-2 (make-lb 4))
(define cost-3 (make-unit 0))

#;(define (cost-temp c)
    (cond
      [(unit? c) ... (unit-cost c)]
      [(lb? c) ... (lb-cost c)]))

; A CE (CatalogueEntry) is a
; (make-ce String Cost)
(define-struct ce [name cost])
; and represents the name of a food and how much it costs

(define CE-1 (make-ce "apple" cost-1))
(define CE-2 (make-ce "corn flakes" cost-2))
(define CE-3 (make-ce "pizza" cost-3))
(define CE-4 (make-ce "ice cream" cost-1))
(define CE-5 (make-ce "cake" cost-2))

#;(define (CE-temp ce)
    (... (ce-name ce) ... (ce-cost ce)))
 
; A GC (GroceryCatalogue) is one of:
; - '()
; - (cons CE GC)
; where each catalogue entry has a unique name

(define GC-empty empty)
(define GC-1 (cons CE-1 GC-empty))
(define GC-2 (cons CE-2 GC-1))
(define GC-3 (cons CE-3 GC-2))
(define GC-4 (cons CE-4 GC-3))
(define GC-5 (cons CE-5 GC-4))

#;(define (GC-temp GC)
    (cond
      [(empty? GC) ...]
      [(cons? GC) ... (CE-temp (first GC)) ... (GC-temp (rest GC))]))


; A Order is one of:
; - String
; - (make-weight String Number)
(define-struct weight [name lb])
; and represents either one unit of food or its name and weight in lbs

(define order-1 "ice cream")
(define order-2 (make-weight "cake" 30))
(define order-stupid "cake")
(define order-stupider (make-weight "ice cream" 10))

#;(define (order-temp o)
    (cond
      [(string? o) ...]
      [(weight? o) ... (weight-name o) ... (weight-lb o)]))

; A Checkout is one of:
; - '()
; - (cons Order Checkout)

(define checkout-0 empty)
(define checkout-1 (cons order-1 checkout-0))
(define checkout-2 (cons order-2 checkout-1))
(define checkout-big (make-list 10 order-1))
(define checkout-bigger (make-list 11 order-1))
(define checkout-stupid (cons order-stupid checkout-2))
(define checkout-stupider (cons order-stupider checkout-2))

#;(define (checkout-temp c)
    (cond
      [(empty? c) ...]
      [(cons? c) ... (order-temp (first c)) ... (checkout-temp (rest c))]))
; WHY IS THIS COMPLETELY BACKWARDS

;Exercise 8
; get-cost : GC, String -> Cost
; Gets the Cost of a given food name in a GC
; Returns an error if none found
(check-error (get-cost GC-empty "pizza") "Given food not found in the GC")
(check-expect (get-cost GC-1 "apple") cost-1)
(check-expect (get-cost GC-2 "corn flakes") cost-2)
(check-expect (get-cost GC-3 "corn flakes") cost-2)
(define (get-cost GC food)
  (cond
    [(empty? GC) (error "Given food not found in the GC")]
    [(cons? GC) (if (string=? food (ce-name (first GC)))
                    (ce-cost (first GC))
                    (get-cost (rest GC) food))]))

;Exercise 9
; set-cost : GC, String, Cost -> GC
; Returns the catalogue with that food’s Cost now set to the given one
(check-error (set-cost GC-empty "ice cream" cost-1)
             "Given food not found in the GC")

(check-expect (set-cost GC-2 "apple" cost-2)
              (cons (make-ce "apple" (make-lb 4)) '()))

(check-expect (set-cost GC-3 "pizza" cost-1)
              (cons
               (make-ce "pizza" (make-unit 10))
               (cons
                (make-ce "corn flakes" (make-lb 4))
                (cons (make-ce "apple" (make-unit 10)) '()))))

(define (set-cost GC food cost)
  (cond
    [(empty? GC) (error "Given food not found in the GC")]
    [(cons? GC) (if (string=? food (ce-name (first GC)))
                    (cons (make-ce food cost) (rest GC))
                    (set-cost (rest GC) food cost))]))


;Exercise 10
; average-unit-cost : GC -> Number
; Returns the average unit cost of all unit-priced items
; in the given GC.
(check-error (average-unit-cost GC-empty)
             "No unit-priced items found")
(check-expect (average-unit-cost GC-1) 10)
(check-expect (average-unit-cost GC-2) 10)
(check-expect (average-unit-cost GC-3) 5)

(define (average-unit-cost GC)
  (average-costs (total-unit-cost GC) (num-units GC)))

; average-costs : Number, Number -> Number
; Returns the average cost from the sum and number of items
; Throws an error if there are 0 items
(check-error (average-costs 0 0)
             "No unit-priced items found")
(check-expect (average-costs 10 1) 10)
(check-expect (average-costs 10 2) 5)
(define (average-costs sum count)
  (if (= count 0)
      (error "No unit-priced items found")
      (/ sum count)))

; total-unit-cost : GC -> Number
; Returns the total unit cost of all unit-priced items
; in the given GC.
(check-expect (total-unit-cost GC-empty) 0)
(check-expect (total-unit-cost GC-1) 10)
(check-expect (total-unit-cost GC-2) 10)
(check-expect (total-unit-cost GC-3) 10)

(define (total-unit-cost GC)
  (cond
    [(empty? GC) 0]
    [(cons? GC)
     (+ (CE->unit-cost (first GC))
        (total-unit-cost (rest GC)))]))

; CE->unit-cost : CE -> Number
; Returns the unit cost of the given CE
; Returns zero if it's a per-pound cost
(check-expect (CE->unit-cost CE-1) 10)
(check-expect (CE->unit-cost CE-2) 0)
(check-expect (CE->unit-cost CE-3) 0)
(define (CE->unit-cost ce)
  (if (unit? (ce-cost ce))
      (unit-cost (ce-cost ce))
      0))

; num-units : GC -> Number
; Returns the total number of unit-priced items
; in the given GC.
(check-expect (num-units GC-empty) 0)
(check-expect (num-units GC-1) 1)
(check-expect (num-units GC-2) 1)
(check-expect (num-units GC-3) 2)

(define (num-units GC)
  (cond
    [(empty? GC) 0]
    [(cons? GC)
     (+
      (if (unit? (ce-cost (first GC)))
          1
          0)
      (num-units (rest GC)))]))

;Exercise 11
; express-lane? : Checkout -> Boolean
; Returns true if a Checkout has at most 10 items,
; false otherwise
(check-expect (express-lane? checkout-0) #t)
(check-expect (express-lane? checkout-1) #t)
(check-expect (express-lane? checkout-2) #t)
(check-expect (express-lane? checkout-big) #t)
(check-expect (express-lane? checkout-bigger) #f)
(define (express-lane? c)
  (<= (checkout-items c) 10))

; checkout-items : Checkout -> Number
; Returns the num of items in a Checkout
(check-expect (checkout-items checkout-0) 0)
(check-expect (checkout-items checkout-1) 1)
(check-expect (checkout-items checkout-2) 2)
(check-expect (checkout-items checkout-big) 10)
(check-expect (checkout-items checkout-bigger) 11)
(define (checkout-items c)
  (cond
    [(empty? c) 0]
    [(cons? c) (add1 (checkout-items (rest c)))]))

;Exercise 12
; total-cost : GC, Checkout -> Number
; Given a Checkout and a GC, produces the total cost.
; If a food has been weighed that is priced by unit or if
; a food that is priced by pound has not been weighed,
; returns an error.

(check-expect (total-cost GC-5 checkout-0) 0)
(check-expect (total-cost GC-5 checkout-1) 10)
(check-expect (total-cost GC-5 checkout-2) 130)
(check-expect (total-cost GC-5 checkout-big) 100)
(check-error (total-cost GC-2 checkout-big)
             "Given food not found in the GC")
(check-error (total-cost GC-5 checkout-stupid)
             "Given per-pound cost and unit order")
(check-error (total-cost GC-5 checkout-stupider)
             "Given unit cost and per-pound order")

(define (total-cost GC c)
  (cond
    [(empty? c) 0]
    [(cons? c) (+
                (order-cost (first c)
                            (get-cost GC
                                      (order-name (first c))))
                (total-cost GC (rest c)))]))

; order-cost : Order, Cost -> Number
; Gets the cost of an order from a given Order
; and Cost.
(check-expect (order-cost order-1 cost-1) 10)
(check-expect (order-cost order-2 cost-2) 120)
(check-error (order-cost order-1 cost-2)
             "Given per-pound cost and unit order")
(check-error (order-cost order-2 cost-1)
             "Given unit cost and per-pound order")

(define (order-cost o c)
  (cond
    [(and (unit? c) (weight? o))
     (error "Given unit cost and per-pound order")]
    [(and (lb? c) (string? o))
     (error "Given per-pound cost and unit order")]
    [(string? o) (unit-cost c)]
    [(weight? o) (* (weight-lb o) (lb-cost c))]))

; order-name : Order -> String
; Gets the name of th food onf an order
(check-expect (order-name order-1) "ice cream")
(check-expect (order-name order-2) "cake")
(define (order-name o)
  (cond
    [(string? o) o]
    [(weight? o) (weight-name o)]))