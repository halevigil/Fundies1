;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |finger exercise hw6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A GC (GroceryCatalogue) is one of:
; - '()
; - (cons CE GC)
; where each catalogue entry has a unique name

(define-struct food [name cost])
;update-gc: GC String [Cost -> Cost] -> GC
; applies the given function to the previously
; associated cost of the food with the given name and leaves the rest
; of the catalogue untouched
(define (update-gc gc food-name convert-cost)
  (cond
    [(empty? gc) '()]
    [(and (cons? gc) (string=? (food-name (first gc)) food-name))
     (cons (make-food "hot dog" (convert-cost (food-cost (first gc)))) (rest gc))]
    [(cons? gc) (update-gc (rest gc) food-name convert-cost)]))

; A House is a (make-house String Number Number String Number)
(define-struct house [address bedrooms bathrooms style value])

; change-price: House [Number -> Number] -> House
; applies the given function to the given house's cost,
; and returns the resulting house
(define (change-price house func)
  (make-house (house-address house) (house-bedrooms house)
              (house-bathrooms house) (house-style house)
              (func (house-value house))))

(check-expect (change-price (make-house "ad" 3 4 "sty" 23) add1) (make-house "ad" 3 4 "sty" 24))
(check-expect (change-price (make-house "ad" 3 4 "sty" 0) add1) (make-house "ad" 3 4 "sty" 1))

; weird-change/house: House -> House
(define (weird-change house)
  (if (>= (house-bathrooms house) 2.5)
      (make-house (house-address house) (house-bedrooms house)
                  (house-bathrooms house) (house-style house)
                  (* (house-value house) 1.1))
  (if (<= (house-bedrooms house) 2)
      (make-house (house-address house) (house-bedrooms house)
                  (house-bathrooms house) (house-style house)
                  (* (house-value house) 0.9))
  (if (string=? (house-style house) "Colonial")
      (make-house (house-address house) (house-bedrooms house)
                  (house-bathrooms house) (house-style house)
                  (+ (house-value house) 500))
      house))))
(check-expect (weird-change (make-house "ad" 3 4 "sty" 23))
              (make-house "ad" 3 4 "sty" (* 1.1 23)))
(check-expect (weird-change (make-house "ad" 1 2 "sty" 23))
              (make-house "ad" 1 2 "sty" (* 0.9 23)))
(check-expect (weird-change (make-house "ad" 3 2 "Colonial" 23))
              (make-house "ad" 3 2 "Colonial" (+ 500 23)))

      