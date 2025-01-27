;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |homework 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 3

;; diagon-convert: NNN -> NNN
;; converts a given number of Pounds into Galleons
(define (diagon-convert Pounds)
  (/ (- Pounds 3) 5))

; A NNN is a non-negative number
; Examples: 1,3.4,0
(define NNN-1 1)
(define NNN-3.4 0)
(define NNN-0 0)
#;(define (NNN-temp NNN) ...NNN...)

(check-expect (diagon-convert 8) 1)
(check-expect (diagon-convert 13) 2)
(check-expect (diagon-convert 23) 4)
(check-expect (diagon-convert 4.5) 0.3)

;Exercise 4

(define (encode str)
  (string-append
   (string-upcase (substring str 0 2))
   " "
   (number->string (string-length str))))
(check-expect (encode "potter") "PO 6")
(check-expect (encode "Fun") "FU 3")
(check-expect (encode "Dog") "DO 3")

;Exercise 5

(define (leap-year? year)
  (cond
    [(not (equal? (remainder year 4) 0 ))  #false]
    [(not (equal? (remainder year 100) 0)) #true]
    [(not (equal? (remainder year 400) 0)) #false]
    [(equal? (remainder year 400) 0) #true]
    )
  )

(check-expect (leap-year? 2001) #false)
(check-expect (leap-year? 2004) #true)
(check-expect (leap-year? 2100) #false)
(check-expect (leap-year? 2000) #true)

;Exercise 6
(require 2htdp/image)

(require 2htdp/universe)

(define SCREEN-HEIGHT 250)
(define SCREEN-WIDTH 50)
(define BALL-RADIUS 25)
(define HALF-CYCLE (- SCREEN-HEIGHT (* BALL-RADIUS 2)))
(define FULL-CYCLE (* 2 HALF-CYCLE))

(define (make-image ball-height down?)
  (place-image
;   (overlay
    (circle BALL-RADIUS "solid" "red") 
    BALL-RADIUS (+ ball-height BALL-RADIUS)

    (rectangle SCREEN-WIDTH SCREEN-HEIGHT "solid"
               (cond
                 [down? "yellow"]
                 [(not down?) "orange"]))
;  )
  )
 )
(define (down-and-up t)
    (cond
      [(< (remainder t FULL-CYCLE) HALF-CYCLE) (make-image (remainder t HALF-CYCLE) #true)]
      [(>= (remainder t FULL-CYCLE) HALF-CYCLE) (make-image (- HALF-CYCLE (remainder t HALF-CYCLE)) #false)]))

; Uncomment below to run animate function
;(animate down-and-up)

; Exercise 7

; A CSG (CoinShuffleGame) is a (make-csg MaybeCoin MaybeCoin MaybeCoin)
(define-struct csg [left middle right])
; and represents the three cups in a coin shuffle game, and what is under them
(define csg-left-10 (make-csg 10 #false #false))
(define csg-middle-3-right-2 (make-csg #false 3 2))
(define csg-right-15.6 (make-csg #false #false 15.6))

#;(define (csg-temp csg)
  ...(csg-left csg)...
  ...(csg-middle csg)...
  ...(csg-right csg)...)
  


; A MaybeCoin is one of:
; - #false
; - number
; and represents either no coin or the coin's monetary value
(define MaybeCoin-false #false)
(define MaybeCoin-10 10)
(define MaybeCoin-15.6 15.6)

#;(define (MaybeCoin-temp mc)
  (cond
    [(boolean? mc) ...]
    [(number? mc) ...mc...]))
 
; A Guess is one of:
; - "left"
; - "middle"
; - "right

(define Guess-LEFT "left")
(define Guess-MIDDLE "middle")
(define Guess-RIGHT "right")

#;(define (Guess-temp g)
  (cond
    [(string=? g "left") ...]
    [(string=? g "middle") ...]
    [(string=? g "right") ...]
    )
  )
;;;;;;;;;;;;;;Look back to see if we need to expand on these ...s

;Exercise 8
;shuffle-right: CSG -> CSG
;shifts all values in CSG to the right, looping the rightmost value back to the left

(check-expect (shuffle-right csg-left-10) (make-csg #false 10 #false))
(check-expect (shuffle-right csg-middle-3-right-2) (make-csg 2 #false 3))
(check-expect (shuffle-right csg-right-15.6) (make-csg 15.6 #false #false))

(define (shuffle-right csg)
  (make-csg (csg-right csg) (csg-left csg) (csg-middle csg)))

          
;get-value: CSG Guess -> number
;takes a CSG and a Guess and outputs the monetary value of the coin in the guess' position
(check-expect (get-value csg-left-10 Guess-LEFT) 10)
(check-expect (get-value csg-middle-3-right-2 Guess-LEFT) 0)
(check-expect (get-value csg-right-15.6 Guess-RIGHT) 15.6)

(define (get-value csg guess)
  (cond
    [(boolean?
      (cond
       [(string=? guess "left") (csg-left csg)]
       [(string=? guess "middle") (csg-middle csg)]
       [(string=? guess "right") (csg-right csg)])) 0]
    [else
      (cond
       [(string=? guess "left") (csg-left csg)]
       [(string=? guess "middle") (csg-middle csg)]
       [(string=? guess "right") (csg-right csg)])
      ]
    )
  )
;;;;;;;;;;;;;;;;Can we use else? 

;Exercise 10
;inflation: NNN CSG -> CSG
;adds a given number to all coins in the CSG, leaving empty coins alone
(check-expect (inflation 5 csg-left-10) (make-csg 15 #false #false))
(check-expect (inflation 0 csg-middle-3-right-2) (make-csg #false 3 2))
(check-expect (inflation 3 csg-right-15.6) (make-csg #false #false 18.6))
(define (inflation delta-value csg)
  (make-csg
   (cond
     [(number? (csg-left csg)) (+ (csg-left csg) delta-value)]
     [else #false])
   (cond
     [(number? (csg-middle csg)) (+ (csg-middle csg) delta-value)]
     [else #false])
   (cond
     [(number? (csg-right csg)) (+ (csg-right csg) delta-value)]
     [else #false])
   ))
  
  