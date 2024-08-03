;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |homework 2 final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 3

;; diagon-convert: NNN -> NNN
;; converts a given number of Pounds into Galleons
(define (diagon-convert Pounds)
  (/ (- Pounds 3) 5))

; A NNN is a non-negative number
; Examples: 1,3.4,0
(define NNN-1 1)
(define NNN-3.4 3.4)
(define NNN-0 0)
#;(define (NNN-temp NNN) ...NNN...)

; A NNI is a non-negative integer
; Examples: 1,3,0
(define NNI-1 1)
(define NNI-3 3)
(define NNI-0 0)
#;(define (NNI-temp NNI) ...NNI...)

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
    [(< (remainder t FULL-CYCLE) HALF-CYCLE)
     (make-image (remainder t HALF-CYCLE) #true)]
    [(>= (remainder t FULL-CYCLE) HALF-CYCLE)
     (make-image (- HALF-CYCLE (remainder t HALF-CYCLE)) #false)]))

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
    ...(MaybeCoin-temp (csg-left csg))...
    ...(MaybeCoin-temp (csg-middle csg))...
    ...(MaybeCoin-temp (csg-right csg))...
    )
  


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
    [(boolean? (cond
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

;Exercise 11
(define-struct UFO (x y))
; A UFO is a (make-UFO (x y))
; It represents a UFO at some position
; x and y are NNIs representing the position of the UFO
; Examples
(define UFO-origin (make-UFO 0 0))
(define UFO-high (make-UFO 5 100))
(define UFO-random (make-UFO 23 71))
(define (UFO-temp UFO)
  ( ... (UFO-x UFO) ... (UFO-y UFO) ...))

; An X-Direction is a string enumeration equal to either "left" or "right"
; It represents the direction something is moving along the x-axis
; Examples
(define x-direction-left "left")
(define x-direction-right "right")
(define (x-direction-temp x-direction)
  ( ... (cond
          [(string=? x-direction "left") ... ]
          [(string=? x-direction "right") ... ])))

(define-struct cow (x dir))
; A cow is a (make-cow (x dir))
; It represents a cow at some position, moving in some direction left or right
; x is an integer representing the position, and dir is the X-direction the cow is moving
; Examples
(define cow-origin (make-cow 0 "left"))
(define cow-far (make-cow 21 "right"))
(define cow-near (make-cow 6 "left"))
(define (cow-temp cow)
  ( ... (x-direction-temp (cow-direction cow))...
        ... (cow-x cow)...))
        


;Exercise 12
(define UFO-DESCENT-SPEED 1)
; UFO-down : UFO -> UFO
; Moves a UFO down by some fixed amount
(define (UFO-down UFO)
  (make-UFO (UFO-x UFO) (- (UFO-y UFO) UFO-DESCENT-SPEED)))

(check-expect (UFO-down UFO-high) (make-UFO 5 99))


;Exercise 13
(define UFO-HORIZ-SPEED 3)
; UFO-side : UFO, X-Direction -> UFO
; Moves a UFO left or right by some fixed amount
(define (UFO-side UFO dir)
  (cond
    [(string=? dir "left") (make-UFO (- (UFO-x UFO) UFO-HORIZ-SPEED) (UFO-y UFO))]
    [(string=? dir "right") (make-UFO (+ (UFO-x UFO) UFO-HORIZ-SPEED) (UFO-y UFO))]))

(check-expect (UFO-side UFO-high "left") (make-UFO 2 100))
(check-expect (UFO-side UFO-random "right") (make-UFO 26 71))

;Exercise 14
(define COW-HORIZ-SPEED 2)
; cow-side : cow -> cow
; Moves a cow left or right, dependent on its direction, some fixed amount
(check-expect (cow-side cow-near) (make-cow 4 "left"))
(check-expect (cow-side cow-far) (make-cow 23 "right"))

(define (cow-side cow)
  (cond
    [(string=? (cow-dir cow) "left") (make-cow (- (cow-x cow) COW-HORIZ-SPEED) (cow-dir cow))]
    [(string=? (cow-dir cow) "right") (make-cow (+ (cow-x cow) COW-HORIZ-SPEED) (cow-dir cow))]))

;Exercise 15
(define UFO-SCREEN-WIDTH 200)
; cow-at-edge? : cow -> bool
; Determines if a cow is at the edge of the screen
(check-expect (cow-at-edge? (make-cow 0 "left")) #t)
(check-expect (cow-at-edge? (make-cow 10 "right")) #f)
(check-expect (cow-at-edge? (make-cow UFO-SCREEN-WIDTH "right")) #t)

(define (cow-at-edge? cow)
  (cond
    [(equal? (cow-x cow) 0) #t]
    [(equal? (cow-x cow) UFO-SCREEN-WIDTH) #t]
    [else #f]))

;Exercise 16
; flip-cow : cow -> cow
; Flips the direction a cow is facing
(check-expect (flip-cow cow-origin) (make-cow 0 "right"))
(check-expect (flip-cow cow-far) (make-cow 21 "left"))

(define (flip-cow cow)
  (cond
    [(string=? (cow-dir cow) "left") (make-cow (cow-x cow) "right") ]
    [(string=? (cow-dir cow) "right") (make-cow (cow-x cow) "left") ]))

;Exercise 17
; move-cow : cow -> cow
; Moves a cow in the direction it is facing, UNLESS it is at the edge, in which case
; it will turn around and then move
(check-expect (move-cow cow-origin) (make-cow 2 "right"))
(check-expect (move-cow cow-far) (make-cow 23 "right"))

(define (move-cow cow)
  (cond
    [(cow-at-edge? cow) (cow-side (flip-cow cow))]
    [(not (cow-at-edge? cow)) (cow-side cow)]))

;Exercise 18
; ufo-at-cow? UFO, cow -> bool
; Determines if a UFO is at the same position as (has captured) a cow
(check-expect (ufo-at-cow?  (make-UFO 0 0) (make-cow 0 "left")) #t)
(check-expect (ufo-at-cow?  (make-UFO 0 10) (make-cow 0 "left")) #f)
(check-expect (ufo-at-cow?  (make-UFO 0 0) (make-cow 5 "left")) #f)

(define (ufo-at-cow? UFO cow)
  (cond
    [(not (equal? (cow-x cow) (UFO-x UFO))) #f]
    [(not (equal? 0 (UFO-y UFO))) #f]
    [else #t]))

;Exercise 19
; ufo-crashed? UFO, cow -> bool
; Determines if a UFO is at the same position as (has captured) a cow
(check-expect (ufo-crashed? (make-UFO 0 0) (make-cow 0 "left")) #f)
(check-expect (ufo-crashed? (make-UFO 0 10) (make-cow 0 "left")) #f)
(check-expect (ufo-crashed? (make-UFO 0 0) (make-cow 5 "left")) #t)

(define (ufo-crashed? UFO cow)
  (cond
    [(not (equal? 0 (UFO-y UFO))) #f]
    [(ufo-at-cow? UFO cow) #f]
    [else #t]))

;Exercise 20
; game-over? : UFO, cow -> Game-End-State
; Determines if the game is over, and if so, whether the UFO won
(check-expect (game-over? (make-UFO 0 0) (make-cow 0 "left")) game-end-state-win)
(check-expect (game-over? (make-UFO 1 0) (make-cow 0 "left")) game-end-state-lose)
(check-expect (game-over? (make-UFO 0 10) (make-cow 0 "left")) game-end-state-false)

(define (game-over? UFO cow)
  (cond
    [(ufo-crashed? UFO cow) game-end-state-lose]
    [(ufo-at-cow? UFO cow) game-end-state-win]
    [else game-end-state-false]))

; A Game-End-State is a string that is one of "UFO Wins" or "UFO Loses", or a #f
; It represents the state of the game's end, or the game not being over if false
(define game-end-state-win "UFO Wins")
(define game-end-state-lose "UFO Loses")
(define game-end-state-false #f)

#;(define (game-end-state-temp game-end-state)
    ... (cond
          [(boolean? game-end-state) ... ]
          [(string=? game-end-state game-end-state-win) ... ]
          [(string=? game-end-state game-end-state-lose) ... ]))