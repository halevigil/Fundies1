;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; sum-x-coords : [List-of Posn] -> Number
; Sum all the x-coordinates in the list of positions

#;(define (sum-x-coords lop)
(cond
[(empty? lop) 0]
[(cons? lop)
(+ (posn-x (first lop))
(sum-x-coords (rest lop)))]))
 
(check-expect (sum-x-coords empty) 0)
(check-expect (sum-x-coords
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 8)
 
; mult-distances : [List-of Posn] -> Number
; Multiply all the distances from each position to the origin
#;(define (mult-distances lop)
    (cond
      [(empty? lop) 1]
      [(cons? lop)
       (* (distance-to-origin (first lop))
          (mult-distances (rest lop)))]))
 
(check-expect (mult-distances empty) 1)
(check-expect (mult-distances
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 65)
 
; distance-to-origin : Posn -> Number
; Produces the distance from this position to the origin
(define (distance-to-origin p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))
 
(check-within (distance-to-origin (make-posn 2 2)) (sqrt 8) 1e-06)
(check-expect (distance-to-origin (make-posn 3 4)) 5)

;Exercise 4
; [X -> Z] [Z Y -> Y] Y [List-of X] -> Y
; Maps a function over a list and then combines its values using an operation
(define (map-foldr func op base l)
  (cond
    [(empty? l) base]
    [(cons? l)
     (op (func (first l))
         (map-foldr func op base (rest l)))]))
(check-expect (map-foldr add1 + 0 (list 1 2 3)) 9)
(check-expect (map-foldr add1 + 0 '()) 0)

(define (sum-x-coords lop)
  (map-foldr posn-x + 0 lop))

(define (mult-distances lop)
  (map-foldr distance-to-origin * 1 lop))

; strings-length : [List-of String] -> Number
; Calculates the total length of all strings in the list
(define (strings-length l)
  (map-foldr string-length + 0 l))
(check-expect (strings-length (list "a" "b" "c")) 3)
(check-expect (strings-length '()) 0)
(check-expect (strings-length (list "apple" "bear" "cheese")) 15)

; biggest-difference : [List-of Posn] -> Number
; Finds the biggest difference between the x and y values
; of any posn in the list
(define (biggest-difference l)
  (local [(define (total-diff posn)
            (abs (- (posn-x posn)
                    (posn-y posn))))]
    (map-foldr total-diff max 0 l)))

(check-expect (biggest-difference (list (make-posn 1 1))) 0)
(check-expect (biggest-difference (list (make-posn 1 1)
                                        (make-posn 2 3))) 1)
(check-expect (biggest-difference (list (make-posn 1 1)
                                        (make-posn 2 3)
                                        (make-posn 10 4)
                                        (make-posn 1 2))) 6)
(check-expect (biggest-difference '()) 0)

;Exercise 5

; A [NEL-of X] is a non-empty list of X
; It can be either
; - (cons X [NEL-of X])
; - (cons X '())
(define NEL-num (list 1 2 3))
(define NEL-str (list "hi" "bye" "go away"))
(define NEL-posn (list (make-posn 1 2) (make-posn 3 4) (make-posn 6 1)))
#;(define (NEL-temp NEL)
    (cond
      [(empty? (rest NEL)) ... (first NEL)]
      [(cons? (rest NEL)) ... (first NEL) ... (NEL-temp (rest NEL))]))

; earliest : [NEL-of String] [String String -> Boolean] : String
; Finds the string that is "earliest" in the list, based on the
; function given
(define (earliest l func)
  (local [(define (get-first s1 s2)
            (if (func s1 s2)
                s1
                s2))]
    (cond
      [(empty? (rest l)) (first l)]
      [(cons? (rest l)) (get-first (first l) (earliest (rest l) func))])))

; first-abc-string : [NEL-of String] -> String
; Finds the string that is first alphabetically in the list
(define (first-abc-string l)
  (earliest l string<?))

(check-expect (first-abc-string (list "a" "b" "c")) "a")
(check-expect (first-abc-string (list "c" "b" "a")) "a")
(check-expect (first-abc-string (list "z")) "z")

; last-abc-string : [NEL-of String] -> String
; Finds the string that is last alphabetically in the list
(define (last-abc-string l)
  (earliest l string>?))

(check-expect (last-abc-string (list "a" "b" "c")) "c")
(check-expect (last-abc-string (list "c" "b" "a")) "c")
(check-expect (last-abc-string (list "a")) "a")

; last-list-string : [NEL-of String] -> String
; Finds the string that is last in the list
(define (last-list-string l)
  (local [(define (last-string s1 s2) #f)]
    (earliest l last-string)))

(check-expect (last-list-string (list "a" "b" "c")) "c")
(check-expect (last-list-string (list "c" "b" "a")) "a")
(check-expect (last-list-string (list "a")) "a")

;Exercise 6
; duplicate-even-strings : [List-of String] -> [List-of String]
; Creates two copies of all even-length strings
(define (duplicate-even-strings l)
  (foldr duplicate-if-even '() l))

(check-expect (duplicate-even-strings (list "hi" "apple" "bye" "cake"))
              (list "hi" "hi" "apple" "bye" "cake" "cake"))
(check-expect (duplicate-even-strings (list "hi"))
              (list "hi" "hi"))
(check-expect (duplicate-even-strings (list "apple"))
              (list "apple"))
(check-expect (duplicate-even-strings '())
              '())

; duplicate-if-even : String [List-of String] -> [List-of String]
; Returns two copies of a string if its length is even, and one if it's odd,
; prepended to the given list
(define (duplicate-if-even s rest)
  (if
   (even? (string-length s))
   (cons s (cons s rest))
   (cons s rest)))

(check-expect (duplicate-if-even "hi" (list "bye")) (list "hi" "hi" "bye"))
(check-expect (duplicate-if-even "bye" (list "bye")) (list "bye" "bye"))
(check-expect (duplicate-if-even "hi" (list "hi")) (list "hi" "hi" "hi"))
(check-expect (duplicate-if-even "hi" '()) (list "hi" "hi"))

;Exercise 7

(define-struct cup [oz color material])
 
; A Cup is a (make-cup NonNegNumber String String)
; and represents a cup's capacity in fluid ounces, color, and material
 
(define CUP1 (make-cup 10 "brown" "wood"))
(define CUP2 (make-cup 8 "brown" "ceramic"))
(define CUP3 (make-cup 10 "red" "plastic"))
(define CUP4 (make-cup 6 "clear" "plastic"))
 
(define CUPS
  (cons CUP1
        (cons CUP2
              (cons CUP3
                    (cons CUP4 empty)))))


; A Group is a (make-group X [List-of Y])
; It represents a group of type Y, which all share some key X
(define-struct group (key items))
(define group1 (make-group 10 (list CUP1 CUP3)))
(define group2 (make-group "brown" (list CUP1 CUP2)))
#;(define (group-temp g)
    (... (group-key g) ...
         (list-temp (group-items g))))

; create-grouping : [List-of X] [X -> Y] [Y Y -> Boolean] -> [List-of Grouping]
(define (create-grouping l X-key X-equiv?)
  (cond
    [(empty? (rest l)) (add-to-grouplist '() (first l) X-key X-equiv?)]
    [(cons? (rest l)) (add-to-grouplist (create-grouping (rest l) X-key X-equiv?)
                                        (first l) X-key X-equiv?)]))
#;(check-expect (create-grouping CUPS cup-oz =) (list
                                                 (make-group 10 (list CUP1 CUP3))
                                                 (make-group 8 (list CUP2))
                                                 (make-group 6 (list CUP4))))

; add-to-grouplist : [List-of Group] X [X -> Y] [Y Y -> Boolean] -> [List-of Group]
(define (add-to-grouplist l x X-key X-equiv?)
  (cond
    [(empty? l) (cons (make-group (X-key x) (list x)) l)]
    [(cons? l) (if
                (X-equiv? (X-key x)
                          (group-key (first l))) ; helper later
                (cons
                 (make-group (group-key (first l))
                             (cons x
                                   (group-items (first l))))
                 (rest l))
                (cons
                 (first l)
                 (add-to-grouplist (rest l) x X-key X-equiv?)))]))

;(check-expect (add-to-grouplist '() CUP1 cup-oz =) (list (make-group 10 (list CUP1))))
#;(check-expect (add-to-grouplist (list (make-group 10 (list CUP1))) CUP3 cup-oz =)
                (list (make-group 10 (list CUP1 CUP3))))


;Exercise 8
; A [Keyed X] is a (list Number X)
;; and represents an X and its extracted "key" 

;; A Movie is a (make-movie String Number)
(define-struct movie [title runtime])
;; and represents a movie's title and runtime in minutes

;; sort-by-title-length : [List-of Movie] -> [List-of Movie]
;; Sort the movies by their title's length (ascending)
(define (sort-by-title-length lom)
  (sort-by-numberized lom (compose string-length movie-title)))
(check-expect (sort-by-title-length
               (list (make-movie "Sorry To Bother You" 111)
                     (make-movie "Hereditary" 127)
                     (make-movie "Annihilation" 120)
                     (make-movie "Blindspotting" 96)
                     (make-movie "You Were Never Really Here" 95)))
              (list
               (make-movie "Hereditary" 127)
               (make-movie "Annihilation" 120)
               (make-movie "Blindspotting" 96)
               (make-movie "Sorry To Bother You" 111)
               (make-movie "You Were Never Really Here" 95)))

;; An [NEList-of X] (Non-Empty List of X) is one of:
;; - (cons X '()))
;; - (cons X [NEList-of X])

;; sort-by-biggest : [List-of [NEList-of Number]] -> [List-of [NEList-of Number]]
;; Sort the lists by their biggest element (ascending)
(define (sort-by-biggest lonelon)
  (local [;; biggest : [NEList-of Number] -> Number
          ;; Find the biggest number in the non-empty list of numbers
          (define (biggest nelon)
            (foldr max (first nelon) (rest nelon)))]
    (sort-by-numberized lonelon biggest)))
(check-expect (sort-by-biggest (list (list 6) (list 1 2 3) (list 5 6) (list 23)))
              (list (list 1 2 3) (list 6) (list 5 6) (list 23)))

; sort-by-numberized : [List-of X] [X -> Number] -> [List-of X]
; Sorts the list by the value of each element when converted
; to a number through the function
(define (sort-by-numberized l numberizer)
  (local [;; insert-by-X : [Keyed X] [List-of [Keyed X]] -> [List-of [Keyed X]]
          ;; Find the correct spot for the keys
          (define (insert-by-X kx lokx)
            (cond [(empty? lokx) (list kx)]
                  [(cons? lokx)
                   (if (<= (first kx) (first (first lokx)))
                       (cons kx lokx)
                       (cons (first lokx) (insert-by-X kx (rest lokx))))]))]
    (map second
         (foldr insert-by-X
                '()
                (map list (map numberizer l) l)))))

;(define (identity x) x)
(check-expect (sort-by-numberized (list "12" "3" "4" "6" "0")
                                  string->number)
              (list "0" "3" "4" "6" "12"))