;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |hw6 final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
; Maps a function over a list and then combines its values using an operation.
; The operation is done from right to left on the list, starting from a given
;base value.
(define (map-foldr func op base l)
  (cond
    [(empty? l) base]
    [(cons? l)
     (op (func (first l))
         (map-foldr func op base (rest l)))]))
(check-expect (map-foldr add1 + 0 (list 1 2 3)) 9)
(check-expect (map-foldr add1 + 0 '()) 0)

;see top for signature
(define (sum-x-coords lop)
  (map-foldr posn-x + 0 lop))

;see top for signature
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
  (local [;diff: Posn-> NonNegNumber
          ;returns the absolute difference between the x and y coordinates
          (define (diff posn)
            (abs (- (posn-x posn)
                    (posn-y posn))))]
    (map-foldr diff max 0 l)))

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
(define NEL-num (list 1))
(define NEL-str (list "hi" "bye" "go away"))
(define NEL-posn (list (make-posn 1 2) (make-posn 3 4) (make-posn 6 1)))
#;(define (NEL-temp NEL)
    (cond
      [(empty? (rest NEL)) ... (first NEL)]
      [(cons? (rest NEL)) ... (first NEL) ... (NEL-temp (rest NEL))]))

; earliest : [NEL-of String] [String String -> Boolean] : String
; Finds the string that is "earliest" in the list, where the
; "earlier" of any two strings is determined by the given function
(define (earliest l func)
  (local [;get-first: String String -> String
          ; returns the "earlier" of the two strings, determined by the function
          ; passed to earliest
          (define (get-first s1 s2)
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
  (local [;last-string: String String -> Boolean
          ;always returns false
          (define (last-string s1 s2) #f)]
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
; and represents a cup, with capacity in fluid ounces, color, and material
 
(define CUP1 (make-cup 10 "brown" "wood"))
(define CUP2 (make-cup 8 "brown" "ceramic"))
(define CUP3 (make-cup 10 "red" "plastic"))
(define CUP4 (make-cup 6 "clear" "plastic"))
 
(define CUPS
  (cons CUP1
        (cons CUP2
              (cons CUP3
                    (cons CUP4 empty)))))


; A [Group-of X Y] is a (make-group X [List-of Y])
; A (make-group k l) represents a group with key k and list of items l
(define-struct group (key items))
(define group1 (make-group 10 (list CUP1 CUP3)))
(define group2 (make-group "brown" (list CUP1 CUP2)))
(define group3 (make-group 8 (list CUP2)))
#;(define (group-temp g)
    (... (group-key g) ...
         (list-temp (group-items g))))

; A [Grouping-of X Y] is a [List-of [Group-of X Y]],
; where every group in the list has key type X element type Y
; It represents a grouping of elements by a certain property
(define grouping0 '())
(define grouping1 (list group1))
(define grouping2 (list group1 group3))
(define grouping3 (list group2))
#;(define (grouping-temp g)
    (... (list-temp g)...))

; belongs-in-group?: [Group-of Y X] X [X -> Y] [Y Y -> Boolean] -> Boolean
; Does the given item's key match the given group's key?
(define (belongs-in-group? group item key-extractor key-equiv?)
  (key-equiv? (key-extractor item) (group-key group)))
(check-expect (belongs-in-group? group1 CUP1 cup-oz =) #t)
(check-expect (belongs-in-group? group1 CUP2 cup-oz =) #f)
(check-expect (belongs-in-group? group2 CUP2 cup-color string=?) #t)

; add-to group: [Group-of Y X] X -> [Group-of Y X]
; returns the given group with the given item prepended
(define (add-to-group group item)
  (make-group (group-key group) (cons item (group-items group))))
(check-expect (add-to-group (make-group 10 (list CUP3)) CUP1) (make-group 10 (list CUP1 CUP3)))
(check-expect (add-to-group (make-group "brown" '()) CUP2) (make-group "brown" (list CUP2)))
         
; add-to-grouping : [Grouping-of Y X] X [X -> Y] [Y Y -> Boolean] -> [Grouping-of Y X]
; returns the given grouping with the given item either prepended to the correct group
; or added to a new group (if no existing group matches its key)
(define (add-to-grouping grouping item key-extractor key-equiv?)
  (cond
    [(empty? grouping) (cons (make-group (key-extractor item) (list item)) grouping)]
    [(cons? grouping)
     (if (belongs-in-group? (first grouping) item key-extractor key-equiv?)
         (cons (add-to-group (first grouping) item) (rest grouping))
         (cons (first grouping)
               (add-to-grouping (rest grouping) item key-extractor key-equiv?)))]))

(check-expect (add-to-grouping '() CUP1 cup-oz =) (list (make-group 10 (list CUP1))))
(check-expect (add-to-grouping (list (make-group 8 (list CUP2)) (make-group 10 (list CUP1)))
                               CUP3 cup-oz =)
              (list (make-group 8 (list CUP2)) (make-group 10 (list CUP3 CUP1))))
(check-expect (add-to-grouping (list (make-group 10 (list CUP1)) (make-group 8 (list CUP2)))
                               CUP3 cup-oz =)
              (list (make-group 10 (list CUP3 CUP1)) (make-group 8 (list CUP2))))
(check-expect (add-to-grouping (list (make-group 8 (list CUP2))) CUP3 cup-oz =)
              (list (make-group 8 (list CUP2)) (make-group 10 (list CUP3))))
(check-expect (add-to-grouping (list (make-group "brown" (list CUP1))) CUP2 cup-color string=?)
              (list (make-group "brown" (list CUP2 CUP1))))


; create-grouping : [List-of X] [X -> Y] [Y Y -> Boolean] -> [Grouping-of Y X]
; it creates a group based on the given list of elements and the keys extracted
; by the key extractor
(define (create-grouping l key-extractor key-equiv?)
  (cond
    [(empty? (rest l)) (add-to-grouping '() (first l) key-extractor key-equiv?)]
    [(cons? (rest l)) (add-to-grouping (create-grouping (rest l) key-extractor key-equiv?)
                                       (first l) key-extractor key-equiv?)]))
(check-expect (create-grouping CUPS cup-oz =)
              (list
               (make-group 6 (list CUP4))
               (make-group 10 (list CUP1 CUP3))
               (make-group 8 (list CUP2))))
(check-expect (create-grouping CUPS cup-color string=?)
              (list
               (make-group "clear" (list CUP4))
               (make-group "red" (list CUP3))
               (make-group "brown" (list CUP1 CUP2))
               ))


;Exercise 8
; A [Keyed X] is a (list Number X)
;; and represents an X and its extracted "key"

; sort-by-numberized : [List-of X] [X -> Number] -> [List-of X]
; Sorts the list by the "numberized" value of each element.
; The "numberized" value of each element is given by the inputted function
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
(check-expect (sort-by-numberized (list "12" "3" "4" "6" "0")
                                  string->number)
              (list "0" "3" "4" "6" "12"))

;; A Movie is a (make-movie String Number)
(define-struct movie [title runtime])
;; and represents a movie's title and runtime in minutes
(define movie1 (make-movie "Sorry To Bother You" 111))
(define movie2 (make-movie "Hereditary" 127))
(define movie3 (make-movie "Annihilation" 120))
(define movie4 (make-movie "Blindspotting" 96))
(define movie5 (make-movie "You Were Never Really Here" 95))

;; sort-by-title-length : [List-of Movie] -> [List-of Movie]
;; Sort the movies by their title's length (ascending)
(define (sort-by-title-length lom)
  (sort-by-numberized lom (compose string-length movie-title)))
(check-expect (sort-by-title-length '()) '())
(check-expect (sort-by-title-length
               (list movie1 movie2 movie3 movie4 movie5))
              (list movie2 movie3 movie4 movie1 movie5))

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