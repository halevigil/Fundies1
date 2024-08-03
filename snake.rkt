;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (chop-tail (list (make-posn 2 2) (make-posn 2 3))) (list (make-posn 2 2)))

(define (chop-tail ls)
  (cond
    [(empty? (rest ls)) '()]
    [(cons? (rest ls)) (cons (first ls) (chop-tail ls))]))


     