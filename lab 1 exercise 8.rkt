;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab 1 exercise 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;function multiple-of-five: num->boolean
;returns true if a number is a multiple of five
(define (multiple-of-five n)
  (cond [(equal? (modulo n 5) 0) #true]
        [(not (equal? (modulo n 5) 0)) #false]
        )
  )

(check-expect (multiple-of-five 5) #true)
(check-expect (multiple-of-five 6) #false)

(define GREETING "hi")
;function greet string->string
;prints out a greeting for a given name
(define (greet name)
  (string-append GREETING " " name))

(check-expect (greet "Gil") "hi Gil")
(check-expect (greet "David") "hi David")