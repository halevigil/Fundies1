;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab 1 exercise 13|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define TOTAL-FLINT-COST 55000000)
;function flint-cost: num->num
; calculates percent of net worth needed to fix flint
(define (flint-cost wealth)
  (* (/ (TOTAL-FLINT-COST wealth) 100)))
(check-expect (flint-cost 5500000) 1000)
(check-expect (flint-cost 550000000) 10)