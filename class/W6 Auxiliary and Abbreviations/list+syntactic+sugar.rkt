;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname list+syntactic+sugar) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define A (cons 1 (cons 2 (cons 3 empty))))
(define B (list 1 2 3))
(define C '(1 2 3))


;;NOTE QUOTE STOPS EVALUATION
(define D (list (+ 1 2) 7 (* 89 3)))
(define E '((+ 1 2) 7 (* 89 3)))
(define HMM '(define (sqr x) (* x x)))
(define SEVEN 7)
(define F '(7 SEVEN 7))

;Quote turns into list of symbols?

(second (list 1 2 3))
;you get 2

(third (list 1 2 3))
;you get 3